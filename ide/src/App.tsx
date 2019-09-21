import React from 'react';
import './App.css';
import { Interaction } from './Interaction';
import { Check, TestResult } from './Check';
import { DefChunks } from './DefChunks';
import { SingleCodeMirrorDefinitions } from './SingleCodeMirrorDefinitions';
import { Menu, Tab } from './Menu';
import { Footer } from './Footer';
import { FontSize } from './FontSize';
import { FSBrowser } from './FSBrowser';
import { Dropdown, DropdownOption } from './Dropdown';
import { Header } from './Header';
import { InteractionError } from './InteractionError';
import * as control from './control';
import SplitterLayout from 'react-splitter-layout';
import 'react-splitter-layout/lib/index.css';

control.installFileSystem();
control.loadBuiltins();

enum EEditor {
    Chunks,
    Text,
}

type AppProps = {};
type AppState = {};

function makeResult(result: any, compiledJSONPath: string): { name: string, value: any }[] {
    const programJSON = JSON.parse(
        control.bfsSetup.fs.readFileSync(compiledJSONPath));

    const providedValues = programJSON.provides.values;
    const providedValuesKeys = Object.keys(programJSON.provides.values);

    const insertLineNumber = (key: string) => {
        const startLine =
            providedValues[key].origin["local-bind-site"][1];
        return { line: startLine, name: key, value: result[key] };
    };

    type Result = {
        line: number,
        name: string,
        value: any,
    };

    const compareResults = (a: Result, b: Result): number => {
        if (a.line < b.line) {
            return -1;
        } else if (a.line > b.line) {
            return 1;
        } else {
            return 0;
        }
    };

    const compareLocations = (a: any, b: any): number => {
        if (a.srcloc.startLine < b.srcloc.startLine) {
            return -1;
        } else if (a.srcloc.startLine > b.srcloc.startLine) {
            return 1;
        } else {
            return 0;
        }
    };

    if (providedValuesKeys.length !== 0) {
        // we have source location information for bindings, so we sort them
        // based on which column they are bound on
        return providedValuesKeys
            .map(insertLineNumber)
            .sort(compareResults);
    } else {
        var sortedLoc = result.$locations.sort(compareLocations);
        // we do not have source location information for bindings, so we sort
        // them alphabetically by identifier name
        return sortedLoc.map((key: any) => {
            return {
                name: key.name,
                value: result[key.name]
            }
        });
    }
}

type LintFailure = {
    name: string,
    errors: string[]
}

type EditorProps = {
    browseRoot: string;
    browsePath: string[];
    currentFileDirectory: string[];
    currentFileName: string;
};

enum CompileState {
    // Starting state for the application. We are waiting for the webworker to
    // give us confirmation that it has finished its setup phase and is ready
    // to receive compilation requests.
    //
    // Startup -> StartupQueue
    //   The user edits the definitions area or clicks "run".
    //
    // Startup -> Ready
    //   The webworker finishes its setup.
    Startup,

    // We are waiting for the webworker to give us confirmation that it has
    // finished its setup phase so that we can satisfy a queued compilation
    // request.
    //
    // StartupQueue -> StartupQueue
    //   The user edits the definitions area or clicks "run".
    //
    // StartupQueue -> Compile
    //   The webworker finishes its setup
    StartupQueue,

    // We are able to immediately satisfy any compilation requests.
    //
    // Ready -> Compile
    //   The user edits the definitions area or clicks "run".
    Ready,

    // We are compiling the program. Any compilation request generated during
    // this state will queue it for later.
    //
    // Compile -> CompileQueue
    //   The user edits the definitions area or clicks "run".
    //
    // Compile -> Ready
    //   Compilation succeeded. The program is run.
    //
    // Compile -> Ready
    //   Compilation failed.
    Compile,

    // We have received a compilation request during a compilation.
    //
    // CompileQueue -> CompileQueue
    //   The user edits the definitions area or clicks "run".
    //
    // CompileQueue -> Compile
    //   Compilation either succeeded or failed. Either way, the program is not
    //   run.
    CompileQueue,
}

const invalidCompileState = (state: CompileState): void => {
    throw new Error(`illegal CompileState reached: ${state}`);
};

const compileStateToString = (state: CompileState): string => {
    // TODO(michael): these could be more pirate-themed
    if (state === CompileState.Startup) {
        return "Finishing setup";
    } else if (state === CompileState.StartupQueue) {
        return "Compile request on hold: finishing setup";
    } else if (state === CompileState.Ready) {
        return "Ready";
    } else if (state === CompileState.Compile) {
        return "Compiling";
    } else if (state === CompileState.CompileQueue) {
        return "Compile request on hold: already compiling";
    } else {
        const assertNever = (_arg: never): never => {
            throw new Error("assertNever");
        };

        return assertNever(state);
    }
};

type EditorState = {
    browseRoot: string;
    browsePath: string[];
    currentFileDirectory: string[];
    currentFileName: string;
    currentFileContents: string;
    typeCheck: boolean;
    checks: Check[],
    interactions: { name: string, value: any }[];
    interactionErrors: string[];
    lintFailures: {[name : string]: LintFailure};
    runKind: control.backend.RunKind;
    autoRun: boolean;
    updateTimer: NodeJS.Timer;
    dropdownVisible: boolean;
    fontSize: number;
    editorMode: EEditor,
    message: string;
    definitionsHighlights: number[][];
    fsBrowserVisible: boolean;
    compileState: CompileState;
};

class Editor extends React.Component<EditorProps, EditorState> {
    constructor(props: EditorProps) {
        super(props);

        const onLintFailure = (lintFailure : { name: string, errors: string[]}) => {
            let newFailures = this.state.lintFailures;
            const name = lintFailure.name;
            newFailures[name] = lintFailure;
            this.setState({ lintFailures: newFailures });
        }
        const onLintSuccess = (lintSuccess : { name: string}) => {
            let newFailures = this.state.lintFailures;
            const name = lintSuccess.name;
            if(name in newFailures) { delete newFailures[name]; }
            this.setState({ lintFailures: newFailures });
        }

        control.setupWorkerMessageHandler(
            console.log,
            () => {
                console.log("setup finished");

                if (this.state.compileState === CompileState.Startup) {
                    this.setState({compileState: CompileState.Ready});
                } else if (this.state.compileState === CompileState.StartupQueue) {
                    this.setState({compileState: CompileState.Ready});
                    this.update();
                } else {
                    invalidCompileState(this.state.compileState);
                }
            },
            (errors: string[]) => {
                console.log("COMPILE FAILURE");
                if (this.state.compileState === CompileState.Compile) {
                    this.setState({compileState: CompileState.Ready});

                    const places: any = [];
                    for (let i = 0; i < errors.length; i++) {
                        const matches = errors[i].match(/:\d+:\d+-\d+:\d+/g);
                        if (matches !== null) {
                            matches.forEach((m) => {
                                places.push(m.match(/\d+/g)!.map(Number));
                            });
                        }
                    }
                    this.setState(
                        {
                            interactionErrors: errors,
                            definitionsHighlights: places
                        }
                    );
                } else if (this.state.compileState === CompileState.CompileQueue) {
                    this.setState({compileState: CompileState.Ready});
                    this.update();
                } else {
                    invalidCompileState(this.state.compileState);
                }
            },
            (errors: string[]) => {
                this.setState(
                    {
                        interactionErrors: [errors.toString()],
                    }
                );
            },
            onLintFailure,
            onLintSuccess,
            () => {
                console.log("COMPILE SUCCESS");
                if (this.state.compileState === CompileState.Compile) {
                    this.setState({compileState: CompileState.Ready});

                    const x = new Date();
                    console.log(`Run ${x} started`);
                    control.run(
                        control.path.runBase,
                        control.path.runProgram,
                        (runResult: any) => {
                            console.log(`Run ${x} finished`);
                            console.log(runResult);
                            if (runResult.result !== undefined) {
                                if (runResult.result.error === undefined) {
                                    const results =
                                        makeResult(
                                            runResult.result,
                                            control.bfsSetup.path.join(
                                                control.path.runBase,
                                                `${this.state.currentFileName}.json`));
                                    const checks = runResult.result.$checks;
                                    this.setState({
                                        interactions: results,
                                        checks: checks
                                    });

                                    if (results[0].name === "error") {
                                        this.setState(
                                            {
                                                interactionErrors: runResult.result.error,
                                            }
                                        );
                                    }
                                } else {
                                    this.setState({
                                        interactionErrors: [runResult.result.error],
                                    });
                                }
                            }
                        },
                        this.state.runKind);
                } else if (this.state.compileState === CompileState.CompileQueue) {
                    this.setState({compileState: CompileState.Ready});
                    this.update();
                } else {
                    invalidCompileState(this.state.compileState);
                }
            });

        this.state = {
            browseRoot: this.props.browseRoot,
            browsePath: this.props.browsePath,
            currentFileDirectory: this.props.currentFileDirectory,
            currentFileName: this.props.currentFileName,
            currentFileContents: control.openOrCreateFile(
                control.bfsSetup.path.join(
                    ...this.props.currentFileDirectory,
                    this.props.currentFileName)),
            typeCheck: true,
            checks: [],
            interactions: [{
                name: "Note",
                value: "Press Run to compile and run"
            }],
            interactionErrors: [],
            lintFailures: {},
            runKind: control.backend.RunKind.Async,
            autoRun: true,
            updateTimer: setTimeout(() => { return; }, 0),
            dropdownVisible: false,
            editorMode: EEditor.Chunks,
            fontSize: 12,
            message: "Ready to rock",
            definitionsHighlights: [],
            fsBrowserVisible: false,
            compileState: CompileState.Startup,
        };
    };

    get isPyretFile() {
        return /\.arr$/.test(this.currentFile);
    }

    get currentFile() {
        return control.bfsSetup.path.join(
            ...this.state.currentFileDirectory,
            this.state.currentFileName);
    }

    get currentFileName() {
        return this.state.currentFileName;
    }

    get currentFileDirectory() {
        return control.bfsSetup.path.join(...this.state.currentFileDirectory);
    }

    get stopify() {
        return this.state.runKind === control.backend.RunKind.Async;
    }

    run = () => {
        this.setState(
            {
                interactionErrors: [],
                definitionsHighlights: []
            }
        );
        if (this.isPyretFile) {
            if (this.state.compileState === CompileState.Startup) {
                this.setState({compileState: CompileState.StartupQueue});
            } else if (this.state.compileState === CompileState.StartupQueue) {
                // state remains as StartupQueue
            } else if (this.state.compileState === CompileState.Ready) {
                this.setState({compileState: CompileState.Compile});
                control.compile(
                    this.currentFileDirectory,
                    this.currentFileName,
                    this.state.typeCheck);
            } else if (this.state.compileState === CompileState.Compile) {
                this.setState({compileState: CompileState.CompileQueue});
            } else if (this.state.compileState === CompileState.CompileQueue) {
                // state remains as CompileQueue
            } else {
                invalidCompileState(this.state.compileState);
            }
        } else {
            this.setState({
                interactions: [
                    {
                        name: "Error",
                        value: "Run is not supported on this file type"
                    },
                    {
                        name: "File",
                        value: this.currentFile
                    }],
                interactionErrors: ["Error: Run is not supported on this file type"],
            });
        }
    };

    update = (): void => {
        control.fs.writeFileSync(
            this.currentFile,
            this.state.currentFileContents);
        if (this.state.autoRun) {
            this.run();
        }
    }

    onEdit = (value: string): void => {
        clearTimeout(this.state.updateTimer);
        this.setState({
    //        currentFileContents: value,
            updateTimer: setTimeout(() => {
                this.setState({currentFileContents: value});
                this.update();
            }, 250),
        });
    }

    onTraverseDown = (path: string[]) => {
        this.setState({
            browsePath: path,
        });
    };

    onTraverseUp = (path: string[]) => {
        this.setState({
            browsePath: path,
        });
    };

    onExpandChild = (child: string, fullChildPath: string): void => {
        this.setState({
            interactions: [{
                name: "Note",
                value: "Press Run to compile and run"
            }],
            currentFileDirectory: this.state.browsePath,
            currentFileName: child,
            currentFileContents: control.fs
                                        .readFileSync(fullChildPath, "utf-8"),
        });
    };

    setEditorMode = (editorMode: EEditor) => {
        this.setState({ editorMode });
    }

    loadBuiltins = (e: React.MouseEvent<HTMLElement>): void => {
        control.loadBuiltins();
    };

    removeRootDirectory = (e: React.MouseEvent<HTMLElement>): void => {
        control.removeRootDirectory();
    };

    makeHeaderButton = (text: string, enabled: boolean, onClick: () => void) => {
        return (
            <button className={(enabled ? "run-option-enabled" : "run-option-disabled")}
                    onClick={onClick}>
                {text}
            </button>
        );
    };

    toggleDropdownVisibility = (e: any) => {
        this.setState({
            dropdownVisible: !this.state.dropdownVisible
        });
    };

    toggleAutoRun = () => {
        this.setState({
            autoRun: !this.state.autoRun
        });
    };

    toggleStopify = () => {
        if (this.stopify) {
            this.setState({
                runKind: control.backend.RunKind.Sync
            });
        } else {
            this.setState({
                runKind: control.backend.RunKind.Async
            })
        }
    };

    toggleTypeCheck = () => {
        this.setState({
            typeCheck: !this.state.typeCheck
        });
    };

    onDecreaseFontSize = () => {
        if (this.state.fontSize > 1) {
            this.setState({
                fontSize: this.state.fontSize - 1
            });
        }
    };

    onIncreaseFontSize = () => {
        this.setState({
            fontSize: this.state.fontSize + 1
        });
    };

    onResetFontSize = () => {
        this.setState({
            fontSize: 12
        });
    };

    removeDropdown = () => {
        this.setState({
            dropdownVisible: false
        });
    };

    setMessage = (newMessage: string) => {
        this.setState({
            message: newMessage
        });
    };

    makeDefinitions() {
        if (this.state.editorMode === EEditor.Text) {
            return <SingleCodeMirrorDefinitions
                text={this.state.currentFileContents}
                onEdit={this.onEdit}
                highlights={this.state.definitionsHighlights}>
            </SingleCodeMirrorDefinitions>;
        }
        else if (this.state.editorMode === EEditor.Chunks) {
            return (<DefChunks
                lintFailures={this.state.lintFailures}
                name={this.state.currentFileName}
                highlights={this.state.definitionsHighlights}
                program={this.state.currentFileContents}
                onEdit={this.onEdit}></DefChunks>);
        }
    }

    render() {
        const interactionValues =
            <div style={{ fontSize: this.state.fontSize }}>
                <pre className="checks-area">
                    { this.state.checks && this.state.checks.map(c => <TestResult check={c}></TestResult>)}
                </pre>
                <pre className="interactions-area">
                    {
                        this.state.interactions.map(
                            (i) => {
                                return <Interaction key={i.name}
                                                    name={i.name}
                                                    value={i.value}/>
                            })
                    }
                </pre>
            </div>;

        const dropdown = this.state.dropdownVisible && (
            <Dropdown>
                <DropdownOption enabled={this.state.autoRun}
                                onClick={this.toggleAutoRun}>
                    Auto Run
                </DropdownOption>
                <DropdownOption enabled={this.stopify}
                                onClick={this.toggleStopify}>
                    Stopify
                </DropdownOption>
                <DropdownOption enabled={this.state.typeCheck}
                                onClick={this.toggleTypeCheck}>
                    Type Check
                </DropdownOption>
            </Dropdown>);

        const fsBrowser =
            <FSBrowser root={this.state.browseRoot}
                       onTraverseUp={this.onTraverseUp}
                       onTraverseDown={this.onTraverseDown}
                       onExpandChild={this.onExpandChild}
                       browsePath={this.state.browsePath}
                       key="FSBrowser">
            </FSBrowser>;

        const fontSize =
            <FontSize onIncrease={this.onIncreaseFontSize}
                      onDecrease={this.onDecreaseFontSize}
                      onReset={this.onResetFontSize}
                      size={this.state.fontSize}
                      key="FontSize">
            </FontSize>;

        const textEditor =
            <button className="text-editor"
                    onClick={() => this.setEditorMode(EEditor.Text)}
                    key="TextEditor">
                Text
            </button>;

        const chunkEditor =
            <button className="chunk-editor"
                    onClick={() => this.setEditorMode(EEditor.Chunks)}
                    key="ChunkEditor">
                Chunks
            </button>;

        const builtinsLoader =
            <button onClick={control.loadBuiltins}>
                Load Builtins
            </button>;

        const menu =
            <Menu>
                <Tab name="📁">
                    {fsBrowser}
                </Tab>
                <Tab name="⚙">
                    {textEditor}
                    {chunkEditor}
                    {builtinsLoader}
                    {fontSize}
                </Tab>
            </Menu>;

        const rightHandSide =
            <div className="interactions-area-container">
                {this.state.interactionErrors.length > 0 ? (
                    <SplitterLayout vertical={true}
                                    percentage={true}>
                        {interactionValues}
                        <InteractionError fontSize={this.state.fontSize}>
                            {this.state.interactionErrors}
                        </InteractionError>
                    </SplitterLayout>
                ) : interactionValues}
            </div>;

        const definitions = this.makeDefinitions();

        return (
            <div className="page-container">
                <Header>
                    {this.stopify ? (
                        <button className="stop-available">
                            Stop
                        </button>
                    ) : (
                        <button className="stop-unavailable">
                            Stop
                        </button>
                    )}
                    <div className="run-container">
                        <button className="run-ready"
                                onClick={this.run}>
                            Run
                        </button>
                        <button className="run-options"
                                onClick={this.toggleDropdownVisibility}
                                onBlur={this.removeDropdown}>&#8628;{dropdown}
                        </button>
                    </div>
                </Header>
                <div className="code-container">
                    {menu}
                    <SplitterLayout vertical={false}
                                    percentage={true}>
                        <div className="edit-area-container"
                             style={{ fontSize: this.state.fontSize }}>
                            {definitions}
                        </div>
                        {rightHandSide}
                    </SplitterLayout>
                </div>
                <Footer message={compileStateToString(this.state.compileState)}></Footer>
            </div>
        );
    }
}

class App extends React.Component<AppProps, AppState> {
    render() {
        return (
            <Editor browseRoot="/"
                    browsePath={["/", "projects"]}
                    currentFileDirectory={["/", "projects"]}
                    currentFileName="program.arr">
            </Editor>
        );
    };
}

export default App;
