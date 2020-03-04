import React from 'react';
import './App.css';
import { TextCompileState, compileStateToString, invalidTextCompileState } from './CompileState';
import { Interaction } from './Interaction';
import { Check, TestResult } from './Check';
import { DefChunks, CHUNKSEP } from './DefChunks';
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

function makeResult(result: any, moduleUri: string): { key: string, name: string, value: any }[] {
    const compareLocations = (a: any, b: any): number => {
        return a.srcloc[1] - b.srcloc[1];
    };

    // There may be toplevel expressions in many modules, but we only want to
    // show the ones from the main module we're working on
    const mainTraces = result.$traces.filter((t : any) => t.srcloc[0] === moduleUri);

    const allWithLocs = result.$locations.concat(mainTraces);

    // We combine and then sort to get the traces interleaved correctly with named values
    const allSorted = allWithLocs.sort(compareLocations);
    return allSorted.map((key: any) => {
        if('name' in key) {
            return {
                name: key.name,
                key: key.name,
                line: key.srcloc[1],
                value: result[key.name]
            };
        }
        else {
            return {
                name: "",
                key: String(key.srcloc[1]),
                line: key.srcloc[1],
                value: key.value
            };
        }
    });
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

type EditorState = {
    browseRoot: string;
    browsePath: string[];
    currentFileDirectory: string[];
    currentFileName: string;
    currentFileContents: string;
    typeCheck: boolean;
    checks: Check[],
    interactions: { key: string, name: string, value: any }[];
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
    compileState: TextCompileState;
    currentRunner: any;
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

                if (this.state.compileState === TextCompileState.Startup) {
                    this.setState({compileState: TextCompileState.Ready});
                } else if (this.state.compileState === TextCompileState.StartupQueue) {
                    this.setState({compileState: TextCompileState.Ready});
                    this.update();
                } else {
                    invalidTextCompileState(this.state.compileState);
                }
            },
            (errors: string[]) => {
                console.log("COMPILE FAILURE");
                if (this.state.compileState === TextCompileState.Compile
                    || this.state.compileState === TextCompileState.CompileRun) {
                    this.setState({compileState: TextCompileState.Ready});

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
                } else if (this.state.compileState === TextCompileState.CompileQueue
                           || this.state.compileState === TextCompileState.CompileRunQueue) {
                    this.setState({compileState: TextCompileState.Ready});
                    this.update();
                } else {
                    invalidTextCompileState(this.state.compileState);
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
                if (this.state.compileState === TextCompileState.Compile) {
                    this.setState({compileState: TextCompileState.Ready});
                } else if (this.state.compileState === TextCompileState.CompileQueue
                           || this.state.compileState === TextCompileState.CompileRunQueue) {
                    this.setState({compileState: TextCompileState.Ready});
                    this.update();
                } else if (this.state.compileState === TextCompileState.CompileRun) {
                    if (this.stopify) {
                        this.setState({compileState: TextCompileState.RunningWithStops});
                    } else {
                        this.setState({compileState: TextCompileState.RunningWithoutStops});
                    }
                    const x = new Date();
                    console.log(`Run ${x} started`);
                    control.run(
                        control.path.runBase,
                        control.path.runProgram,
                        (runResult: any) => {
                            this.setState({compileState: TextCompileState.Ready});
                            console.log(`Run ${x} finished`);
                            console.log(runResult);
                            if (runResult.result !== undefined) {
                                if (runResult.result.error === undefined) {
                                    const results =
                                        makeResult(
                                            runResult.result,
                                            'file://' + 
                                            control.bfsSetup.path.join(
                                                control.path.compileBase,
                                                this.state.currentFileName));
                                    const checks = runResult.result.$checks;
                                    this.setState({
                                        interactions: results,
                                        checks: checks
                                    });

                                    if (results[0] !== undefined && results[0].name === "error") {
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
                        (runner: any) => {
                            this.setState({currentRunner: runner});
                        },
                        this.state.runKind);
                } else {
                    invalidTextCompileState(this.state.compileState);
                }
            },
            () => {
                // onCreateReplSuccess
                console.log("REPL successfully created");
                return;
            },
            (response) => {
                // onCompileInteractionSuccess
                console.log(`Chunk ${response.program} successfully compiled.`);
                return;
            },
            (response) => {
                // onCompileInteractionFailure
                console.error(`Failed to compile ${response.program}.`);
                return;
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
                key: "Note",
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
            compileState: TextCompileState.Startup,
            currentRunner: undefined,
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

    run = (runAfterwards: boolean) => {
        this.setState(
            {
                interactionErrors: [],
                definitionsHighlights: []
            }
        );
        if (this.isPyretFile) {
            if (this.state.compileState === TextCompileState.Startup) {
                this.setState({compileState: TextCompileState.StartupQueue});
            } else if (this.state.compileState === TextCompileState.StartupQueue) {
                // state remains as StartupQueue
            } else if (this.state.compileState === TextCompileState.Ready
                       || this.state.compileState === TextCompileState.Stopped) {
                if (runAfterwards || this.state.autoRun) {
                    this.setState({compileState: TextCompileState.CompileRun});
                } else {
                    this.setState({compileState: TextCompileState.Compile});
                }
                control.compile(
                    this.currentFileDirectory,
                    this.currentFileName,
                    this.state.typeCheck);
            } else if (this.state.compileState === TextCompileState.Compile) {
                this.setState({compileState: TextCompileState.CompileQueue});
            } else if (this.state.compileState === TextCompileState.CompileRun) {
                this.setState({compileState: TextCompileState.CompileRunQueue});
            } else if (this.state.compileState === TextCompileState.CompileQueue) {
                // state remains as CompileQueue
            } else if (this.state.compileState === TextCompileState.CompileRunQueue) {
                // state remains as CompileRunQueue
            } else if (this.state.compileState === TextCompileState.RunningWithStops) {
                this.stop();
                this.update();
                // state remains as RunningWithStops
            } else if (this.state.compileState === TextCompileState.RunningWithoutStops) {
                // state remains as RunningWithoutStops
            } else {
                invalidTextCompileState(this.state.compileState);
            }
        } else {
            this.setState({
                interactions: [
                    {
                        key: "Error",
                        name: "Error",
                        value: "Run is not supported on this file type"
                    },
                    {
                        key: "File",
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

        if (this.state.editorMode === EEditor.Chunks) {
            const chunkstrs = this.state.currentFileContents.split(CHUNKSEP);
            for (let i = 0; i < chunkstrs.length; i++) {
                control.fs.writeFileSync(
                    `${this.currentFile}.chunk.${i}`,
                    chunkstrs[i]);
            }

            for (let i = 0; i < chunkstrs.length; i++) {
                console.log(`Sending message to webworker to compile: ${this.currentFile}.chunk.${i}`)
                control.backend.compileInteraction(control.worker, `${this.currentFile}.chunk.${i}`);
                // todo: wait for response before compiling more chunks
            }
        } else {
            this.run(false);
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
                key: "Note",
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

    stop = () => {
        if (this.state.currentRunner !== undefined) {
            this.state.currentRunner.pause((line: number) => console.log("paused on line", line))
            this.setState({
                currentRunner: undefined,
                compileState: TextCompileState.Stopped
            });
        }
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
            control.createRepl();

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
                                return <Interaction key={i.key}
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
                    {this.stopify && this.state.compileState === TextCompileState.RunningWithStops ? (
                        <button className="stop-available"
                                onClick={this.stop}>
                            Stop
                        </button>
                    ) : (
                        <button className="stop-unavailable">
                            Stop
                        </button>
                    )}
                    <div className="run-container">
                        <button className="run-ready"
                                onClick={() => this.run(true)}>
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
