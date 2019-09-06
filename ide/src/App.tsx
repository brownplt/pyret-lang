import React from 'react';
import './App.css';
import { Interaction } from './Interaction';
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

function makeResult(result: any): { name: string, value: any }[] {
    return Object.keys(result).sort().map((key) => {
        return {
            name: key,
            value: result[key]
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
    interactions: { name: string, value: any }[];
    interactionErrors: string[];
    interactErrorExists: boolean;
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
    menuVisible: boolean;
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
            (errors: string[]) => {
                this.setMessage("Compilation failed with error(s)")
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
                        interactErrorExists: true,
                        definitionsHighlights: places
                    }
                );
            },
            (errors: string[]) => {
                this.setState(
                    {
                        interactionErrors: [errors.toString()],
                        interactErrorExists: true,
                    }
                );
            },
            onLintFailure,
            onLintSuccess,
            () => {
                this.setMessage("Run started");
                control.run(
                    control.path.runBase,
                    control.path.runProgram,
                    (runResult: any) => {
                        console.log(runResult);
                        if (runResult.result !== undefined) {
                            if (runResult.result.error === undefined) {
                                this.setMessage("Run completed successfully");

                                this.setState({
                                    interactions: makeResult(runResult.result)
                                });
                                if (makeResult(runResult.result)[0].name === "error") {
                                    this.setState(
                                        {
                                            interactionErrors: runResult.result.error,
                                            interactErrorExists: true
                                        }
                                    );
                                }
                            } else {
                                this.setMessage("Run failed with error(s)");

                                this.setState({
                                    interactionErrors: [runResult.result.error],
                                    interactErrorExists: true
                                });
                            }
                        }
                    },
                    this.state.runKind);
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
            interactions: [{
                name: "Note",
                value: "Press Run to compile and run"
            }],
            interactionErrors: [],
            interactErrorExists: false,
            lintFailures: {},
            runKind: control.backend.RunKind.Async,
            autoRun: true,
            updateTimer: setTimeout(this.update, 2000),
            dropdownVisible: false,
            editorMode: EEditor.Text,
            fontSize: 12,
            message: "Ready to rock",
            definitionsHighlights: [],
            fsBrowserVisible: false,
            menuVisible: false,
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
                interactErrorExists: false
            }
        );
        if (this.isPyretFile) {
            this.setMessage("Compilation started");
            control.compile(
                this.currentFileDirectory,
                this.currentFileName,
                this.state.typeCheck);
        } else {
            this.setMessage("Visited a non-pyret file");
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
                interactErrorExists: true
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
            currentFileContents: value,
            updateTimer: setTimeout(this.update, 250),
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

    toggleMenuVisibility = (): void => {
        this.setState({
            menuVisible: !this.state.menuVisible,
        });
    };

    makeDefinitions() {
        if (this.state.editorMode === EEditor.Text) {
            return <SingleCodeMirrorDefinitions
                text={this.state.currentFileContents}
                onEdit={this.onEdit}
                highlights={this.state.definitionsHighlights}
                interactErrorExists={this.state.interactErrorExists}>
            </SingleCodeMirrorDefinitions>;
        }
        else if (this.state.editorMode === EEditor.Chunks) {
            return (<DefChunks
                lintFailures={this.state.lintFailures}
                name={this.state.currentFileName}
                highlights={this.state.definitionsHighlights}
                interactErrorExists={this.state.interactErrorExists}
                program={this.state.currentFileContents}
                onEdit={this.onEdit}></DefChunks>);
        }
    }

    render() {
        const interactionValues =
            <pre className="interactions-area"
                 style={{ fontSize: this.state.fontSize }}>
                {
                    this.state.interactions.map(
                        (i) => {
                            return <Interaction key={i.name}
                                                name={i.name}
                                                value={i.value}
                                                setMessage={this.setMessage}/>
                        })
                }
            </pre>;

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

        const menu =
            <Menu>
                <Tab name="📁">
                    {fsBrowser}
                </Tab>
                <Tab name="⚙">
                    {textEditor}
                    {chunkEditor}
                    {fontSize}
                </Tab>
            </Menu>;

        const rightHandSide =
            <div className="interactions-area-container">
                {this.state.interactErrorExists ? (
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
                    <button className="menu"
                            onClick={this.toggleMenuVisibility}>
                        Menu
                    </button>
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
                    {this.state.menuVisible && menu}
                    <SplitterLayout vertical={false}
                                    percentage={true}>
                        <div className="edit-area-container"
                             style={{ fontSize: this.state.fontSize }}>
                            {definitions}
                        </div>
                        {rightHandSide}
                    </SplitterLayout>
                </div>
                <Footer message={this.state.message}></Footer>
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
