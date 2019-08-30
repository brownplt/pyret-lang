import React from 'react';
import './App.css';
import {Interaction} from './Interaction';
import * as control from './control';
import {UnControlled as CodeMirror} from 'react-codemirror2';
import 'codemirror/lib/codemirror.css';
import 'pyret-codemirror-mode/css/pyret.css';
import 'codemirror/mode/javascript/javascript.js';
import SplitterLayout from 'react-splitter-layout';
import 'react-splitter-layout/lib/index.css';

// pyret-codemirror-mode/mode/pyret.js expects window.CodeMirror to exist and
// to be bound to the 'codemirror' import.
import * as RawCodeMirror from 'codemirror';
(window as any).CodeMirror = RawCodeMirror;
require('pyret-codemirror-mode/mode/pyret');

control.installFileSystem();
control.loadBuiltins();

type AppProps = {};
type AppState = {};

function makeResult(result: any): {name: string, value: any}[] {
    return Object.keys(result).sort().map((key) => {
        return {
            name: key,
            value: result[key]
        }
    });
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
    interactions: {name: string, value: any}[];
    interactionErrors: string[];
    interactErrorExists: boolean;
    fsBrowserVisible: boolean;
    runKind: control.backend.RunKind;
    autoRun: boolean;
    updateTimer: NodeJS.Timer;
    debug: boolean;
    dropdownVisible: boolean;
};

type FSItemProps = {
    onClick: () => void;
    contents: string;
};

type FSItemState = {};

class FSItem extends React.Component<FSItemProps, FSItemState> {
    get contents() {
        return this.props.contents;
    }

    render() {
        return (
            <li onClick={this.props.onClick}
                className="fs-browser-item">
                {this.props.contents}
            </li>
        );
    }
}

class Editor extends React.Component<EditorProps, EditorState> {
    constructor(props: EditorProps) {
        super(props);

        control.setupWorkerMessageHandler(
            console.log,
            (errors: string[]) => {
                console.log("Error (App.ts): ", errors);
                this.setState(
                    {
                        interactionErrors: errors,
                        interactErrorExists: true
                    }
                );
            },
            () => {
                control.run(
                    control.path.runBase,
                    control.path.runProgram,
                    (runResult: any) => {
                        console.log(runResult);
                        if (runResult.result !== undefined) {
                            this.setState(
                                {
                                    interactions: makeResult(runResult.result)
                                }
                            );
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
            fsBrowserVisible: false,
            runKind: control.backend.RunKind.Async,
            autoRun: true,
            updateTimer: setTimeout(this.update, 2000),
            debug: false,
            dropdownVisible: false,
        };
    };

    get isPyretFile() {
        return /\.arr$/.test(this.currentFile);
    }

    get browsePath() {
        return control.bfsSetup.path.join(...this.state.browsePath);
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

    get browsingRoot() {
        return control.bfsSetup.path.join(...this.state.browsePath) ===
            this.state.browseRoot;
    }

    run = () => {
        this.setState(
            {
                interactionErrors: [],
                interactErrorExists: false
            }
        );
        if (this.isPyretFile) {
            control.compile(
                this.currentFileDirectory,
                this.currentFileName,
                this.state.typeCheck);
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

    onEdit = (editor: CodeMirror.Editor, data: CodeMirror.EditorChange, value: string): void => {
        clearTimeout(this.state.updateTimer);

        this.setState({
            currentFileContents: value,
            updateTimer: setTimeout(this.update, 250)
        });
    };

    traverseDown = (childDirectory: string) => {
        const newPath = this.state.browsePath.slice();
        newPath.push(childDirectory);

        this.setState({
            browsePath: newPath,
        });
    };

    traverseUp = () => {
        const newPath = this.state.browsePath.slice();
        newPath.pop();

        this.setState({
            browsePath: newPath,
        });
    };

    expandChild = (child: string) => {
        const fullChildPath =
            control.bfsSetup.path.join(this.browsePath, child);
        const stats = control.fs.statSync(fullChildPath);

        if (stats.isDirectory()) {
            this.traverseDown(child);
        } else if (stats.isFile()) {
            this.setState({
                interactions: [{
                    name: "Note",
                    value: "Press Run to compile and run"
                }],
                currentFileDirectory: this.state.browsePath,
                currentFileName: child,
                currentFileContents: control.fs.readFileSync(fullChildPath, "utf-8"),
            });
        }
    };

    createFSItemPair = (filePath: string) => {
        return [
            filePath,
            <FSItem key={filePath}
                    onClick={() => this.expandChild(filePath)}
                    contents={filePath}/>
        ];
    };

    compareFSItemPair = (a: [string, FSItem], b: [string, FSItem]) => {
        if (a[0] < b[0]) {
            return -1;
        } else if (a[0] > b[0]) {
            return 1;
        } else {
            return 0;
        }
    };

    toggleFSBrowser = () => {
        this.setState({
            fsBrowserVisible: !this.state.fsBrowserVisible
        });
    };

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

    makeDropdownOption = (text: string, enabled: boolean, onClick: () => void) => {
        return (
            <div className={enabled ? "run-option-enabled" : "run-option-disabled"}
                 onClick={onClick}>
                <input type="checkBox"
                       checked={enabled}
                       name={text}
                       className="run-option-checkbox"
                       readOnly={true}>
                </input>
                <label htmlFor={text}
                       className="run-option-label">
                    {text}
                </label>
            </div>
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
        if (this.state.runKind === control.backend.RunKind.Async) {
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

    render() {
        return (
            <div className="page-container">
                <div className="header-container">
                    {this.state.runKind === control.backend.RunKind.Async ? (
                        <button className="stop-available">
                            Stop
                        </button>
                    ) : (
                        <button className="stop-unavailable">
                            Stop
                        </button>
                    )}
                    <div className="run-container">
                        <button className="run-ready">
                            Run
                        </button>
                        <button className="run-options"
                                onClick={this.toggleDropdownVisibility}>&#8628;</button>
                    </div>
                    {this.state.dropdownVisible ? (
                        <div className="run-dropdown">
                            {this.makeDropdownOption("Auto Run", this.state.autoRun, this.toggleAutoRun)}
                            {this.makeDropdownOption("Stopify", this.state.runKind === control.backend.RunKind.Async, this.toggleStopify)}
                            {this.makeDropdownOption("Type Check", this.state.typeCheck, this.toggleTypeCheck)}
                        </div>
                    ) : (
                        null
                    )}
                </div>
                <div className="code-container">
                    <SplitterLayout vertical={false}
                                    percentage={true}>
                        <div className="edit-area-container">
                            <CodeMirror value={this.state.currentFileContents}
                                        options={{
                                            mode: 'pyret',
                                            theme: 'default',
                                            lineNumbers: true
                                        }}
                                        onChange={this.onEdit}
                                        autoCursor={false}>
                            </CodeMirror>
                        </div>
                        <div className="interactions-area-container"></div>
                    </SplitterLayout>
                </div>
                <div className="footer-container">
                </div>
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
