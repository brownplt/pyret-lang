import React from 'react';
import './App.css';
import {Interaction} from './Interaction';
import * as control from './control';
import {UnControlled as CodeMirror} from 'react-codemirror2';
import 'codemirror/lib/codemirror.css';
import 'pyret-codemirror-mode/css/pyret.css';
import 'codemirror/mode/javascript/javascript.js';
import SplitPane from 'react-split-pane';

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
    codeContainerWidth: undefined | number;
    splitX: undefined | number;
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

        this.codeContainerRef = React.createRef();

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
            codeContainerWidth: undefined,
            splitX: undefined,
        };
    };

    componentDidMount() {
        if (this.codeContainerRef.current != null) {
            this.setState({
                codeContainerWidth: this.codeContainerRef.current.clientWidth
            });
        }
    }

    private codeContainerRef: React.RefObject<HTMLDivElement>;

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
            <button className= {(enabled ? "run-option-enabled" : "run-option-disabled")}
                    onClick={onClick}>
                {text}
            </button>
        );
    };

    render() {
        return (
            this.state.debug ? (
                <div id="outer-box">
                    <div id="header">
                        <button className="right-header-button"
                                onClick={this.run}>
                            Run
                        </button>
                        <button className="left-header-button"
                                onClick={this.toggleFSBrowser}>
                            File System
                        </button>
                        <button className="left-header-button"
                                onClick={this.loadBuiltins}>
                            Load Builtins
                        </button>
                        <button className="left-header-button"
                                onClick={this.removeRootDirectory}>
                            Remove Root
                        </button>
                        <div className="header-run-option">
                            <input type="checkbox"
                                   checked={this.state.typeCheck}
                                   name="typeCheck"
                                   onChange={(e) => {
                                       this.setState({
                                           typeCheck: !this.state.typeCheck
                                       });
                                   }}>
                            </input>
                            <label htmlFor="typeCheck">
                                Type Check
                            </label>
                        </div>
                        <div className="header-run-option">
                            <input type="checkBox"
                                   checked={this.state.runKind === control.backend.RunKind.Async}
                                   name="stopify"
                                   onChange={(e) => {
                                       if (this.state.runKind === control.backend.RunKind.Async) {
                                           this.setState({
                                               runKind: control.backend.RunKind.Sync
                                           });
                                       } else {
                                           this.setState({
                                               runKind: control.backend.RunKind.Async
                                           })
                                       }
                                   }}>
                            </input>
                            <label htmlFor="stopify">
                                Stopify
                            </label>
                        </div>
                        <div className="header-run-option">
                            <input type="checkBox"
                                   checked={this.state.autoRun}
                                   name="autoRun"
                                   onChange={(e) => {
                                       this.setState({
                                           autoRun: !this.state.autoRun
                                       });
                                   }}>
                            </input>
                            <label htmlFor="autoRun">
                                Auto Run
                            </label>
                        </div>
                    </div>
                    <div id="main">
                        <div id="edit-box">
                            {
                                (this.state.fsBrowserVisible ? (
                                    <ul id="fs-browser">
                                        {(!this.browsingRoot) ? (
                                            <li onClick={() => {
                                                this.traverseUp();
                                            }}
                                                className="fs-browser-item">
                                                ..
                                            </li>
                                        ) : (
                                            null
                                        )}
                                        {
                                            control.fs
                                                   .readdirSync(this.browsePath)
                                                   .map(this.createFSItemPair)
                                                   .sort(this.compareFSItemPair)
                                                   .map((x: [string, FSItem]) => x[1])
                                        }
                                    </ul>
                                ) : (
                                    null
                                ))
                            }
                            <div id="file-container">
                                <div id="file-name-label">
                                    {this.currentFile}
                                </div>
                                <div id="main-container">
                                    <div id="definitions-container">
                                        <CodeMirror
                                            value={this.state.currentFileContents}
                                            options={{
                                                mode: 'pyret',
                                                theme: 'default',
                                                lineNumbers: true
                                            }}
                                            onChange={this.onEdit}
                                            autoCursor={false}>
                                        </CodeMirror>
                                    </div>
                                    <div id="separator">
                                    </div>
                                    <div id="interactions-container">
                                        <pre id="interactions-area"
                                             className="code">
                                            {
                                                this.state.interactions.map(
                                                    (i) => {
                                                        return <Interaction key={i.name} name={i.name} value={i.value} />
                                                    })
                                            }
                                        </pre>
                                        {
                                            (() => {
                                                console.log(this.state.interactErrorExists);
                                                return (this.state.interactErrorExists ? (
                                                    <div id="interaction-error">
                                                        <p>{this.state.interactionErrors}</p>
                                                    </div>
                                                ) : (
                                                    null
                                                ));
                                            })()
                                        }
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div id="footer"></div>
                </div>
            ) : (
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
                        <button className="run-queued">
                            Run
                        </button>
                        {this.makeHeaderButton(
                            "Stopify",
                            this.state.runKind === control.backend.RunKind.Async,
                            () => {
                                if (this.state.runKind === control.backend.RunKind.Async) {
                                    this.setState({
                                        runKind: control.backend.RunKind.Sync
                                    });
                                } else {
                                    this.setState({
                                        runKind: control.backend.RunKind.Async
                                    })
                                }
                            }
                        )}
                        {this.makeHeaderButton(
                            "Auto Run",
                            this.state.autoRun,
                            () => {
                                this.setState({
                                    autoRun: !this.state.autoRun
                                })
                            })
                        }
                        {this.makeHeaderButton(
                            "Type Check",
                            this.state.typeCheck,
                            () => {
                                this.setState({
                                    typeCheck: !this.state.typeCheck
                                });
                            })
                        }
                    </div>
                    <div className="code-container"
                         ref={this.codeContainerRef}>
                        <SplitPane split="vertical"
                                   defaultSize={500}>
                            <div className="edit-area-container"
                                 style={{width: (this.state.splitX === undefined ? (
                                     "50%"
                                 ) : (
                                     this.state.splitX
                                 ))}}>
                            </div>
                            <div className="interactions-area-container"
                                 style={{width: (this.state.splitX === undefined ? (
                                     "50%"
                                 ) : (
                                     this.state.codeContainerWidth! - this.state.splitX
                                 ))}}>
                            </div>
                        </SplitPane>
                    </div>
                    <div className="footer-container">
                    </div>
                </div>
            )
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
