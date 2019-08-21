import React from 'react';
import './App.css';
import {Interaction} from './Interaction';
import * as control from './control';

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
    fsBrowserVisible: boolean;
    runKind: control.backend.RunKind;
    autoRun: boolean;
    updateTimer: NodeJS.Timer;
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
            console.log,
            () => { return; },
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
            fsBrowserVisible: false,
            runKind: control.backend.RunKind.Async,
            autoRun: true,
            updateTimer: setTimeout(this.update, 2000),
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
                    }]
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

    onEdit = (e: React.ChangeEvent<HTMLTextAreaElement>): void => {
        clearTimeout(this.state.updateTimer);

        this.setState({
            currentFileContents: e.target.value,
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
                currentFileContents: control.fs.readFileSync(fullChildPath),
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
    }

    render() {
        return (
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
                                    <textarea className="editor"
                                              value={this.state.currentFileContents}
                                              onChange={this.onEdit}>
                                    </textarea>
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
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
                <div id="footer"></div>
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
