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
    openFilePath: string;
    contents: string;
};

type EditorState = {
    typeCheck: boolean;
    path: string[];
    openFilePath: string;
    contents: string;
    interactions: {name: string, value: any}[];
    fsBrowserVisible: boolean;
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
                        this.setState(
                            {
                                interactions: makeResult(runResult.result)
                            }
                        );
                    });
            });

        this.state = {
            typeCheck: true,
            fsBrowserVisible: false,
            interactions: [{
                name: "Note",
                value: "Press Run to compile and run"
            }],
            path: [],
            openFilePath: this.props.openFilePath,
            contents: this.props.contents
        };
    };

    static isPyretFile(path: string) {
        return /\.arr$/.test(path);
    }

    run = () => {
        if (Editor.isPyretFile(this.state.openFilePath)) {
            control.compile(
                control.path.compileBase,
                control.path.compileProgram,
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
                        value: this.state.openFilePath
                    }]
            });
        }
    };

    autosave = (e: React.ChangeEvent<HTMLTextAreaElement>) => {
        this.setState({contents: e.target.value});
        control.fs.writeFileSync(this.state.openFilePath, e.target.value);
    }

    traverseDown = (childDirectory: string) => {
        const newPath = this.state.path.slice();
        newPath.push(childDirectory);

        this.setState({
            path: newPath
        });
    }

    traverseUp = () => {
        const newPath = this.state.path.slice();
        newPath.pop();

        this.setState({
            path: newPath
        });
    }

    get currentDirectory() {
        if (this.state.path.length === 0) {
            return "/";
        } else {
            const path = this.state.path
                             .reduce(
                                 (acc, v) => {
                                     return `${acc}/${v}`;
                                 },
                                 "");
            return `${path}/`;
        }
    }

    fullPathTo = (file: string) => {
        return `${this.currentDirectory}${file}`;
    }

    expandChild = (child: string) => {
        const stats = control.fs.statSync(this.fullPathTo(child));

        if (stats.isDirectory()) {
            this.traverseDown(child);
        } else if (stats.isFile()) {
            this.setState({
                interactions: [{
                    name: "Note",
                    value: "Press Run to compile and run"
                }],
                openFilePath: this.fullPathTo(child),
                contents: control.fs.readFileSync(this.fullPathTo(child))
            });
        }
    }

    createFSItemPair = (filePath: string) => {
        return [
            filePath,
            <FSItem key={filePath}
                    onClick={() => this.expandChild(filePath)}
                    contents={filePath} />
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
    }


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
                </div>
                <div id="main">
                    <div id="edit-box">
                        {
                            (this.state.fsBrowserVisible ? (
                                <ul id="fs-browser">
                                    {(this.state.path.length !== 0) ? (
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
                                            .readdirSync(this.currentDirectory)
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
                                {this.state.openFilePath}
                            </div>
                            <div id="main-container">
                                <div id="definitions-container">
                                    <textarea className="editor"
                                              value={this.state.contents}
                                              onChange={this.autosave}>
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
            <Editor openFilePath={control.path.program}
                    contents={this.state.editorContents}>
            </Editor>
        );
    };
}

export default App;
