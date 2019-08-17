import React from 'react';
import './App.css';

const control = require('./control.ts');

control.installFileSystem();
control.loadBuiltins();

type AppProps = {};
type AppStateInteractions = {name: string, value:any}[];
type AppState = {
    fsBrowserVisible: boolean;
    interactions: AppStateInteractions;
    editorContents: string;
};

function makeResult(result: any): {name: string, value: any}[] {
    return Object.keys(result).sort().map((key) => {
        return {
            name: key,
            value: result[key]
        }
    });
}

type InteractionProps = {
    name: string,
    value: any
};

type InteractionState = {};

class Interaction extends React.Component<InteractionProps, InteractionState> {
    format = (name: string, value: any) => {
        return (
            <div>
                {name} = {value}
            </div>
        );
    };

    convert = (name: string, value: any) => {
        if (typeof value === 'number') {
            return this.format(name, value.toString());
        } else if (typeof value === 'string') {
            return this.format(name, `"${value}"`);
        } else if (typeof value === 'boolean') {
            return this.format(name, value.toString());
        } else if (typeof value === 'function') {
            // TODO(michael) can we display more info than just <function> ?
            return this.format(name, "<function>");
        } else if (typeof value === 'object') {
            // TODO(michael) palceholder for better object display
            return this.format(name, JSON.stringify(value));
        }
    };

    render() {
        return (
            <div>
                {this.convert(this.props.name, this.props.value)}
            </div>
        )
    };
}

type EditorProps = {
    fs: any;
    openFilePath: string;
    contents: string;
    worker: Worker;
    runner: any;
};

type EditorState = {
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

        this.props.worker.onmessage =
            control.backend.makeBackendMessageHandler(
                console.log,
                console.log,
                () => { return; },
                () => {
                    this.pyretRun((runResult) => {
                        this.setState(
                            {
                                interactions: makeResult(runResult.result)
                            }
                        );
                    });
                });

        this.state = {
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

    pyretRun = (callback: (result: any) => void): void => {
        control.backend.runProgram(
            this.props.runner,
            control.path.compiledProject,
            control.path.programCacheJS,
            control.backend.RunKind.Sync)
               .catch(callback)
               .then(callback);
    };

    static isPyretFile(path: string) {
        return /\.arr$/.test(path);
    }

    run = () => {
        if (Editor.isPyretFile(this.state.openFilePath)) {

            control.backend.compileProgram(
                this.props.worker,
                {
                    "program": this.state.openFilePath,
                    "baseDir": control.path.root,
                    "builtinJSDir": control.path.prewritten,
                    "checks": "none",
                    "typeCheck": true
                });
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
        this.props.fs.writeFileSync(this.state.openFilePath, e.target.value);
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
        const stats = this.props.fs.statSync(this.fullPathTo(child));

        if (stats.isDirectory()) {
            this.traverseDown(child);
        } else if (stats.isFile()) {
            this.setState({
                interactions: [{
                    name: "Note",
                    value: "Press Run to compile and run"
                }],
                openFilePath: this.fullPathTo(child),
                contents: this.props.fs.readFileSync(this.fullPathTo(child))
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
                    <button id="run"
                            className="prose"
                            onClick={this.run}>
                        Run
                    </button>
                    <button id="open-fs"
                            className="prose"
                            onClick={this.toggleFSBrowser}>
                        File System
                    </button>
                    <button id="load-builtins"
                            className="prose"
                            onClick={this.loadBuiltins}>
                        Load Builtins
                    </button>
                    <button id="remove-root"
                            className="prose"
                            onClick={this.removeRootDirectory}>
                        Remove Root
                    </button>
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
                                        this.props.fs
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
    constructor(props: AppProps) {
        super(props);
        this.state = {
            fsBrowserVisible: false,
            interactions: [],
            editorContents: App.openOrCreateFile(control.fs, control.path.programCache)
        };
    };

    static openOrCreateFile(fs: any, path: string): string {
        if (fs.existsSync(path)) {
            return fs.readFileSync(path);
        } else {
            fs.writeFileSync(path, "");
            return "";
        }
    }

    render() {
        return (
            <Editor fs={control.fs}
                    openFilePath={control.path.programCache}
                    contents={this.state.editorContents}
                    worker={control.worker}
                    runner={control.runner} >
            </Editor>
        );
    };
}

export default App;
