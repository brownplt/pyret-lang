import React from 'react';
import './App.css';
const BrowserFS = require('browserfs');

const worker = new Worker('pyret.jarr');

BrowserFS.install(window);

BrowserFS.configure(
    {
        fs: "LocalStorage"
    },
    function(e: any) {
        BrowserFS.FileSystem.WorkerFS.attachRemoteListener(worker);
        if (e) {
            throw e;
        }
    }
);

const fs = BrowserFS.BFSRequire('fs');

worker.onmessage = (e) => console.log(e.data);

const programCacheFile = '/program-cache.arr';

type CompileResult = CompileSuccess | CompileFailure;
type CompileSuccess = {
    path: string;
};
type CompileFailure = string;

function pyretCompile(path: string, callback: (result: CompileResult) => void): void {

    worker.postMessage({
        _parley: true,
        options: {
            program: 'program-cache.arr',
            "base-dir": '/',
            "builtin-js-dir": '/',
            checks: "none",
            'type-check': true,
        }
    });

    callback({path: path});
}

type RunResult = RunSuccess | RunFailure;
type RunSuccess = {
    answer: any;
};
type RunFailure = string;

function pyretRun(compileSuccess: CompileSuccess,
                  callback: (result: RunResult) => void): void {
                      const contents = fs.readFileSync(programCacheFile, {encoding: 'utf-8'});

                      // We don't have the infrastructure in place to compile or run Pyret programs at the
                      // moment, so just echo back the contents of the file as a placeholder.

                      callback({answer: {stringContents: contents}});
                  }

const mockRunOutput = {
    "a": 9,
    "b": 5,
    "c": true,
    "d": false,
    "e": "Ahoy, world!",
    "f": ((x: any) => x),
    "g": {"Ahoy": "World!"}
};

const mockRunResult: RunResult = {
    answer: mockRunOutput
};

function isCompileSuccess(x: any): x is CompileSuccess {
    if (x.path) {
        return true;
    }

    return false;
}

function isRunSuccess(x: any): x is RunSuccess {
    if (x.answer) {
        return true;
    }

    return false;
}

type AppProps = {};
type AppStateInteractions = {name: string, value:any}[];
type AppState = {
    fsBrowserVisible: boolean;
    interactions: AppStateInteractions;
    editorContents: string;
};

function makeResult(result: any): {name: string, value: any}[] {
    return Object.keys(result).map((key) => {
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

        this.state = {
            fsBrowserVisible: false,
            interactions: [],
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
            pyretCompile(
                this.state.openFilePath,
                (compileResult) => {
                    if (isCompileSuccess(compileResult)) {
                        pyretRun(
                            compileResult,
                            (runResult) => {
                                if (isRunSuccess(mockRunResult)) {
                                    this.setState(
                                        {
                                            interactions: makeResult(mockRunResult.answer)
                                        }
                                    );
                                } else {
                                    // there was an issue at run time
                                }
                            })
                    } else {
                        // there was an issue at compile time.
                    }
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
            editorContents: App.openOrCreateFile(fs, programCacheFile)
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
            <Editor fs={fs}
                    openFilePath={programCacheFile}
                    contents={this.state.editorContents} />
        );
    };
}

export default App;
