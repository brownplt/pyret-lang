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

type EditorProps = {
    fs: any;
    path: string;
};

type EditorState = {
    contents: string;
};

class Editor extends React.Component<EditorProps, EditorState> {
    constructor(props: EditorProps) {
        super(props);

        this.state = {
            contents: this.openOrCreate()
        }
    };

    openOrCreate = () => {
        if (this.props.fs.existsSync(this.props.path)) {
            return this.props.fs.readFileSync(this.props.path);
        } else {
            this.props.fs.writeFileSync(this.props.path, "");
            return "";
        }
    };

    autosave = (e: React.ChangeEvent<HTMLTextAreaElement>) => {
        this.setState({contents: e.target.value});
        this.props.fs.writeFileSync(this.props.path, e.target.value);
    };

    render() {
        return (
            <textarea className="editor"
                      value={this.state.contents}
                      onChange={this.autosave}>
            </textarea>
        );
    };
}

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

type FSBrowserProps = {
    fs: any;
};

type FSBrowserState = {
    currentDirectory: string;
    parentDirectory: string | false;
};

class FSBrowser extends React.Component<FSBrowserProps, FSBrowserState> {
    constructor(props: FSBrowserProps) {
        super(props);

        this.state = {
            currentDirectory: "/",
            parentDirectory: false
        };
    };

    changeDirectory = (newDirectory: string) => {
        if (newDirectory === "/") {
            this.setState({
                currentDirectory: newDirectory,
                parentDirectory: false
            });
        } else {
            this.setState({
                currentDirectory: newDirectory,
                parentDirectory: this.state.currentDirectory
            });
        }
    };

    render() {
        return (
            <ul id="fs-browser">
                {this.state.parentDirectory ? (
                    <li onClick={() => {
                        if (this.state.parentDirectory) {
                            this.changeDirectory(this.state.parentDirectory);
                        }
                    }}
                        className="fs-browser-item">
                        ..
                    </li>
                ) : (
                    null
                )}
                {
                    this.props.fs
                        .readdirSync(this.state.currentDirectory)
                        .map((file: string) => {
                            return (
                                <li onClick={() => this.changeDirectory(file)}
                                    className="fs-browser-item">
                                    {file}
                                </li>
                            );
                        })
                }
            </ul>
        );
    }
}

class App extends React.Component<AppProps, AppState> {
    constructor(props: AppProps) {
        super(props);
        this.state = {
            fsBrowserVisible: false,
            interactions: []
        };
    };

    toggleFSBrowser = () => {
        this.setState({
            fsBrowserVisible: !this.state.fsBrowserVisible
        });
    };

    run = () => {
        pyretCompile(
            programCacheFile,
            (compileResult) => {
                if (isCompileSuccess(compileResult)) {
                    pyretRun(
                        compileResult,
                        (runResult) => {
                            //if (isRunSuccess(runResult)) {
                            if (isRunSuccess(mockRunResult)) {
                                this.setState(
                                    {
                                        //interactions: runResult.answer.stringContents
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
    };

    render() {
        return (
            <div id="outer-box">
                <div id="header">
                    <button id="open-fs"
                            className="prose"
                            onClick={this.toggleFSBrowser}>
                        File System
                    </button>
                    <button id="run"
                            className="prose"
                            onClick={this.run}>
                        Run
                    </button>
                </div>
                <div id="main">
                    <div id="edit-box">
                        {this.state.fsBrowserVisible ? (
                            <FSBrowser fs={fs} />
                        ) : (
                            null
                        )}
                        <div id="definitions-container">
                            <Editor path={programCacheFile} fs={fs} />
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
                <div id="footer"> </div>
            </div>
        );
    };
}

export default App;
