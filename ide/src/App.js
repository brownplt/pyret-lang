import React from 'react';
import './App.css';
const BrowserFS = require('browserfs');

BrowserFS.install(window);

BrowserFS.configure(
    {
        fs: "LocalStorage"
    },
    function(e) {
        if (e) {
            throw e;
        }
    }
);

const fs = BrowserFS.BFSRequire('fs');

const programCacheFile = '/program-cache.arr';

function pyretCompile(path, callback) {

    // We don't have the infrastructure in place to compile or run Pyret programs at the
    // moment, so just echo back the path of the file as a placeholder.

    callback({path: path});
}

function pyretRun(compileSuccess, callback) {
    const contents = fs.readFileSync(programCacheFile, {encoding: 'utf-8'});

    // We don't have the infrastructure in place to compile or run Pyret programs at the
    // moment, so just echo back the contents of the file as a placeholder.

    callback({answer: {stringContents: contents}});
}

class DefinitionsArea extends React.Component {
    constructor(props) {
        super(props);

        if (!fs.existsSync(programCacheFile)) {
            fs.writeFileSync(programCacheFile, "provide *");
        }

        this.state = {
            value: fs.readFileSync(programCacheFile)
        };
    };

    saveDefinitions = (e) => {
        this.setState({value: e.target.value});
        fs.writeFileSync(programCacheFile, e.target.value);
    };

    render() {
        return (
            <textarea id="definitions-area"
                      value={this.state.value}
                      className="code"
                      onChange={this.saveDefinitions}>
            </textarea>
        );
    };
}

function isCompileSuccess(x) {
    if (x.path) {
        return true;
    }

    return false;
}

function isRunSuccess(x) {
    if (x.answer) {
        return true;
    }

    return false;
}

class App extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            interactions: ""
        };
    };

    run = () => {
        pyretCompile(
            programCacheFile,
            (compileResult) => {
                if (isCompileSuccess(compileResult)) {
                    pyretRun(
                        compileResult,
                        (runResult) => {
                            if (isRunSuccess(runResult)) {
                                this.setState(
                                    {
                                        interactions: runResult.answer.stringContents
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
                </div>
                <div id="main">
                    <div id="edit-box">
                        <div id="definitions-container">
                            <DefinitionsArea />
                        </div>
                        <div id="separator" >
                        </div>
                        <div id="interactions-container">
                            <pre id="interactions-area"
                                 className="code">
                                {this.state.interactions}
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
