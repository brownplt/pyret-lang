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

class App extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            interactions: ""
        };
    };

    run = () => {
        this.setState(
            {
                interactions: fs.readFileSync(programCacheFile, {encoding: 'utf-8'})
            }
        );
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
                        <div id="separator"></div>
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
