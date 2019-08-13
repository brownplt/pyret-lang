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

class DefinitionsArea extends React.Component {
    constructor(props) {
        super(props);

        if (!fs.existsSync('/program-cache.arr')) {
            fs.writeFileSync('/program-cache.arr', "provide *");
        }

        this.state = {
            value: fs.readFileSync('/program-cache.arr')
        }
    }

    saveDefinitions = (e) => {
        this.setState({value: e.target.value});
        fs.writeFileSync('/program-cache.arr', e.target.value);
    }

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

function App() {
    return (
        <div id="outer-box">
            <div id="header">
                <button id="run" className="prose">
                    Run
                </button>
            </div>
            <div id="main">
                <div id="edit-box">
                    <div id="definitions-container">
                        <DefinitionsArea/>
                    </div>
                    <div id="separator"></div>
                    <div id="interactions-container">
                        <pre id="interactions-area" className="code"></pre>
                    </div>
                </div>
            </div>
            <div id="footer"> </div>
        </div>
    );
}

export default App;
