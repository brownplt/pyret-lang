import React from 'react';
import './App.css';

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
                      <textarea id="definitions-area" className="code"></textarea>
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
