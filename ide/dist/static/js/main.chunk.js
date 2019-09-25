(window["webpackJsonpide"] = window["webpackJsonpide"] || []).push([["main"],{

/***/ "./node_modules/babel-core/lib/transformation/file sync recursive":
/*!**************************************************************!*\
  !*** ./node_modules/babel-core/lib/transformation/file sync ***!
  \**************************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

function webpackEmptyContext(req) {
	var e = new Error("Cannot find module '" + req + "'");
	e.code = 'MODULE_NOT_FOUND';
	throw e;
}
webpackEmptyContext.keys = function() { return []; };
webpackEmptyContext.resolve = webpackEmptyContext;
module.exports = webpackEmptyContext;
webpackEmptyContext.id = "./node_modules/babel-core/lib/transformation/file sync recursive";

/***/ }),

/***/ "./node_modules/babel-core/lib/transformation/file/options sync recursive":
/*!**********************************************************************!*\
  !*** ./node_modules/babel-core/lib/transformation/file/options sync ***!
  \**********************************************************************/
/*! no static exports found */
/***/ (function(module, exports) {

function webpackEmptyContext(req) {
	var e = new Error("Cannot find module '" + req + "'");
	e.code = 'MODULE_NOT_FOUND';
	throw e;
}
webpackEmptyContext.keys = function() { return []; };
webpackEmptyContext.resolve = webpackEmptyContext;
module.exports = webpackEmptyContext;
webpackEmptyContext.id = "./node_modules/babel-core/lib/transformation/file/options sync recursive";

/***/ }),

/***/ "./node_modules/css-loader/dist/cjs.js?!./node_modules/postcss-loader/src/index.js?!./src/App.css":
/*!************************************************************************************************************************!*\
  !*** ./node_modules/css-loader/dist/cjs.js??ref--6-oneOf-3-1!./node_modules/postcss-loader/src??postcss!./src/App.css ***!
  \************************************************************************************************************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

exports = module.exports = __webpack_require__(/*! ../node_modules/css-loader/dist/runtime/api.js */ "./node_modules/css-loader/dist/runtime/api.js")(false);
// Module
exports.push([module.i, "html, body, #root {\n    height: 100%;\n    margin: 0;\n}\n\n.page-container {\n    height: 100%;\n    display: flex;\n    flex-direction: column;\n}\n\n.header-container {\n    height: 2.7em;\n    background: #c8c8c8;\n    box-shadow: 0px 1px 2px rgba(0, 0, 0, 0.6);\n    flex: none;\n    position: relative;\n    z-index: 10;\n}\n\n.run-container {\n    height: 100%;\n    display: flex;\n    flex-direction: row;\n    float: right;\n}\n\n.run-options {\n    width: 1.5em;\n    text-align: center;\n    border: none;\n    font-family: sans-serif;\n    font-size: 16px;\n    text-indent: -0.2em;\n    cursor: pointer;\n    transition: box-shadow 0.2s;\n    background: #317bcf;\n    color: #fff;\n}\n\nbutton::-moz-focus-inner {\n    border: none;\n}\n\n.stop-available {\n    height: 100%;\n    cursor: pointer;\n    border: none;\n    transition: box-shadow 0.2s;\n    width: 12em;\n    float: right;\n    background: #cf3b31;\n    color: #fff;\n}\n\n.stop-unavailable {\n    height: 100%;\n    cursor: pointer;\n    border: none;\n    width: 12em;\n    float: right;\n    background: none;\n    color: rgba(0, 0, 0, 0.26);\n}\n\n.menu {\n    height: 100%;\n    cursor: pointer;\n    transition: box-shadow 0.2s;\n    width: 6em;\n    float: left;\n    background: rgba(0,0,0,0.3);\n    color: #fff;\n    border: none;\n}\n\n.run-ready {\n    height: 100%;\n    cursor: pointer;\n    transition: box-shadow 0.2s;\n    width: 12em;\n    float: right;\n    background: #317bcf;\n    color: #fff;\n    border: none;\n    border-right: 2px solid rgba(0, 0, 0, 0.26);\n}\n\n.run-queued {\n    height: 100%;\n    cursor: pointer;\n    transition: box-shadow 0.2s;\n    width: 12em;\n    float: right;\n    background: none;\n    color: rgba(0, 0, 0, 0.26);\n    border: none;\n    border-right: 2px solid rgba(0, 0, 0, 0.26);\n}\n\n.run-option-disabled {\n    height: 2.7em;\n    cursor: pointer;\n    border: none;\n    transition: box-shadow 0.2s;\n    width: 100%;\n    background: #c8c8c8;\n    color: rgba(0, 0, 0, 0.26);\n    display: flex;\n    justify-content: flex-start;\n}\n\n.run-option-label {\n    text-align: center;\n    line-height: 3em;\n    font-family: sans-serif;\n    font-size: 14px;\n    padding-left: 1em;\n    cursor: pointer;\n    -webkit-user-select: none;\n       -moz-user-select: none;\n            user-select: none;\n}\n\n.run-option-enabled {\n    height: 2.7em;\n    cursor: pointer;\n    border: none;\n    transition: box-shadow 0.2s;\n    width: 100%;\n    background: #317bcf;\n    color: #fff;\n    display: flex;\n    justify-content: flex-start;\n    text-align: center;\n}\n\n.run-option-enabled:focus, .run-option-disabled:focus, .run-ready:focus, .run-queued:focus, .run-options:focus, .stop-available:focus, .stop-unavailable:focus, .menu:focus, .menu-content-button:focus {\n    outline: none;\n}\n\n.interaction-error {\n    background: rgb(255, 202, 202);\n    height: 100%;\n}\n\n.run-option-enabled:hover, .run-ready:hover, .run-options:hover {\n    box-shadow: 0px -5px 10px 5px rgba(0, 0, 0, 0.2);\n    background: #0060ce;\n}\n\n.stop-available:hover {\n    box-shadow: 0px -5px 10px 5px rgba(0, 0, 0, 0.2);\n    background: #b30c00;\n}\n\n.run-option-disabled:hover, .run-queued:hover, .menu:hover, .menu-content-button:hover, .fs-browser-item:hover, .font-label:hover, .font-minus:hover, .font-plus:hover {\n    box-shadow: 0px -5px 10px 5px rgba(0, 0, 0, 0.2);\n    background: rgba(0, 0, 0, 0.6);\n}\n\n.run-option-checkbox {\n    margin: 0 0 0 1em;\n    margin-top: 1em;\n    padding: 0;\n    float: left;\n    cursor: pointer;\n    width: 1em;\n    height: 1em;\n}\n\n.code-container {\n    height: calc(100% - 5.4em);\n    width: 100%;\n    display: flex;\n    flex-direction: row;\n}\n\n.edit-area-container {\n    height: 100%;\n}\n\n.splitter-layout {\n    position: relative !important;\n}\n\n.splitter-layout > .layout-splitter {\n    width: 0.5em !important;\n    border: 1px solid #a6a6a6;\n    background: linear-gradient(to right, #bfbfbf 0%, #d9d9d9 40%, #d9d9d9 60%, #bfbfbf 100%);\n}\n\n.splitter-layout.splitter-layout-vertical > .layout-splitter {\n    width: auto !important;\n    height: 0.5em !important;\n    background: linear-gradient(to top, #bfbfbf 0%, #d9d9d9 40%, #d9d9d9 60%, #bfbfbf 100%);\n}\n\n.footer-container {\n    height: 2.7em;\n    background: #111;\n    flex: none;\n    color: #fff;\n    display: flex;\n    align-items: center;\n}\n\n.run-dropdown {\n    position: absolute;\n    right: 10em;\n    top: 2.7em;\n    width: 11.5em;\n    display: flex;\n    flex-direction: column;\n    z-index: 1;\n    border-top: 1px solid rgba(0, 0, 0, 1);\n}\n\n.react-codemirror2 {\n    height: 100%;\n}\n\n.CodeMirror {\n    min-height: 100% !important;\n    border: none !important;\n}\n\n.interactions-area-container {\n    height: 100%;\n    display: flex;\n    flex-flow: column;\n}\n\n.interactions-area {\n    font-family: monospace;\n    margin: 0;\n    padding: 0;\n    margin-bottom: 1em;\n    overflow-y: scroll;\n}\n\n.interaction {\n    display: flex;\n    flex-direction: row;\n    padding: 1em;\n    padding-bottom: 0;\n}\n\n.interaction-identifier {\n    margin: 0;\n    display: flex;\n    align-items: center;\n}\n\n.interaction-error {\n    background: rgb(255, 202, 202);\n}\n\n.menu-content-button {\n    border: none;\n    height: 2.7em;\n    background: rgba(0, 0, 0, 0.3);\n    color: #fff;\n}\n\n.fs-browser-item {\n    border: none;\n    height: 2.7em;\n    background: rgba(0, 0, 0, 0.3);\n    color: #fff;\n    text-align: left;\n    flex: none;\n}\n\n.font-size-options {\n    display: flex;\n    flex-direction: row;\n}\n\n.font-label {\n    flex-grow: 2;\n    border: none;\n    background: rgba(0, 0, 0, 0.3);\n    color: #fff;\n}\n\n.font-minus {\n    border: none;\n    width: 2.7em;\n    background: rgba(0, 0, 0, 0.3);\n    color: #fff;\n}\n\n.font-plus {\n    border: none;\n    width: 2.7em;\n    background: rgba(0, 0, 0, 0.3);\n    color: #fff;\n}\n\n.font-size-options {\n    height: 2.7em;\n}\n\n.table-copy {\n    cursor: pointer;\n    position: absolute;\n    padding: 0.5em;\n    z-index: 1;\n}\n\n.table-container .table-copy {\n    opacity: 0;\n}\n\n.table-container:hover .table-copy {\n    opacity: 1;\n}\n\n.styled-background {\n    background-color: rgb(134, 134, 245);\n}\n\n.styled-background-error {\n    background-color: rgb(255, 202, 202);\n}\n\n.ReactTable {\n    overflow: auto;\n}\n\n.menu-container {\n    display: flex;\n    flex-direction: row;\n    background: #c8c8c8;\n}\n\n.menu-tab-inactive {\n    font-size: 2em;\n    padding: 0.25em;\n}\n\n.menu-tab-active {\n    font-size: 2em;\n    padding: 0.25em;\n    background: rgba(0, 0, 0, 0.6);\n}\n\n.menu-content {\n    background: #c8c8c8;\n    height: 100%;\n    width: 16em;\n    display: flex;\n    flex-direction: column;\n    box-shadow: 1px 0px 2px rgba(0, 0, 0, 0.6);\n    position: relative;\n    z-index: 5;\n    overflow-y: scroll;\n}\n", ""]);



/***/ }),

/***/ "./node_modules/css-loader/dist/cjs.js?!./node_modules/postcss-loader/src/index.js?!./src/index.css":
/*!**************************************************************************************************************************!*\
  !*** ./node_modules/css-loader/dist/cjs.js??ref--6-oneOf-3-1!./node_modules/postcss-loader/src??postcss!./src/index.css ***!
  \**************************************************************************************************************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

exports = module.exports = __webpack_require__(/*! ../node_modules/css-loader/dist/runtime/api.js */ "./node_modules/css-loader/dist/runtime/api.js")(false);
// Module
exports.push([module.i, "", ""]);



/***/ }),

/***/ "./src/App.css":
/*!*********************!*\
  !*** ./src/App.css ***!
  \*********************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

var content = __webpack_require__(/*! !../node_modules/css-loader/dist/cjs.js??ref--6-oneOf-3-1!../node_modules/postcss-loader/src??postcss!./App.css */ "./node_modules/css-loader/dist/cjs.js?!./node_modules/postcss-loader/src/index.js?!./src/App.css");

if (typeof content === 'string') {
  content = [[module.i, content, '']];
}

var options = {}

options.insert = "head";
options.singleton = false;

var update = __webpack_require__(/*! ../node_modules/style-loader/dist/runtime/injectStylesIntoStyleTag.js */ "./node_modules/style-loader/dist/runtime/injectStylesIntoStyleTag.js")(content, options);

if (content.locals) {
  module.exports = content.locals;
}

if (true) {
  if (!content.locals) {
    module.hot.accept(
      /*! !../node_modules/css-loader/dist/cjs.js??ref--6-oneOf-3-1!../node_modules/postcss-loader/src??postcss!./App.css */ "./node_modules/css-loader/dist/cjs.js?!./node_modules/postcss-loader/src/index.js?!./src/App.css",
      function () {
        var newContent = __webpack_require__(/*! !../node_modules/css-loader/dist/cjs.js??ref--6-oneOf-3-1!../node_modules/postcss-loader/src??postcss!./App.css */ "./node_modules/css-loader/dist/cjs.js?!./node_modules/postcss-loader/src/index.js?!./src/App.css");

        if (typeof newContent === 'string') {
          newContent = [[module.i, newContent, '']];
        }
        
        update(newContent);
      }
    )
  }

  module.hot.dispose(function() { 
    update();
  });
}

/***/ }),

/***/ "./src/App.tsx":
/*!*********************!*\
  !*** ./src/App.tsx ***!
  \*********************/
/*! exports provided: default */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! react */ "./node_modules/react/index.js");
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_0__);
/* harmony import */ var _App_css__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./App.css */ "./src/App.css");
/* harmony import */ var _App_css__WEBPACK_IMPORTED_MODULE_1___default = /*#__PURE__*/__webpack_require__.n(_App_css__WEBPACK_IMPORTED_MODULE_1__);
/* harmony import */ var _Interaction__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ./Interaction */ "./src/Interaction.tsx");
/* harmony import */ var _DefChunks__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ./DefChunks */ "./src/DefChunks.tsx");
/* harmony import */ var _SingleCodeMirrorDefinitions__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ./SingleCodeMirrorDefinitions */ "./src/SingleCodeMirrorDefinitions.tsx");
/* harmony import */ var _Menu__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! ./Menu */ "./src/Menu.tsx");
/* harmony import */ var _Footer__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! ./Footer */ "./src/Footer.tsx");
/* harmony import */ var _FontSize__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! ./FontSize */ "./src/FontSize.tsx");
/* harmony import */ var _FSBrowser__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! ./FSBrowser */ "./src/FSBrowser.tsx");
/* harmony import */ var _Dropdown__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! ./Dropdown */ "./src/Dropdown.tsx");
/* harmony import */ var _Header__WEBPACK_IMPORTED_MODULE_10__ = __webpack_require__(/*! ./Header */ "./src/Header.tsx");
/* harmony import */ var _InteractionError__WEBPACK_IMPORTED_MODULE_11__ = __webpack_require__(/*! ./InteractionError */ "./src/InteractionError.tsx");
/* harmony import */ var _control__WEBPACK_IMPORTED_MODULE_12__ = __webpack_require__(/*! ./control */ "./src/control.ts");
/* harmony import */ var react_splitter_layout__WEBPACK_IMPORTED_MODULE_13__ = __webpack_require__(/*! react-splitter-layout */ "./node_modules/react-splitter-layout/lib/index.js");
/* harmony import */ var react_splitter_layout__WEBPACK_IMPORTED_MODULE_13___default = /*#__PURE__*/__webpack_require__.n(react_splitter_layout__WEBPACK_IMPORTED_MODULE_13__);
/* harmony import */ var react_splitter_layout_lib_index_css__WEBPACK_IMPORTED_MODULE_14__ = __webpack_require__(/*! react-splitter-layout/lib/index.css */ "./node_modules/react-splitter-layout/lib/index.css");
/* harmony import */ var react_splitter_layout_lib_index_css__WEBPACK_IMPORTED_MODULE_14___default = /*#__PURE__*/__webpack_require__.n(react_splitter_layout_lib_index_css__WEBPACK_IMPORTED_MODULE_14__);
var _jsxFileName = "/mnt/c/users/andro/Desktop/cs5152/pyret-lang/ide/src/App.tsx";















_control__WEBPACK_IMPORTED_MODULE_12__["installFileSystem"]();
_control__WEBPACK_IMPORTED_MODULE_12__["loadBuiltins"]();
var EEditor;

(function (EEditor) {
  EEditor[EEditor["Chunks"] = 0] = "Chunks";
  EEditor[EEditor["Text"] = 1] = "Text";
})(EEditor || (EEditor = {}));

function makeResult(result, compiledJSONPath) {
  const programJSON = JSON.parse(_control__WEBPACK_IMPORTED_MODULE_12__["bfsSetup"].fs.readFileSync(compiledJSONPath));
  const providedValues = programJSON.provides.values;
  const providedValuesKeys = Object.keys(programJSON.provides.values);

  const insertLineNumber = key => {
    const column = providedValues[key].origin["local-bind-site"][1];
    return {
      line: column,
      name: key,
      value: result[key]
    };
  };

  const compareResults = (a, b) => {
    if (a.line < b.line) {
      return -1;
    } else if (a.line > b.line) {
      return 1;
    } else {
      return 0;
    }
  };

  if (providedValuesKeys.length !== 0) {
    // we have source location information for bindings, so we sort them
    // based on which column they are bound on
    return providedValuesKeys.map(insertLineNumber).sort(compareResults);
  } else {
    // we do not have source location information for bindings, so we sort
    // them alphabetically by identifier name
    return Object.keys(result).sort().map(key => {
      return {
        name: key,
        value: result[key]
      };
    });
  }
}

class Editor extends react__WEBPACK_IMPORTED_MODULE_0___default.a.Component {
  constructor(props) {
    super(props);

    this.run = () => {
      this.setState({
        interactionErrors: [],
        interactErrorExists: false
      });

      if (this.isPyretFile) {
        this.setMessage("Compilation started");
        _control__WEBPACK_IMPORTED_MODULE_12__["compile"](this.currentFileDirectory, this.currentFileName, this.state.typeCheck);
      } else {
        this.setMessage("Visited a non-pyret file");
        this.setState({
          interactions: [{
            name: "Error",
            value: "Run is not supported on this file type"
          }, {
            name: "File",
            value: this.currentFile
          }],
          interactionErrors: ["Error: Run is not supported on this file type"],
          interactErrorExists: true
        });
      }
    };

    this.update = () => {
      _control__WEBPACK_IMPORTED_MODULE_12__["fs"].writeFileSync(this.currentFile, this.state.currentFileContents);

      if (this.state.autoRun) {
        this.run();
      }
    };

    this.onEdit = value => {
      clearTimeout(this.state.updateTimer);
      this.setState({
        currentFileContents: value,
        updateTimer: setTimeout(this.update, 250)
      });
    };

    this.onTraverseDown = path => {
      this.setState({
        browsePath: path
      });
    };

    this.onTraverseUp = path => {
      this.setState({
        browsePath: path
      });
    };

    this.onExpandChild = (child, fullChildPath) => {
      this.setState({
        interactions: [{
          name: "Note",
          value: "Press Run to compile and run"
        }],
        currentFileDirectory: this.state.browsePath,
        currentFileName: child,
        currentFileContents: _control__WEBPACK_IMPORTED_MODULE_12__["fs"].readFileSync(fullChildPath, "utf-8")
      });
    };

    this.setEditorMode = editorMode => {
      this.setState({
        editorMode
      });
    };

    this.loadBuiltins = e => {
      _control__WEBPACK_IMPORTED_MODULE_12__["loadBuiltins"]();
    };

    this.removeRootDirectory = e => {
      _control__WEBPACK_IMPORTED_MODULE_12__["removeRootDirectory"]();
    };

    this.makeHeaderButton = (text, enabled, onClick) => {
      return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("button", {
        className: enabled ? "run-option-enabled" : "run-option-disabled",
        onClick: onClick,
        __source: {
          fileName: _jsxFileName,
          lineNumber: 338
        },
        __self: this
      }, text);
    };

    this.toggleDropdownVisibility = e => {
      this.setState({
        dropdownVisible: !this.state.dropdownVisible
      });
    };

    this.toggleAutoRun = () => {
      this.setState({
        autoRun: !this.state.autoRun
      });
    };

    this.toggleStopify = () => {
      if (this.stopify) {
        this.setState({
          runKind: _control__WEBPACK_IMPORTED_MODULE_12__["backend"].RunKind.Sync
        });
      } else {
        this.setState({
          runKind: _control__WEBPACK_IMPORTED_MODULE_12__["backend"].RunKind.Async
        });
      }
    };

    this.toggleTypeCheck = () => {
      this.setState({
        typeCheck: !this.state.typeCheck
      });
    };

    this.onDecreaseFontSize = () => {
      if (this.state.fontSize > 1) {
        this.setState({
          fontSize: this.state.fontSize - 1
        });
      }
    };

    this.onIncreaseFontSize = () => {
      this.setState({
        fontSize: this.state.fontSize + 1
      });
    };

    this.onResetFontSize = () => {
      this.setState({
        fontSize: 12
      });
    };

    this.removeDropdown = () => {
      this.setState({
        dropdownVisible: false
      });
    };

    this.setMessage = newMessage => {
      this.setState({
        message: newMessage
      });
    };

    const onLintFailure = lintFailure => {
      let newFailures = this.state.lintFailures;
      const name = lintFailure.name;
      newFailures[name] = lintFailure;
      this.setState({
        lintFailures: newFailures
      });
    };

    const onLintSuccess = lintSuccess => {
      let newFailures = this.state.lintFailures;
      const name = lintSuccess.name;

      if (name in newFailures) {
        delete newFailures[name];
      }

      this.setState({
        lintFailures: newFailures
      });
    };

    _control__WEBPACK_IMPORTED_MODULE_12__["setupWorkerMessageHandler"](console.log, errors => {
      this.setMessage("Compilation failed with error(s)");
      const places = [];

      for (let i = 0; i < errors.length; i++) {
        const matches = errors[i].match(/:\d+:\d+-\d+:\d+/g);

        if (matches !== null) {
          matches.forEach(m => {
            places.push(m.match(/\d+/g).map(Number));
          });
        }
      }

      this.setState({
        interactionErrors: errors,
        interactErrorExists: true,
        definitionsHighlights: places
      });
    }, errors => {
      this.setState({
        interactionErrors: [errors.toString()],
        interactErrorExists: true
      });
    }, onLintFailure, onLintSuccess, () => {
      this.setMessage("Run started");
      _control__WEBPACK_IMPORTED_MODULE_12__["run"](_control__WEBPACK_IMPORTED_MODULE_12__["path"].runBase, _control__WEBPACK_IMPORTED_MODULE_12__["path"].runProgram, runResult => {
        console.log(runResult);

        if (runResult.result !== undefined) {
          if (runResult.result.error === undefined) {
            this.setMessage("Run completed successfully");
            const results = makeResult(runResult.result, _control__WEBPACK_IMPORTED_MODULE_12__["bfsSetup"].path.join(_control__WEBPACK_IMPORTED_MODULE_12__["path"].runBase, "".concat(this.state.currentFileName, ".json")));
            this.setState({
              interactions: results
            });

            if (results[0].name === "error") {
              this.setState({
                interactionErrors: runResult.result.error,
                interactErrorExists: true
              });
            }
          } else {
            this.setMessage("Run failed with error(s)");
            this.setState({
              interactionErrors: [runResult.result.error],
              interactErrorExists: true
            });
          }
        }
      }, this.state.runKind);
    });
    this.state = {
      browseRoot: this.props.browseRoot,
      browsePath: this.props.browsePath,
      currentFileDirectory: this.props.currentFileDirectory,
      currentFileName: this.props.currentFileName,
      currentFileContents: _control__WEBPACK_IMPORTED_MODULE_12__["openOrCreateFile"](_control__WEBPACK_IMPORTED_MODULE_12__["bfsSetup"].path.join(...this.props.currentFileDirectory, this.props.currentFileName)),
      typeCheck: true,
      interactions: [{
        name: "Note",
        value: "Press Run to compile and run"
      }],
      interactionErrors: [],
      interactErrorExists: false,
      lintFailures: {},
      runKind: _control__WEBPACK_IMPORTED_MODULE_12__["backend"].RunKind.Async,
      autoRun: true,
      updateTimer: setTimeout(this.update, 2000),
      dropdownVisible: false,
      editorMode: EEditor.Text,
      fontSize: 12,
      message: "Ready to rock",
      definitionsHighlights: [],
      fsBrowserVisible: false
    };
  }

  get isPyretFile() {
    return /\.arr$/.test(this.currentFile);
  }

  get currentFile() {
    return _control__WEBPACK_IMPORTED_MODULE_12__["bfsSetup"].path.join(...this.state.currentFileDirectory, this.state.currentFileName);
  }

  get currentFileName() {
    return this.state.currentFileName;
  }

  get currentFileDirectory() {
    return _control__WEBPACK_IMPORTED_MODULE_12__["bfsSetup"].path.join(...this.state.currentFileDirectory);
  }

  get stopify() {
    return this.state.runKind === _control__WEBPACK_IMPORTED_MODULE_12__["backend"].RunKind.Async;
  }

  makeDefinitions() {
    if (this.state.editorMode === EEditor.Text) {
      return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_SingleCodeMirrorDefinitions__WEBPACK_IMPORTED_MODULE_4__["SingleCodeMirrorDefinitions"], {
        text: this.state.currentFileContents,
        onEdit: this.onEdit,
        highlights: this.state.definitionsHighlights,
        interactErrorExists: this.state.interactErrorExists,
        __source: {
          fileName: _jsxFileName,
          lineNumber: 409
        },
        __self: this
      });
    } else if (this.state.editorMode === EEditor.Chunks) {
      return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_DefChunks__WEBPACK_IMPORTED_MODULE_3__["DefChunks"], {
        lintFailures: this.state.lintFailures,
        name: this.state.currentFileName,
        highlights: this.state.definitionsHighlights,
        interactErrorExists: this.state.interactErrorExists,
        program: this.state.currentFileContents,
        onEdit: this.onEdit,
        __source: {
          fileName: _jsxFileName,
          lineNumber: 417
        },
        __self: this
      });
    }
  }

  render() {
    const interactionValues = react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("pre", {
      className: "interactions-area",
      style: {
        fontSize: this.state.fontSize
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 429
      },
      __self: this
    }, this.state.interactions.map(i => {
      return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_Interaction__WEBPACK_IMPORTED_MODULE_2__["Interaction"], {
        key: i.name,
        name: i.name,
        value: i.value,
        setMessage: this.setMessage,
        __source: {
          fileName: _jsxFileName,
          lineNumber: 434
        },
        __self: this
      });
    }));
    const dropdown = this.state.dropdownVisible && react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_Dropdown__WEBPACK_IMPORTED_MODULE_9__["Dropdown"], {
      __source: {
        fileName: _jsxFileName,
        lineNumber: 443
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_Dropdown__WEBPACK_IMPORTED_MODULE_9__["DropdownOption"], {
      enabled: this.state.autoRun,
      onClick: this.toggleAutoRun,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 444
      },
      __self: this
    }, "Auto Run"), react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_Dropdown__WEBPACK_IMPORTED_MODULE_9__["DropdownOption"], {
      enabled: this.stopify,
      onClick: this.toggleStopify,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 448
      },
      __self: this
    }, "Stopify"), react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_Dropdown__WEBPACK_IMPORTED_MODULE_9__["DropdownOption"], {
      enabled: this.state.typeCheck,
      onClick: this.toggleTypeCheck,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 452
      },
      __self: this
    }, "Type Check"));
    const fsBrowser = react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_FSBrowser__WEBPACK_IMPORTED_MODULE_8__["FSBrowser"], {
      root: this.state.browseRoot,
      onTraverseUp: this.onTraverseUp,
      onTraverseDown: this.onTraverseDown,
      onExpandChild: this.onExpandChild,
      browsePath: this.state.browsePath,
      key: "FSBrowser",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 459
      },
      __self: this
    });
    const fontSize = react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_FontSize__WEBPACK_IMPORTED_MODULE_7__["FontSize"], {
      onIncrease: this.onIncreaseFontSize,
      onDecrease: this.onDecreaseFontSize,
      onReset: this.onResetFontSize,
      size: this.state.fontSize,
      key: "FontSize",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 468
      },
      __self: this
    });
    const textEditor = react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("button", {
      className: "text-editor",
      onClick: () => this.setEditorMode(EEditor.Text),
      key: "TextEditor",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 476
      },
      __self: this
    }, "Text");
    const chunkEditor = react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("button", {
      className: "chunk-editor",
      onClick: () => this.setEditorMode(EEditor.Chunks),
      key: "ChunkEditor",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 483
      },
      __self: this
    }, "Chunks");
    const builtinsLoader = react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("button", {
      onClick: _control__WEBPACK_IMPORTED_MODULE_12__["loadBuiltins"],
      __source: {
        fileName: _jsxFileName,
        lineNumber: 490
      },
      __self: this
    }, "Load Builtins");
    const menu = react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_Menu__WEBPACK_IMPORTED_MODULE_5__["Menu"], {
      __source: {
        fileName: _jsxFileName,
        lineNumber: 495
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_Menu__WEBPACK_IMPORTED_MODULE_5__["Tab"], {
      name: "\uD83D\uDCC1",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 496
      },
      __self: this
    }, fsBrowser), react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_Menu__WEBPACK_IMPORTED_MODULE_5__["Tab"], {
      name: "\u2699",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 499
      },
      __self: this
    }, textEditor, chunkEditor, builtinsLoader, fontSize));
    const rightHandSide = react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: "interactions-area-container",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 508
      },
      __self: this
    }, this.state.interactErrorExists ? react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(react_splitter_layout__WEBPACK_IMPORTED_MODULE_13___default.a, {
      vertical: true,
      percentage: true,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 510
      },
      __self: this
    }, interactionValues, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_InteractionError__WEBPACK_IMPORTED_MODULE_11__["InteractionError"], {
      fontSize: this.state.fontSize,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 513
      },
      __self: this
    }, this.state.interactionErrors)) : interactionValues);
    const definitions = this.makeDefinitions();
    return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: "page-container",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 523
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_Header__WEBPACK_IMPORTED_MODULE_10__["Header"], {
      __source: {
        fileName: _jsxFileName,
        lineNumber: 524
      },
      __self: this
    }, this.stopify ? react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("button", {
      className: "stop-available",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 526
      },
      __self: this
    }, "Stop") : react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("button", {
      className: "stop-unavailable",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 530
      },
      __self: this
    }, "Stop"), react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: "run-container",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 534
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("button", {
      className: "run-ready",
      onClick: this.run,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 535
      },
      __self: this
    }, "Run"), react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("button", {
      className: "run-options",
      onClick: this.toggleDropdownVisibility,
      onBlur: this.removeDropdown,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 539
      },
      __self: this
    }, "\u21B4", dropdown))), react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: "code-container",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 545
      },
      __self: this
    }, menu, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(react_splitter_layout__WEBPACK_IMPORTED_MODULE_13___default.a, {
      vertical: false,
      percentage: true,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 547
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: "edit-area-container",
      style: {
        fontSize: this.state.fontSize
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 549
      },
      __self: this
    }, definitions), rightHandSide)), react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_Footer__WEBPACK_IMPORTED_MODULE_6__["Footer"], {
      message: this.state.message,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 556
      },
      __self: this
    }));
  }

}

class App extends react__WEBPACK_IMPORTED_MODULE_0___default.a.Component {
  render() {
    return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(Editor, {
      browseRoot: "/",
      browsePath: ["/", "projects"],
      currentFileDirectory: ["/", "projects"],
      currentFileName: "program.arr",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 565
      },
      __self: this
    });
  }

}

/* harmony default export */ __webpack_exports__["default"] = (App);

/***/ }),

/***/ "./src/DefChunks.tsx":
/*!***************************!*\
  !*** ./src/DefChunks.tsx ***!
  \***************************/
/*! exports provided: DefChunk, DefChunks */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "DefChunk", function() { return DefChunk; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "DefChunks", function() { return DefChunks; });
/* harmony import */ var _mnt_c_users_andro_Desktop_cs5152_pyret_lang_ide_node_modules_babel_runtime_helpers_esm_slicedToArray__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./node_modules/@babel/runtime/helpers/esm/slicedToArray */ "./node_modules/@babel/runtime/helpers/esm/slicedToArray.js");
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! react */ "./node_modules/react/index.js");
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_1___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_1__);
/* harmony import */ var react_beautiful_dnd__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! react-beautiful-dnd */ "./node_modules/react-beautiful-dnd/dist/react-beautiful-dnd.esm.js");
/* harmony import */ var react_codemirror2__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! react-codemirror2 */ "./node_modules/react-codemirror2/index.js");
/* harmony import */ var react_codemirror2__WEBPACK_IMPORTED_MODULE_3___default = /*#__PURE__*/__webpack_require__.n(react_codemirror2__WEBPACK_IMPORTED_MODULE_3__);
/* harmony import */ var _control__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ./control */ "./src/control.ts");

var _jsxFileName = "/mnt/c/users/andro/Desktop/cs5152/pyret-lang/ide/src/DefChunks.tsx";




class DefChunk extends react__WEBPACK_IMPORTED_MODULE_1___default.a.Component {
  constructor(props) {
    super(props);
    this.state = {
      editor: null,
      updateTimer: setTimeout(this.lint.bind(this), 0),
      focused: false
    };
  }

  componentWillReceiveProps() {
    if (this.state.editor !== null) {
      const marks = this.state.editor.getDoc().getAllMarks();
      marks.forEach(m => m.clear());
    }
  }

  componentDidUpdate() {
    if (this.state.editor !== null) {
      const marks = this.state.editor.getDoc().getAllMarks();
      marks.forEach(m => m.clear());

      if (this.props.highlights.length > 0) {
        for (let i = 0; i < this.props.highlights.length; i++) {
          this.state.editor.getDoc().markText({
            line: this.props.highlights[i][0] - 1 - this.props.startLine,
            ch: this.props.highlights[i][1]
          }, {
            line: this.props.highlights[i][2] - 1 - this.props.startLine,
            ch: this.props.highlights[i][3]
          }, {
            className: "styled-background-error"
          });
        }
      }
    }
  }

  scheduleUpdate() {
    clearTimeout(this.state.updateTimer);
    this.setState({
      updateTimer: setTimeout(this.lint.bind(this), 250)
    });
  }

  lint() {
    _control__WEBPACK_IMPORTED_MODULE_4__["lint"](this.props.chunk, this.props.name);
  }

  render() {
    let borderWidth = "1px";
    let borderColor = "#eee";
    let shadow = "";

    if (this.state.focused) {
      shadow = "3px 3px 2px #aaa";
      borderWidth = "2px";
      borderColor = "black";
    }

    if (this.props.highlights.length > 0) {
      borderColor = "red";
    }

    const border = borderWidth + " solid " + borderColor;
    return react__WEBPACK_IMPORTED_MODULE_1___default.a.createElement("div", {
      style: {
        boxShadow: shadow,
        border: border,
        "paddingTop": "0.5em",
        "paddingBottom": "0.5em"
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 64
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_1___default.a.createElement(react_codemirror2__WEBPACK_IMPORTED_MODULE_3__["UnControlled"], {
      onFocus: (_, __) => {
        if (this.props.isLast) {
          this.props.onEdit(this.props.index, "");
        }

        this.setState({
          focused: true
        });
      },
      onBlur: (_, __) => this.setState({
        focused: false
      }),
      editorDidMount: (editor, value) => {
        this.setState({
          editor: editor
        });
        const marks = editor.getDoc().getAllMarks();
        marks.forEach(m => m.clear());
        editor.setSize(null, "auto");
      },
      value: this.props.chunk,
      options: {
        mode: 'pyret',
        theme: 'default',
        lineNumbers: true,
        lineWrapping: true,
        lineNumberFormatter: l => String(l + this.props.startLine)
      },
      onChange: (editor, __, value) => {
        this.scheduleUpdate();
        return this.props.onEdit(this.props.index, value);
      },
      autoCursor: false,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 65
      },
      __self: this
    }), react__WEBPACK_IMPORTED_MODULE_1___default.a.createElement("ul", {
      __source: {
        fileName: _jsxFileName,
        lineNumber: 94
      },
      __self: this
    }, this.props.failures.map((f, ix) => react__WEBPACK_IMPORTED_MODULE_1___default.a.createElement("li", {
      key: String(ix),
      __source: {
        fileName: _jsxFileName,
        lineNumber: 95
      },
      __self: this
    }, f))));
  }

}
const CHUNKSEP = "#.CHUNK#\n";
class DefChunks extends react__WEBPACK_IMPORTED_MODULE_1___default.a.Component {
  constructor(props) {
    super(props);
    const chunkstrs = this.props.program.split(CHUNKSEP);
    const chunks = [];
    var totalLines = 0;

    for (let i = 0; i < chunkstrs.length; i += 1) {
      chunks.push({
        text: chunkstrs[i],
        id: String(i),
        startLine: totalLines
      });
      totalLines += chunkstrs[i].split("\n").length;
    }

    this.state = {
      chunkIndexCounter: chunkstrs.length,
      chunks
    };
  }

  getStartLineForIndex(chunks, index) {
    if (index === 0) {
      return 0;
    } else {
      return chunks[index - 1].startLine + chunks[index - 1].text.split("\n").length;
    }
  }

  chunksToString(chunks) {
    return chunks.map(c => c.text).join(CHUNKSEP);
  }

  render() {
    const onEdit = (index, text) => {
      let newChunks;

      if (index === this.state.chunks.length) {
        const id = String(this.state.chunkIndexCounter);
        this.setState({
          chunkIndexCounter: this.state.chunkIndexCounter + 1
        });
        newChunks = this.state.chunks.concat([{
          text,
          id,
          startLine: this.getStartLineForIndex(this.state.chunks, this.state.chunks.length)
        }]);
      } else {
        newChunks = this.state.chunks.map((p, ix) => {
          if (ix === index) {
            return {
              text,
              id: p.id,
              startLine: p.startLine
            };
          } else {
            return p;
          }
        });
        newChunks = newChunks.map((p, ix) => {
          if (ix <= index) {
            return p;
          } else {
            return {
              text: p.text,
              id: p.id,
              startLine: this.getStartLineForIndex(newChunks, ix)
            };
          }
        });
      }

      this.setState({
        chunks: newChunks
      });
      this.props.onEdit(this.chunksToString(newChunks));
    };

    const onDragEnd = result => {
      if (result.destination === null || result.source.index === result.destination.index) {
        return;
      } else {
        // Great examples! https://codesandbox.io/s/k260nyxq9v
        const reorder = (chunks, start, end) => {
          const result = Array.from(chunks);

          const _result$splice = result.splice(start, 1),
                _result$splice2 = Object(_mnt_c_users_andro_Desktop_cs5152_pyret_lang_ide_node_modules_babel_runtime_helpers_esm_slicedToArray__WEBPACK_IMPORTED_MODULE_0__["default"])(_result$splice, 1),
                removed = _result$splice2[0];

          result.splice(end, 0, removed);
          return result;
        };

        if (result.destination === undefined) {
          return;
        }

        let newChunks = reorder(this.state.chunks, result.source.index, result.destination.index);

        for (let i = 0; i < newChunks.length; i += 1) {
          const p = newChunks[i];
          newChunks[i] = {
            text: p.text,
            id: p.id,
            startLine: this.getStartLineForIndex(newChunks, i)
          };
        }

        this.setState({
          chunks: newChunks
        });
        this.props.onEdit(this.chunksToString(newChunks));
      }
    };

    const endBlankChunk = {
      text: "",
      id: String(this.state.chunkIndexCounter),
      startLine: this.getStartLineForIndex(this.state.chunks, this.state.chunks.length)
    };
    return react__WEBPACK_IMPORTED_MODULE_1___default.a.createElement(react_beautiful_dnd__WEBPACK_IMPORTED_MODULE_2__["DragDropContext"], {
      onDragEnd: onDragEnd,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 195
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_1___default.a.createElement(react_beautiful_dnd__WEBPACK_IMPORTED_MODULE_2__["Droppable"], {
      droppableId: "droppable",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 196
      },
      __self: this
    }, (provided, snapshot) => {
      return react__WEBPACK_IMPORTED_MODULE_1___default.a.createElement("div", Object.assign({}, provided.droppableProps, {
        ref: provided.innerRef,
        __source: {
          fileName: _jsxFileName,
          lineNumber: 198
        },
        __self: this
      }), this.state.chunks.concat([endBlankChunk]).map((chunk, index) => {
        const linesInChunk = chunk.text.split("\n").length;
        let highlights;
        const name = this.props.name + "_chunk_" + chunk.id;
        let failures = [];

        if (name in this.props.lintFailures) {
          failures = this.props.lintFailures[name].errors;
        }

        if (this.props.interactErrorExists) {
          highlights = this.props.highlights.filter(h => h[0] > chunk.startLine && h[0] <= chunk.startLine + linesInChunk);
        } else {
          highlights = [];
        }

        const isLast = index === this.state.chunks.length;
        return react__WEBPACK_IMPORTED_MODULE_1___default.a.createElement(react_beautiful_dnd__WEBPACK_IMPORTED_MODULE_2__["Draggable"], {
          key: chunk.id,
          draggableId: chunk.id,
          index: index,
          __source: {
            fileName: _jsxFileName,
            lineNumber: 216
          },
          __self: this
        }, (provided, snapshot) => {
          return react__WEBPACK_IMPORTED_MODULE_1___default.a.createElement("div", Object.assign({
            ref: provided.innerRef
          }, provided.draggableProps, provided.dragHandleProps, {
            __source: {
              fileName: _jsxFileName,
              lineNumber: 218
            },
            __self: this
          }), react__WEBPACK_IMPORTED_MODULE_1___default.a.createElement(DefChunk, {
            name: name,
            isLast: isLast,
            failures: failures,
            highlights: highlights,
            startLine: chunk.startLine,
            key: chunk.id,
            index: index,
            chunk: chunk.text,
            onEdit: onEdit,
            __source: {
              fileName: _jsxFileName,
              lineNumber: 220
            },
            __self: this
          }));
        });
      }));
    }));
  }

}

/***/ }),

/***/ "./src/Dropdown.tsx":
/*!**************************!*\
  !*** ./src/Dropdown.tsx ***!
  \**************************/
/*! exports provided: DropdownOption, Dropdown */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "DropdownOption", function() { return DropdownOption; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "Dropdown", function() { return Dropdown; });
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! react */ "./node_modules/react/index.js");
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_0__);
var _jsxFileName = "/mnt/c/users/andro/Desktop/cs5152/pyret-lang/ide/src/Dropdown.tsx";

class DropdownOption extends react__WEBPACK_IMPORTED_MODULE_0___default.a.Component {
  render() {
    const internalName = "DropdownOption".concat(new Date().getTime());
    return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: this.props.enabled ? "run-option-enabled" : "run-option-disabled",
      onClick: this.props.onClick,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 15
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("input", {
      type: "checkBox",
      checked: this.props.enabled,
      name: internalName,
      className: "run-option-checkbox",
      readOnly: true,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 17
      },
      __self: this
    }), react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("label", {
      htmlFor: internalName,
      className: "run-option-label",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 23
      },
      __self: this
    }, this.props.children));
  }

}
class Dropdown extends react__WEBPACK_IMPORTED_MODULE_0___default.a.Component {
  render() {
    return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: "run-dropdown",
      onClick: e => e.stopPropagation(),
      __source: {
        fileName: _jsxFileName,
        lineNumber: 39
      },
      __self: this
    }, this.props.children);
  }

}

/***/ }),

/***/ "./src/FSBrowser.tsx":
/*!***************************!*\
  !*** ./src/FSBrowser.tsx ***!
  \***************************/
/*! exports provided: FSBrowser */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "FSBrowser", function() { return FSBrowser; });
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! react */ "./node_modules/react/index.js");
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_0__);
/* harmony import */ var _control__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./control */ "./src/control.ts");
var _jsxFileName = "/mnt/c/users/andro/Desktop/cs5152/pyret-lang/ide/src/FSBrowser.tsx";



class FSItem extends react__WEBPACK_IMPORTED_MODULE_0___default.a.Component {
  render() {
    const path = _control__WEBPACK_IMPORTED_MODULE_1__["bfsSetup"].path.join(...this.props.path);
    const stats = _control__WEBPACK_IMPORTED_MODULE_1__["fs"].statSync(path);

    const label = (() => {
      if (stats.isDirectory()) {
        return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
          __source: {
            fileName: _jsxFileName,
            lineNumber: 20
          },
          __self: this
        }, "\uD83D\uDCC2");
      } else if (stats.isFile()) {
        return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
          __source: {
            fileName: _jsxFileName,
            lineNumber: 22
          },
          __self: this
        }, "\uD83D\uDDB9");
      } else {
        return "?";
      }
    })();

    const background = this.props.selected ? "darkgray" : "rgba(0, 0, 0, 0.3)";
    return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("button", {
      onClick: this.props.onClick,
      style: {
        background: background,
        border: 0,
        height: "2.7em",
        color: "#fff",
        textAlign: "left",
        flex: "none",
        cursor: "pointer"
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 30
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      style: {
        display: "flex",
        flexDirection: "row"
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 40
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      style: {
        width: "1em",
        paddingRight: "1em"
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 44
      },
      __self: this
    }, label), react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      __source: {
        fileName: _jsxFileName,
        lineNumber: 50
      },
      __self: this
    }, this.props.path[this.props.path.length - 1])));
  }

}

var EditType;

(function (EditType) {
  EditType[EditType["CreateFile"] = 0] = "CreateFile";
  EditType[EditType["CreateDirectory"] = 1] = "CreateDirectory";
})(EditType || (EditType = {}));

class FSBrowser extends react__WEBPACK_IMPORTED_MODULE_0___default.a.Component {
  constructor(props) {
    super(props);

    this.traverseUp = () => {
      const newPath = this.props.browsePath.slice();
      newPath.pop();
      this.setState({
        selected: undefined
      });
      this.props.onTraverseUp(newPath);
    };

    this.traverseDown = childDirectory => {
      const newPath = this.props.browsePath.slice();
      newPath.push(childDirectory);
      this.setState({
        selected: undefined
      });
      this.props.onTraverseDown(newPath);
    };

    this.expandChild = child => {
      const fullChildPath = _control__WEBPACK_IMPORTED_MODULE_1__["bfsSetup"].path.join(this.browsePathString, child);
      const stats = _control__WEBPACK_IMPORTED_MODULE_1__["fs"].statSync(fullChildPath);

      if (stats.isDirectory()) {
        this.traverseDown(child);
      } else if (stats.isFile()) {
        this.setState({
          selected: child
        });
        this.props.onExpandChild(child, fullChildPath);
      }
    };

    this.createFSItemPair = filePath => {
      return [filePath, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(FSItem, {
        key: filePath,
        onClick: () => this.expandChild(filePath),
        path: [...this.props.browsePath, filePath],
        selected: filePath === this.state.selected,
        __source: {
          fileName: _jsxFileName,
          lineNumber: 151
        },
        __self: this
      })];
    };

    this.toggleEditFile = () => {
      if (this.state.editType === EditType.CreateFile) {
        this.setState({
          editType: undefined
        });
      } else {
        this.setState({
          editType: EditType.CreateFile
        });
      }
    };

    this.toggleEditDirectory = () => {
      if (this.state.editType === EditType.CreateDirectory) {
        this.setState({
          editType: undefined
        });
      } else {
        this.setState({
          editType: EditType.CreateDirectory
        });
      }
    };

    this.handleSubmit = value => {
      value.preventDefault();
      const name = this.state.editValue;
      const path = _control__WEBPACK_IMPORTED_MODULE_1__["bfsSetup"].path.join(...this.props.browsePath, name);

      if (this.state.editType === EditType.CreateFile) {
        _control__WEBPACK_IMPORTED_MODULE_1__["createFile"](path);
      } else {
        _control__WEBPACK_IMPORTED_MODULE_1__["createDirectory"](path);
      }

      this.setState({
        editType: undefined,
        editValue: ""
      });
    };

    this.onChange = event => {
      this.setState({
        editValue: event.target.value
      });
    };

    this.deleteSelected = () => {
      if (this.state.selected === undefined) {
        _control__WEBPACK_IMPORTED_MODULE_1__["removeDirectory"](this.browsePathString);
        this.traverseUp();
      } else {
        _control__WEBPACK_IMPORTED_MODULE_1__["removeFile"](_control__WEBPACK_IMPORTED_MODULE_1__["bfsSetup"].path.join(...this.props.browsePath, this.state.selected));
        this.setState({
          selected: undefined
        });
      }
    };

    this.selectCurrentDirectory = () => {
      this.setState({
        selected: undefined
      });
    };

    this.uploadFile = event => {
      const currentDirectory = this.props.browsePath;
      const file = event.target.files[0];

      if (file) {
        const reader = new FileReader();

        reader.onload = e => {
          const data = e.target.result;
          const name = file.name;
          _control__WEBPACK_IMPORTED_MODULE_1__["bfsSetup"].fs.writeFileSync(_control__WEBPACK_IMPORTED_MODULE_1__["bfsSetup"].path.join(...currentDirectory, name), data);
          this.setState(this.state);
        };

        reader.readAsText(file);
      }
    };

    this.state = {
      editType: undefined,
      editValue: "",
      selected: undefined
    };
  }

  get browsePathString() {
    return _control__WEBPACK_IMPORTED_MODULE_1__["bfsSetup"].path.join(...this.props.browsePath);
  }

  get browsingRoot() {
    return _control__WEBPACK_IMPORTED_MODULE_1__["bfsSetup"].path.join(...this.props.browsePath) === this.props.root;
  }

  render() {
    const editor = this.state.editType !== undefined && react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      style: {
        display: "flex",
        flexDirection: "row",
        alignItems: "center"
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 251
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("pre", {
      style: {
        paddingLeft: "1em",
        paddingRight: "1em"
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 256
      },
      __self: this
    }, this.state.editType === EditType.CreateFile ? react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      __source: {
        fileName: _jsxFileName,
        lineNumber: 261
      },
      __self: this
    }, "\uD83D\uDDB9 Name") : react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      __source: {
        fileName: _jsxFileName,
        lineNumber: 263
      },
      __self: this
    }, "\uD83D\uDCC2 Name")), react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("form", {
      onSubmit: this.handleSubmit,
      style: {
        height: "100%",
        flexGrow: 1
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 266
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("input", {
      type: "text",
      value: this.state.editValue,
      onChange: this.onChange,
      style: {
        border: 0,
        padding: 0,
        width: "100%",
        height: "100%"
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 271
      },
      __self: this
    })));
    return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      style: {
        display: "flex",
        flexDirection: "column"
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 285
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      style: {
        display: "flex",
        flexDirection: "column"
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 286
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: "fs-browser-item",
      style: {
        display: "flex",
        flexDirection: "row",
        height: "auto"
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 287
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      onClick: this.selectCurrentDirectory,
      style: {
        cursor: "pointer",
        fontFamily: "monospace",
        display: "flex",
        alignItems: "center",
        paddingLeft: "1em",
        paddingRight: "1em",
        background: this.state.selected ? "none" : "darkgray"
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 293
      },
      __self: this
    }, this.props.browsePath[this.props.browsePath.length - 1]), react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      style: {
        flexGrow: 1,
        display: "flex",
        flexDirection: "row",
        justifyContent: "flex-end"
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 305
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("label", {
      className: "fs-browser-item",
      style: {
        width: "2.3em",
        height: "100%",
        display: "flex",
        justifyContent: "center",
        alignContent: "center",
        alignItems: "center"
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 311
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("input", {
      type: "file",
      onChange: this.uploadFile,
      style: {
        display: "none"
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 320
      },
      __self: this
    }), "\u2BB9"), react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("button", {
      className: "fs-browser-item",
      onClick: this.toggleEditFile,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 327
      },
      __self: this
    }, "+\uD83D\uDDB9"), react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("button", {
      className: "fs-browser-item",
      onClick: this.toggleEditDirectory,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 331
      },
      __self: this
    }, "+\uD83D\uDCC2"), this.browsePathString !== this.props.root && react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("button", {
      className: "fs-browser-item",
      onClick: this.deleteSelected,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 336
      },
      __self: this
    }, "\u274C"))), editor, !this.browsingRoot && react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(FSItem, {
      onClick: this.traverseUp,
      path: [".."],
      selected: false,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 344
      },
      __self: this
    }), _control__WEBPACK_IMPORTED_MODULE_1__["fs"].readdirSync(this.browsePathString).map(this.createFSItemPair).sort(FSBrowser.compareFSItemPair).map(x => x[1])));
  }

}

FSBrowser.compareFSItemPair = (a, b) => {
  if (a[0] < b[0]) {
    return -1;
  } else if (a[0] > b[0]) {
    return 1;
  } else {
    return 0;
  }
};

/***/ }),

/***/ "./src/FontSize.tsx":
/*!**************************!*\
  !*** ./src/FontSize.tsx ***!
  \**************************/
/*! exports provided: FontSize */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "FontSize", function() { return FontSize; });
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! react */ "./node_modules/react/index.js");
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_0__);
var _jsxFileName = "/mnt/c/users/andro/Desktop/cs5152/pyret-lang/ide/src/FontSize.tsx";

class FontSize extends react__WEBPACK_IMPORTED_MODULE_0___default.a.Component {
  render() {
    return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: "font-size-options",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 15
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("button", {
      className: "font-minus",
      onClick: this.props.onDecrease,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 16
      },
      __self: this
    }, "-"), react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("button", {
      className: "font-label",
      onClick: this.props.onReset,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 20
      },
      __self: this
    }, "Font (", this.props.size, " px)"), react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("button", {
      className: "font-plus",
      onClick: this.props.onIncrease,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 24
      },
      __self: this
    }, "+"));
  }

}

/***/ }),

/***/ "./src/Footer.tsx":
/*!************************!*\
  !*** ./src/Footer.tsx ***!
  \************************/
/*! exports provided: Footer */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "Footer", function() { return Footer; });
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! react */ "./node_modules/react/index.js");
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_0__);
var _jsxFileName = "/mnt/c/users/andro/Desktop/cs5152/pyret-lang/ide/src/Footer.tsx";

class Footer extends react__WEBPACK_IMPORTED_MODULE_0___default.a.Component {
  render() {
    return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: "footer-container",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 11
      },
      __self: this
    }, this.props.message);
  }

}

/***/ }),

/***/ "./src/Header.tsx":
/*!************************!*\
  !*** ./src/Header.tsx ***!
  \************************/
/*! exports provided: Header */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "Header", function() { return Header; });
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! react */ "./node_modules/react/index.js");
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_0__);
var _jsxFileName = "/mnt/c/users/andro/Desktop/cs5152/pyret-lang/ide/src/Header.tsx";

class Header extends react__WEBPACK_IMPORTED_MODULE_0___default.a.Component {
  render() {
    return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: "header-container",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 9
      },
      __self: this
    }, this.props.children);
  }

}

/***/ }),

/***/ "./src/Image.tsx":
/*!***********************!*\
  !*** ./src/Image.tsx ***!
  \***********************/
/*! exports provided: ImageWidget */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "ImageWidget", function() { return ImageWidget; });
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! react */ "./node_modules/react/index.js");
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_0__);
var _jsxFileName = "/mnt/c/users/andro/Desktop/cs5152/pyret-lang/ide/src/Image.tsx";

class ImageWidget extends react__WEBPACK_IMPORTED_MODULE_0___default.a.Component {
  constructor(props) {
    super(props);
    this.canvas = void 0;
    this.canvas = null;
  }

  componentDidMount() {
    this.updateCanvas();
  }

  componentDidUpdate() {
    this.updateCanvas();
  }

  updateCanvas() {
    const ctx = this.canvas.getContext('2d');
    ctx.clearRect(0, 0, this.props.image.getHeight(), this.props.image.getHeight());
    this.props.image.render(ctx, 0, 0);
  }

  render() {
    return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("canvas", {
      width: this.props.image.getWidth(),
      height: this.props.image.getHeight(),
      ref: canvas => this.canvas = canvas,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 27
      },
      __self: this
    });
  }

}

/***/ }),

/***/ "./src/Interaction.tsx":
/*!*****************************!*\
  !*** ./src/Interaction.tsx ***!
  \*****************************/
/*! exports provided: Interaction */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "Interaction", function() { return Interaction; });
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! react */ "./node_modules/react/index.js");
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_0__);
/* harmony import */ var _Table__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./Table */ "./src/Table.tsx");
/* harmony import */ var _Image__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ./Image */ "./src/Image.tsx");
var _jsxFileName = "/mnt/c/users/andro/Desktop/cs5152/pyret-lang/ide/src/Interaction.tsx";



class Interaction extends react__WEBPACK_IMPORTED_MODULE_0___default.a.Component {
  constructor(...args) {
    super(...args);

    this.convert = value => {
      if (value === undefined) {
        return "undefined";
      } else if (typeof value === 'number') {
        return value.toString();
      } else if (typeof value === 'string') {
        return "\"".concat(value, "\"");
      } else if (typeof value === 'boolean') {
        return value.toString();
      } else if (typeof value === 'function') {
        // TODO(michael) can we display more info than just <function> ?
        return "<function>";
      } else if (value.$brand === '$table') {
        return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_Table__WEBPACK_IMPORTED_MODULE_1__["TableWidget"], {
          headers: value._headers,
          rows: value._rows,
          htmlify: this.convert,
          setMessage: this.props.setMessage,
          __source: {
            fileName: _jsxFileName,
            lineNumber: 28
          },
          __self: this
        });
      } else if (value.$brand === 'image') {
        return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_Image__WEBPACK_IMPORTED_MODULE_2__["ImageWidget"], {
          image: value,
          __source: {
            fileName: _jsxFileName,
            lineNumber: 36
          },
          __self: this
        });
      } else if (typeof value === 'object') {
        // TODO(michael) palceholder for better object display
        return JSON.stringify(value);
      }
    };
  }

  render() {
    if (this.props.name === "$checks" || this.props.name === "$answer") {
      return null;
    }

    return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: "interaction",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 51
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("pre", {
      className: "interaction-identifier",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 52
      },
      __self: this
    }, this.props.name, " =\xA0"), this.convert(this.props.value));
  }

}

/***/ }),

/***/ "./src/InteractionError.tsx":
/*!**********************************!*\
  !*** ./src/InteractionError.tsx ***!
  \**********************************/
/*! exports provided: InteractionError */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "InteractionError", function() { return InteractionError; });
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! react */ "./node_modules/react/index.js");
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_0__);
var _jsxFileName = "/mnt/c/users/andro/Desktop/cs5152/pyret-lang/ide/src/InteractionError.tsx";

class InteractionError extends react__WEBPACK_IMPORTED_MODULE_0___default.a.Component {
  render() {
    return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: "interaction-error",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 12
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("p", {
      style: {
        fontSize: this.props.fontSize,
        padding: 0,
        margin: 0
      },
      __source: {
        fileName: _jsxFileName,
        lineNumber: 13
      },
      __self: this
    }, this.props.children));
  }

}

/***/ }),

/***/ "./src/Menu.tsx":
/*!**********************!*\
  !*** ./src/Menu.tsx ***!
  \**********************/
/*! exports provided: Tab, Menu */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "Tab", function() { return Tab; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "Menu", function() { return Menu; });
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! react */ "./node_modules/react/index.js");
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_0__);
var _jsxFileName = "/mnt/c/users/andro/Desktop/cs5152/pyret-lang/ide/src/Menu.tsx";

class Tab extends react__WEBPACK_IMPORTED_MODULE_0___default.a.Component {
  render() {
    return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: "menu-content",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 11
      },
      __self: this
    }, this.props.children);
  }

}
class Menu extends react__WEBPACK_IMPORTED_MODULE_0___default.a.Component {
  constructor(props) {
    super(props);

    this.toggleTab = n => {
      if (this.state.tab === n) {
        this.setState({
          visible: !this.state.visible
        });
      } else {
        this.setState({
          tab: n,
          visible: true
        });
      }
    };

    this.state = {
      visible: false,
      tab: 0
    };
  }

  render() {
    const childNodes = Array.isArray(this.props.children) && this.props.children.map((tab, index) => {
      return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
        className: this.state.visible && this.state.tab === index ? "menu-tab-active" : "menu-tab-inactive",
        key: index,
        onClick: () => this.toggleTab(index),
        __source: {
          fileName: _jsxFileName,
          lineNumber: 49
        },
        __self: this
      }, tab.props.name);
    });
    const content = Array.isArray(this.props.children) && this.props.children[this.state.tab];
    return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: "menu-container",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 64
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: "menu-tabbar",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 65
      },
      __self: this
    }, childNodes), this.state.visible && content);
  }

}

/***/ }),

/***/ "./src/SingleCodeMirrorDefinitions.tsx":
/*!*********************************************!*\
  !*** ./src/SingleCodeMirrorDefinitions.tsx ***!
  \*********************************************/
/*! exports provided: SingleCodeMirrorDefinitions */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "SingleCodeMirrorDefinitions", function() { return SingleCodeMirrorDefinitions; });
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! react */ "./node_modules/react/index.js");
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_0__);
/* harmony import */ var react_codemirror2__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! react-codemirror2 */ "./node_modules/react-codemirror2/index.js");
/* harmony import */ var react_codemirror2__WEBPACK_IMPORTED_MODULE_1___default = /*#__PURE__*/__webpack_require__.n(react_codemirror2__WEBPACK_IMPORTED_MODULE_1__);
/* harmony import */ var codemirror_lib_codemirror_css__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! codemirror/lib/codemirror.css */ "./node_modules/codemirror/lib/codemirror.css");
/* harmony import */ var codemirror_lib_codemirror_css__WEBPACK_IMPORTED_MODULE_2___default = /*#__PURE__*/__webpack_require__.n(codemirror_lib_codemirror_css__WEBPACK_IMPORTED_MODULE_2__);
/* harmony import */ var pyret_codemirror_mode_css_pyret_css__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! pyret-codemirror-mode/css/pyret.css */ "./node_modules/pyret-codemirror-mode/css/pyret.css");
/* harmony import */ var pyret_codemirror_mode_css_pyret_css__WEBPACK_IMPORTED_MODULE_3___default = /*#__PURE__*/__webpack_require__.n(pyret_codemirror_mode_css_pyret_css__WEBPACK_IMPORTED_MODULE_3__);
/* harmony import */ var codemirror__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! codemirror */ "./node_modules/codemirror/lib/codemirror.js");
/* harmony import */ var codemirror__WEBPACK_IMPORTED_MODULE_4___default = /*#__PURE__*/__webpack_require__.n(codemirror__WEBPACK_IMPORTED_MODULE_4__);
var _jsxFileName = "/mnt/c/users/andro/Desktop/cs5152/pyret-lang/ide/src/SingleCodeMirrorDefinitions.tsx";



 // pyret-codemirror-mode/mode/pyret.js expects window.CodeMirror to exist and
// to be bound to the 'codemirror' import.


window.CodeMirror = codemirror__WEBPACK_IMPORTED_MODULE_4__;

__webpack_require__(/*! pyret-codemirror-mode/mode/pyret */ "./node_modules/pyret-codemirror-mode/mode/pyret.js");

class SingleCodeMirrorDefinitions extends react__WEBPACK_IMPORTED_MODULE_0___default.a.Component {
  constructor(props) {
    super(props);

    this.onChange = (editor, data, value) => {
      this.setState({
        editor: editor
      });

      for (var i = 0; i < editor.getDoc().getAllMarks().length; i++) {
        editor.getDoc().getAllMarks()[i].clear();
      }

      this.props.onEdit(value);
    };

    this.state = {
      editor: null
    };
  }

  componentDidUpdate() {
    if (this.state.editor !== null) {
      if (this.props.interactErrorExists) {
        for (let i = 0; i < this.props.highlights.length; i++) {
          this.state.editor.getDoc().markText({
            line: this.props.highlights[i][0] - 1,
            ch: this.props.highlights[i][1]
          }, {
            line: this.props.highlights[i][2] - 1,
            ch: this.props.highlights[i][3]
          }, {
            className: "styled-background-error"
          });
        }
      } else {
        for (let i = 0; i < this.state.editor.getDoc().getAllMarks().length; i++) {
          this.state.editor.getDoc().getAllMarks()[i].clear();
        }
      }
    }
  }

  render() {
    return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(react_codemirror2__WEBPACK_IMPORTED_MODULE_1__["UnControlled"], {
      value: this.props.text,
      options: {
        mode: 'pyret',
        theme: 'default',
        lineNumbers: true,
        lineWrapping: true
      },
      onChange: this.onChange,
      autoCursor: false,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 56
      },
      __self: this
    });
  }

}

/***/ }),

/***/ "./src/Table.tsx":
/*!***********************!*\
  !*** ./src/Table.tsx ***!
  \***********************/
/*! exports provided: TableWidget */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "TableWidget", function() { return TableWidget; });
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! react */ "./node_modules/react/index.js");
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_0__);
/* harmony import */ var react_table__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! react-table */ "./node_modules/react-table/es/index.js");
/* harmony import */ var react_table_react_table_css__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! react-table/react-table.css */ "./node_modules/react-table/react-table.css");
/* harmony import */ var react_table_react_table_css__WEBPACK_IMPORTED_MODULE_2___default = /*#__PURE__*/__webpack_require__.n(react_table_react_table_css__WEBPACK_IMPORTED_MODULE_2__);
/* harmony import */ var react_copy_to_clipboard__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! react-copy-to-clipboard */ "./node_modules/react-copy-to-clipboard/lib/index.js");
/* harmony import */ var react_copy_to_clipboard__WEBPACK_IMPORTED_MODULE_3___default = /*#__PURE__*/__webpack_require__.n(react_copy_to_clipboard__WEBPACK_IMPORTED_MODULE_3__);
var _jsxFileName = "/mnt/c/users/andro/Desktop/cs5152/pyret-lang/ide/src/Table.tsx";




class TableWidget extends react__WEBPACK_IMPORTED_MODULE_0___default.a.Component {
  render() {
    const data = this.props.rows;
    const columns = this.props.headers.map((header, index) => {
      return {
        id: header,
        Header: header,
        accessor: row => {
          return this.props.htmlify(row[index]);
        }
      };
    });
    const maxRowsPerPage = 5;
    const showOptions = this.props.rows.length > maxRowsPerPage;
    const defaultPageSize = showOptions ? maxRowsPerPage : this.props.rows.length;
    return react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: "table-container",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 31
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(react_copy_to_clipboard__WEBPACK_IMPORTED_MODULE_3___default.a, {
      text: data.map(d => d.join("\t")).join("\n"),
      onCopy: () => this.props.setMessage("Copied table data"),
      __source: {
        fileName: _jsxFileName,
        lineNumber: 32
      },
      __self: this
    }, react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement("div", {
      className: "table-copy",
      __source: {
        fileName: _jsxFileName,
        lineNumber: 34
      },
      __self: this
    }, "\uD83D\uDCCB")), react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(react_table__WEBPACK_IMPORTED_MODULE_1__["default"], {
      data: data,
      columns: columns,
      showPagination: showOptions,
      pageSize: defaultPageSize,
      showPageSizeOptions: false,
      filterable: showOptions,
      __source: {
        fileName: _jsxFileName,
        lineNumber: 38
      },
      __self: this
    }));
  }

}

/***/ }),

/***/ "./src/backend.ts":
/*!************************!*\
  !*** ./src/backend.ts ***!
  \************************/
/*! exports provided: RunKind, makeBackendMessageHandler, lintProgram, compileProgram, runProgram */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "RunKind", function() { return RunKind; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "makeBackendMessageHandler", function() { return makeBackendMessageHandler; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "lintProgram", function() { return lintProgram; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "compileProgram", function() { return compileProgram; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "runProgram", function() { return runProgram; });
let RunKind;

(function (RunKind) {
  RunKind["Sync"] = "SYNC";
  RunKind["Async"] = "ASYNC";
})(RunKind || (RunKind = {}));

let compileStart = window.performance.now();
/*
 * Handles Pyret compiler messages ONLY.
 * Ignores all other messages (including BrowserFS messages)
 */

const makeBackendMessageHandler = (echoLog, compileFailure, runtimeFailure, lintFailure, lintSuccess, compileSuccess) => {
  const backendMessageHandler = e => {
    if (e.data.browserfsMessage === true) {
      return null;
    }

    try {
      var msgObject = JSON.parse(e.data);
      var msgType = msgObject["type"];

      if (msgType === undefined) {
        return null;
      } else if (msgType === "echo-log") {
        echoLog(msgObject.contents);
      } else if (msgType === "lint-failure") {
        lintFailure(msgObject.data);
      } else if (msgType === "lint-success") {
        lintSuccess(msgObject.data);
      } else if (msgType === "compile-failure") {
        compileFailure(msgObject.data);
      } else if (msgType === "compile-success") {
        console.log("compile-time: ", window.performance.now() - compileStart);
        compileSuccess();
      } else {
        return null;
      }
    } catch (e) {
      console.log(e);
      runtimeFailure(e);
      return null;
    }
  };

  return backendMessageHandler;
};
const lintProgram = (compilerWorker, options) => {
  const message = {
    _parley: true,
    options: {
      program: options.program,
      "program-source": options.programSource,
      "lint": true
    }
  };
  compilerWorker.postMessage(message);
};
const compileProgram = (compilerWorker, options) => {
  compileStart = window.performance.now();
  const message = {
    _parley: true,
    options: {
      program: options.program,
      "base-dir": options.baseDir,
      "builtin-js-dir": options.builtinJSDir,
      checks: options.checks,
      'type-check': options.typeCheck,
      'recompile-builtins': options.recompileBuiltins
    }
  };
  compilerWorker.postMessage(message);
};

const assertNever = _arg => {
  throw new Error('assertNever');
};

const runProgram = (runner, baseDir, program, runKind) => {
  if (runKind === RunKind.Sync) {
    const start = window.performance.now();
    const result = runner.makeRequire(baseDir)(program);
    const end = window.performance.now();
    return Promise.resolve({
      time: end - start,
      result: result
    });
  } else if (runKind === RunKind.Async) {
    const entry = runner.makeRequireAsync(baseDir);
    const resultP = entry(program);

    let wrapper = async function () {
      const start = window.performance.now();
      let result = await resultP;
      const end = window.performance.now();
      return {
        time: end - start,
        result: result
      };
    };

    return wrapper();
  } else {
    // NOTE(michael): type checking in Typescript on enums is not exhaustive (as of v3.5.3)
    return assertNever(runKind);
  }
};

/***/ }),

/***/ "./src/browserfs-setup.ts":
/*!********************************!*\
  !*** ./src/browserfs-setup.ts ***!
  \********************************/
/*! exports provided: BrowserFS, fs, path, install, configure */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "BrowserFS", function() { return BrowserFS; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "fs", function() { return fs; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "path", function() { return path; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "install", function() { return install; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "configure", function() { return configure; });
const BrowserFS = __webpack_require__(/*! browserfs */ "./node_modules/browserfs/dist/browserfs.js");
const fs = BrowserFS.BFSRequire('fs');
const path = BrowserFS.BFSRequire('path');
const install = () => {
  BrowserFS.install(window);
};
const configure = (worker, projectsDirectory) => {
  BrowserFS.configure({
    fs: "LocalStorage"
  }, function (e) {
    if (e) {
      throw e;
    }

    if (!fs.existsSync(projectsDirectory)) {
      fs.mkdirSync(projectsDirectory);
    }

    BrowserFS.FileSystem.WorkerFS.attachRemoteListener(worker);
    window["bfs"] = BrowserFS.BFSRequire("fs");
  });
};

/***/ }),

/***/ "./src/control.ts":
/*!************************!*\
  !*** ./src/control.ts ***!
  \************************/
/*! exports provided: backend, path, bfsSetup, worker, installFileSystem, loadBuiltins, runProgram, compileProgram, fs, createFile, createDirectory, removeFile, removeDirectory, deleteDir, removeRootDirectory, lint, compile, run, setupWorkerMessageHandler, openOrCreateFile */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "worker", function() { return worker; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "installFileSystem", function() { return installFileSystem; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "loadBuiltins", function() { return loadBuiltins; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "runProgram", function() { return runProgram; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "compileProgram", function() { return compileProgram; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "fs", function() { return fs; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "createFile", function() { return createFile; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "createDirectory", function() { return createDirectory; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "removeFile", function() { return removeFile; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "removeDirectory", function() { return removeDirectory; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "deleteDir", function() { return deleteDir; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "removeRootDirectory", function() { return removeRootDirectory; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "lint", function() { return lint; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "compile", function() { return compile; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "run", function() { return run; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "setupWorkerMessageHandler", function() { return setupWorkerMessageHandler; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "openOrCreateFile", function() { return openOrCreateFile; });
/* harmony import */ var _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./browserfs-setup */ "./src/browserfs-setup.ts");
/* harmony reexport (module object) */ __webpack_require__.d(__webpack_exports__, "bfsSetup", function() { return _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__; });
/* harmony import */ var _runtime_loader__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./runtime-loader */ "./src/runtime-loader.ts");
/* harmony import */ var _runner__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ./runner */ "./src/runner.ts");
/* harmony import */ var _backend__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ./backend */ "./src/backend.ts");
/* harmony reexport (module object) */ __webpack_require__.d(__webpack_exports__, "backend", function() { return _backend__WEBPACK_IMPORTED_MODULE_3__; });
/* harmony import */ var _path__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ./path */ "./src/path.ts");
/* harmony reexport (module object) */ __webpack_require__.d(__webpack_exports__, "path", function() { return _path__WEBPACK_IMPORTED_MODULE_4__; });






const runtimeFiles = __webpack_require__(/*! ./runtime-files.json */ "./src/runtime-files.json");


const worker = new Worker(_path__WEBPACK_IMPORTED_MODULE_4__["pyretJarr"]);
const installFileSystem = () => {
  _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["install"]();
  _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["configure"](worker, _path__WEBPACK_IMPORTED_MODULE_4__["compileBase"]);
};
const loadBuiltins = () => {
  _runtime_loader__WEBPACK_IMPORTED_MODULE_1__["load"](_browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["fs"], _path__WEBPACK_IMPORTED_MODULE_4__["compileBuiltinJS"], _path__WEBPACK_IMPORTED_MODULE_4__["uncompiled"], runtimeFiles);
};
const runProgram = _backend__WEBPACK_IMPORTED_MODULE_3__["runProgram"];
const compileProgram = _backend__WEBPACK_IMPORTED_MODULE_3__["compileProgram"];
const fs = _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["fs"];
const createFile = file => {
  _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["fs"].writeFileSync(file, "");
};
const createDirectory = dir => {
  _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["fs"].mkdirSync(dir);
};
const removeFile = path => {
  _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["fs"].unlinkSync(path);
}; // Synchronous deleteDir

const removeDirectory = dir => {
  const files = _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["fs"].readdirSync(dir);
  files.forEach(file => {
    const filePath = _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["path"].join(dir, file);
    const stats = _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["fs"].statSync(filePath);

    if (stats.isDirectory()) {
      removeDirectory(filePath);
    } else {
      _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["fs"].unlinkSync(filePath);
    }
  });
  _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["fs"].rmdirSync(dir);
};
const deleteDir = dir => {
  _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["fs"].readdir(dir, function (err, files) {
    if (err) {
      throw err;
    }

    files.forEach(function (file) {
      let filePath = _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["path"].join(dir, file);
      _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["fs"].stat(filePath, function (err, stats) {
        if (err) {
          throw err;
        }

        if (stats.isDirectory()) {
          deleteDir(filePath);
        } else {
          _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["fs"].unlink(filePath, function (err) {
            if (err) {
              throw err;
            }
          });
        }
      });
    });
  });
};
const removeRootDirectory = () => {
  deleteDir(_path__WEBPACK_IMPORTED_MODULE_4__["root"]);
};
const lint = (programFileName, programText) => {
  _backend__WEBPACK_IMPORTED_MODULE_3__["lintProgram"](worker, {
    "program": programFileName,
    "programSource": programText
  });
};
const compile = (baseDirectory, programFileName, typeCheck) => {
  _backend__WEBPACK_IMPORTED_MODULE_3__["compileProgram"](worker, {
    "program": programFileName,
    "baseDir": baseDirectory,
    "builtinJSDir": _path__WEBPACK_IMPORTED_MODULE_4__["compileBuiltinJS"],
    "checks": "none",
    "typeCheck": typeCheck,
    "recompileBuiltins": false
  });
};
const run = (baseDirectory, programFileName, callback, runKind) => {
  _backend__WEBPACK_IMPORTED_MODULE_3__["runProgram"](_runner__WEBPACK_IMPORTED_MODULE_2__, baseDirectory, programFileName, runKind).catch(x => {
    console.error(x);
    return {
      result: {
        error: String(x.value)
      }
    };
  }).then(callback);
};
const setupWorkerMessageHandler = (onLog, onCompileFailure, onRuntimeFailure, lintFailure, lintSuccess, onCompileSuccess) => {
  worker.onmessage = _backend__WEBPACK_IMPORTED_MODULE_3__["makeBackendMessageHandler"](onLog, onCompileFailure, onRuntimeFailure, lintFailure, lintSuccess, onCompileSuccess);
};
const openOrCreateFile = path => {
  if (_browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["fs"].existsSync(path)) {
    return _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["fs"].readFileSync(path, "utf-8");
  } else {
    _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["fs"].writeFileSync(path, "");
    return "";
  }
};

/***/ }),

/***/ "./src/index.css":
/*!***********************!*\
  !*** ./src/index.css ***!
  \***********************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

var content = __webpack_require__(/*! !../node_modules/css-loader/dist/cjs.js??ref--6-oneOf-3-1!../node_modules/postcss-loader/src??postcss!./index.css */ "./node_modules/css-loader/dist/cjs.js?!./node_modules/postcss-loader/src/index.js?!./src/index.css");

if (typeof content === 'string') {
  content = [[module.i, content, '']];
}

var options = {}

options.insert = "head";
options.singleton = false;

var update = __webpack_require__(/*! ../node_modules/style-loader/dist/runtime/injectStylesIntoStyleTag.js */ "./node_modules/style-loader/dist/runtime/injectStylesIntoStyleTag.js")(content, options);

if (content.locals) {
  module.exports = content.locals;
}

if (true) {
  if (!content.locals) {
    module.hot.accept(
      /*! !../node_modules/css-loader/dist/cjs.js??ref--6-oneOf-3-1!../node_modules/postcss-loader/src??postcss!./index.css */ "./node_modules/css-loader/dist/cjs.js?!./node_modules/postcss-loader/src/index.js?!./src/index.css",
      function () {
        var newContent = __webpack_require__(/*! !../node_modules/css-loader/dist/cjs.js??ref--6-oneOf-3-1!../node_modules/postcss-loader/src??postcss!./index.css */ "./node_modules/css-loader/dist/cjs.js?!./node_modules/postcss-loader/src/index.js?!./src/index.css");

        if (typeof newContent === 'string') {
          newContent = [[module.i, newContent, '']];
        }
        
        update(newContent);
      }
    )
  }

  module.hot.dispose(function() { 
    update();
  });
}

/***/ }),

/***/ "./src/index.tsx":
/*!***********************!*\
  !*** ./src/index.tsx ***!
  \***********************/
/*! no exports provided */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! react */ "./node_modules/react/index.js");
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_0__);
/* harmony import */ var react_dom__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! react-dom */ "./node_modules/react-dom/index.js");
/* harmony import */ var react_dom__WEBPACK_IMPORTED_MODULE_1___default = /*#__PURE__*/__webpack_require__.n(react_dom__WEBPACK_IMPORTED_MODULE_1__);
/* harmony import */ var _index_css__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ./index.css */ "./src/index.css");
/* harmony import */ var _index_css__WEBPACK_IMPORTED_MODULE_2___default = /*#__PURE__*/__webpack_require__.n(_index_css__WEBPACK_IMPORTED_MODULE_2__);
/* harmony import */ var _App__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ./App */ "./src/App.tsx");
/* harmony import */ var _serviceWorker__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ./serviceWorker */ "./src/serviceWorker.js");
var _jsxFileName = "/mnt/c/users/andro/Desktop/cs5152/pyret-lang/ide/src/index.tsx";





react_dom__WEBPACK_IMPORTED_MODULE_1___default.a.render(react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement(_App__WEBPACK_IMPORTED_MODULE_3__["default"], {
  __source: {
    fileName: _jsxFileName,
    lineNumber: 7
  },
  __self: undefined
}), document.getElementById('root')); // If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA

_serviceWorker__WEBPACK_IMPORTED_MODULE_4__["unregister"]();

/***/ }),

/***/ "./src/path.ts":
/*!*********************!*\
  !*** ./src/path.ts ***!
  \*********************/
/*! exports provided: root, compileBase, compileBuiltinJS, compileProgram, runBase, runProgram, uncompiled, program, pyretJarr */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "root", function() { return root; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "compileBase", function() { return compileBase; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "compileBuiltinJS", function() { return compileBuiltinJS; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "compileProgram", function() { return compileProgram; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "runBase", function() { return runBase; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "runProgram", function() { return runProgram; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "uncompiled", function() { return uncompiled; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "program", function() { return program; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "pyretJarr", function() { return pyretJarr; });
/* harmony import */ var _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./browserfs-setup */ "./src/browserfs-setup.ts");

const root = "/";
const compileBase = _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["path"].join(root, "projects");
const compileBuiltinJS = _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["path"].join(root, "prewritten");
const compileProgram = "program.arr";
const runBase = _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["path"].join(root, "compiled", "project");
const runProgram = "program.arr.js";
const uncompiled = _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["path"].join(root, "uncompiled");
const program = _browserfs_setup__WEBPACK_IMPORTED_MODULE_0__["path"].join(compileBase, compileProgram);
const pyretJarr = "pyret.jarr";

/***/ }),

/***/ "./src/runner.ts":
/*!***********************!*\
  !*** ./src/runner.ts ***!
  \***********************/
/*! exports provided: stopify, makeRequireAsync, makeRequire */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "stopify", function() { return stopify; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "makeRequireAsync", function() { return makeRequireAsync; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "makeRequire", function() { return makeRequire; });
const csv = __webpack_require__(!(function webpackMissingModule() { var e = new Error("Cannot find module 'csv-parse/lib/sync'"); e.code = 'MODULE_NOT_FOUND'; throw e; }()));

const assert = __webpack_require__(/*! assert */ "./node_modules/assert/assert.js");

const immutable = __webpack_require__(/*! immutable */ "./node_modules/immutable/dist/immutable.js");

const stopify = __webpack_require__(/*! @stopify/stopify */ "./node_modules/@stopify/stopify/dist/src/index.js");

const browserFS = __webpack_require__(/*! ./browserfs-setup.ts */ "./src/browserfs-setup.ts");

window["stopify"] = stopify;
const fs = browserFS.fs;
const path = browserFS.path;
const nodeModules = {
  'assert': assert,
  'csv-parse/lib/sync': csv,
  'fs': browserFS.fs,
  'immutable': immutable
};

function wrapContent(content) {
  return "(function() { ".concat(content, " })();");
}

const makeRequireAsync = basePath => {
  let cwd = basePath;
  let currentRunner = null;
  const cache = {};

  const requireAsyncMain = importPath => {
    return new Promise(function (resolve, reject) {
      if (importPath in nodeModules) {
        return nodeModules[importPath];
      }

      const oldWd = cwd;
      const nextPath = path.join(cwd, importPath);
      cwd = path.parse(nextPath).dir;

      if (!fs.existsSync(nextPath)) {
        throw new Error("Path did not exist in requireSync: " + nextPath);
      }

      const stoppedPath = nextPath + ".stopped";

      if (stoppedPath in cache) {
        resolve(cache[stoppedPath]);
        return;
      }

      let runner = null;
      const contents = String(fs.readFileSync(nextPath));
      const toStopify = wrapContent(contents);
      runner = stopify.stopifyLocally(toStopify, {});

      if (runner.kind !== "ok") {
        reject(runner);
      }

      fs.writeFileSync(stoppedPath, runner.code);
      const stopifyModuleExports = {
        exports: {
          __pyretExports: nextPath
        }
      };
      runner.g = Object.assign(runner.g, {
        document,
        Number,
        Math,
        Array,
        Object,
        RegExp,
        stopify,
        Error,
        Image: () => {
          return new Image();
        },
        decodeURIComponent,
        require: requireAsync,
        "module": stopifyModuleExports,
        // TS 'export' syntax desugars to 'exports.name = value;'
        "exports": stopifyModuleExports.exports,
        String,
        $STOPIFY: runner,
        setTimeout: setTimeout,
        console: console,
        parseFloat,
        isNaN,
        isFinite
      });
      runner.path = nextPath;
      currentRunner = runner;
      runner.run(result => {
        // TODO(Alex): fix stopify bug where evaled result is not passed to AbstractRunner.onDone callback
        cwd = oldWd;

        if (result.type !== "normal") {
          reject(result);
          return;
        }

        const toReturn = runner.g.module.exports;
        cache[stoppedPath] = toReturn;
        resolve(toReturn);
      });
    });
  };

  const requireAsync = importPath => {
    if (importPath in nodeModules) {
      return nodeModules[importPath];
    }

    const oldWd = cwd;
    const nextPath = path.join(cwd, importPath);
    cwd = path.parse(nextPath).dir;

    if (!fs.existsSync(nextPath)) {
      throw new Error("Path did not exist in requireSync: " + nextPath);
    }

    const stoppedPath = nextPath + ".stopped";

    if (stoppedPath in cache) {
      return cache[stoppedPath];
    }

    currentRunner.pauseK(kontinue => {
      const lastPath = currentRunner.path;
      const module = {
        exports: {
          __pyretExports: nextPath
        }
      };
      const lastModule = currentRunner.g.module;
      currentRunner.g.module = module; // Need to set 'exports' global to work with TS export desugaring

      currentRunner.g.exports = module.exports;
      currentRunner.path = nextPath;
      let stopifiedCode = "";

      if (fs.existsSync(stoppedPath) && fs.statSync(stoppedPath).mtime > fs.statSync(nextPath).mtime) {
        stopifiedCode = String(fs.readFileSync(stoppedPath));
      } else {
        const contents = String(fs.readFileSync(nextPath));
        stopifiedCode = currentRunner.compile(wrapContent(contents));
        fs.writeFileSync(stoppedPath, stopifiedCode);
      }

      currentRunner.evalCompiled(stopifiedCode, result => {
        if (result.type !== "normal") {
          kontinue(result);
          return;
        }

        const toReturn = currentRunner.g.module.exports;
        currentRunner.path = lastPath; // g.exports and g.module may be overwritten by JS code. Need to restore

        currentRunner.g.module = lastModule; // Need to set 'exports' global to work with TS export desugaring

        currentRunner.g.exports = lastModule.exports;
        cache[stoppedPath] = toReturn;
        kontinue({
          type: 'normal',
          value: toReturn
        });
      });
    });
  };

  return requireAsyncMain;
};
const makeRequire = basePath => {
  const cache = {};
  var cwd = basePath;
  /*
    Recursively eval (with this definition of require in scope) all of the
    described JavaScript.
     Note that since JS code is generated/written with the assumption that
    require() is sync, we can only use sync versions of the FS function here;
    require must be entirely one synchronous run of the code.
     Future use of stopify could enable the definition of requireAsync, which
    could pause the stack while requiring and then resume.
  */

  const requireSync = importPath => {
    if (importPath in nodeModules) {
      return nodeModules[importPath];
    }

    const oldWd = cwd;
    const nextPath = path.join(cwd, importPath);

    if (nextPath in cache) {
      return cache[nextPath];
    }

    cwd = path.parse(nextPath).dir;

    if (!fs.existsSync(nextPath)) {
      throw new Error("Path did not exist in requireSync: " + nextPath);
    }

    const contents = fs.readFileSync(nextPath); // TS 'export' syntax desugars to 'exports.name = value;'
    // Adding an 'exports' parameter simulates the global 'exports' variable

    const f = new Function("require", "module", "exports", contents);
    const module = {
      exports: {
        __pyretExports: nextPath
      }
    };
    const result = f(requireSync, module, module.exports);
    const toReturn = module.exports ? module.exports : result;
    cwd = oldWd;
    cache[nextPath] = toReturn;
    return toReturn;
  };

  return requireSync;
};

/***/ }),

/***/ "./src/runtime-files.json":
/*!********************************!*\
  !*** ./src/runtime-files.json ***!
  \********************************/
/*! exports provided: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, default */
/***/ (function(module) {

module.exports = JSON.parse("[{\"key\":\"prewritten/array.js\",\"content\":\"module.exports = {\\n  'raw-array': {\\n    'make': function(arr) {\\n      return arr;\\n    }\\n  }\\n};\\n\",\"timestamp\":1568484216637},{\"key\":\"prewritten/file.arr.js\",\"content\":\"const fs = require( 'fs' );\\r\\n\\r\\nmodule.exports = {\\r\\n  \\\"file-exists\\\": function( path ) {\\r\\n    return fs.existsSync( path );\\r\\n  },\\r\\n\\r\\n  \\\"file-imes\\\": function( path ) {\\r\\n    stats = fs.statSync( path );\\r\\n    return {mtime: stats.mtime, atime: stats.atime, ctime: stats.ctime};\\r\\n  },\\r\\n\\r\\n  \\\"file-to-string\\\": function( path ) {\\r\\n    return fs.readFileSync( path, 'utf8' );\\r\\n  },\\r\\n\\r\\n  \\\"real-path\\\": function( path ) {\\r\\n    return fs.realpathSync( path );\\r\\n  }\\r\\n};\\r\\n\",\"timestamp\":1568484216640},{\"key\":\"prewritten/file.arr.json\",\"content\":\"{\\r\\n  \\\"requires\\\": [],\\r\\n  \\\"provides\\\": {\\r\\n    \\\"values\\\": {\\r\\n      \\\"file-exists\\\": [\\\"arrow\\\", [\\\"String\\\"], \\\"Boolean\\\"],\\r\\n      \\\"file-times\\\": [\\\"arrow\\\", [\\\"String\\\"], [\\\"record\\\", {\\\"mtime\\\": \\\"Number\\\", \\\"atime\\\": \\\"Number\\\", \\\"ctime\\\": \\\"Number\\\"}]],\\r\\n      \\\"file-to-string\\\": [\\\"arrow\\\", [\\\"String\\\"], \\\"String\\\"],\\r\\n      \\\"real-path\\\": [\\\"arrow\\\", [\\\"String\\\"], \\\"String\\\"]\\r\\n    }\\r\\n  }\\r\\n}\",\"timestamp\":1568484216690},{\"key\":\"prewritten/global.arr.js\",\"content\":\"var runtime = require('./runtime.js');\\nvar array = require('./array.js');\\nvar assert = require('assert');\\n\\nfunction _plus(l, r) { return l + r; }\\nfunction _minus(l, r) { return l - r; }\\nfunction _times(l, r) { return l * r; }\\nfunction _divide(l, r) { return l / r; }\\nfunction _lessthan(l, r) { return l < r; }\\nfunction _greaterthan(l, r) { return l > r; }\\nfunction _lessequal(l, r) { return l <= r; }\\nfunction _greaterequal(l, r) { return l >= r; }\\n\\nfunction _not(x) { return !x; }\\n\\nfunction numToString(n) {\\n  return String(n);\\n}\\n\\nfunction timeNow() {\\n  return new Date().getTime();\\n}\\n\\nmodule.exports = {\\n  'num-to-str': numToString,\\n  'time-now' : timeNow,\\n  'js-to-string': function(v) { return String(v); },\\n  'raw-array': array['raw-array'],\\n  'display-string': function(s) { process.stdout.write(s); },\\n  \\\"console-log\\\": function(v) { console.log(v); },\\n  'assert': function( lv, rv, msg ) {\\n    if(!(lv === rv)) {\\n      throw new Error(msg);\\n    }\\n    else {\\n      return true;\\n    }\\n  },\\n  print: function(v) {\\n    process.stdout.write(String(v));\\n  },\\n  '_plus': _plus,\\n  '_minus': _minus,\\n  '_times': _times,\\n  '_divide': _divide,\\n  '_lessthan': _lessthan,\\n  '_greaterthan': _greaterthan,\\n  '_lessequal': _lessequal,\\n  '_greaterequal': _greaterequal,\\n  'not': _not,\\n\\n  'Equal': runtime['Equal'],\\n  'NotEqual': runtime['NotEqual'],\\n  'Unknown': runtime['Unknown'],\\n  'is-Equal': runtime['is-Equal'],\\n  'is-NotEqual': runtime['is-NotEqual'],\\n  'is-Unknown': runtime['is-Unknown'],\\n\\n  'equal-always': runtime['equalAlways'],\\n  'equal-always3': runtime['equalAlways3'],\\n  'identical': runtime['identical'],\\n  'identical3': runtime['identical3'],\\n  'trace-value': runtime['traceValue'],\\n\\n  // TODO(alex): Think of better way to expose runtime\\n  'runtime': runtime,\\n};\\n\",\"timestamp\":1568484216656},{\"key\":\"prewritten/global.arr.json\",\"content\":\"{\\n  \\\"requires\\\": [ ],\\n  \\\"provides\\\": {\\n    \\\"shorthands\\\": {\\n      \\\"rOfA\\\": [\\\"tyapp\\\", [\\\"local\\\", \\\"RawArray\\\"], [[\\\"tid\\\", \\\"a\\\"]]],\\n      \\\"tva\\\":      [\\\"tid\\\", \\\"a\\\"]\\n    },\\n    \\\"values\\\": {\\n      \\\"display-string\\\": [\\\"arrow\\\", [\\\"String\\\"], \\\"Nothing\\\"],\\n      \\\"js-to-string\\\": [\\\"arrow\\\", [\\\"Any\\\"], \\\"String\\\"],\\n      \\\"num-to-str\\\": [\\\"arrow\\\", [\\\"Number\\\"], \\\"String\\\"],\\n      \\\"print\\\": [\\\"forall\\\", \\\"a\\\", [\\\"arrow\\\", [\\\"tva\\\"], \\\"tva\\\"]],\\n      \\\"nothing\\\": \\\"Nothing\\\",\\n      \\\"raw-array\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"Maker\\\", [\\\"tid\\\", \\\"a\\\"], \\\"rOfA\\\"]],\\n      \\\"time-now\\\": [\\\"arrow\\\", [], \\\"Number\\\"],\\n      \\\"console-log\\\": [\\\"arrow\\\", [\\\"tany\\\"], \\\"Nothing\\\"],\\n      \\\"assert\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"tva\\\", \\\"tva\\\", \\\"String\\\"], \\\"Nothing\\\"]],\\n      \\\"_plus\\\":   {\\\"bind\\\": \\\"fun\\\",\\n                  \\\"flatness\\\": false,\\n                  \\\"name\\\": \\\"_plus\\\",\\n                  \\\"typ\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], \\\"Any\\\"]},\\n      \\\"_minus\\\":  {\\\"bind\\\": \\\"fun\\\",\\n                  \\\"flatness\\\": false,\\n                  \\\"name\\\": \\\"_plus\\\",\\n                  \\\"typ\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], \\\"Any\\\"]},\\n      \\\"_times\\\":  {\\\"bind\\\": \\\"fun\\\",\\n                  \\\"flatness\\\": false,\\n                  \\\"name\\\": \\\"_plus\\\",\\n                  \\\"typ\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], \\\"Any\\\"]},\\n      \\\"_divide\\\": {\\\"bind\\\": \\\"fun\\\",\\n                  \\\"flatness\\\": false,\\n                  \\\"name\\\": \\\"_plus\\\",\\n                  \\\"typ\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], \\\"Any\\\"]},\\n      \\\"_lessthan\\\": {\\\"bind\\\": \\\"fun\\\",\\n                  \\\"flatness\\\": false,\\n                  \\\"name\\\": \\\"_plus\\\",\\n                  \\\"typ\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], \\\"Boolean\\\"]},\\n      \\\"_lessequal\\\": {\\\"bind\\\": \\\"fun\\\",\\n                  \\\"flatness\\\": false,\\n                  \\\"name\\\": \\\"_plus\\\",\\n                  \\\"typ\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], \\\"Boolean\\\"]},\\n      \\\"_greaterthan\\\": {\\\"bind\\\": \\\"fun\\\",\\n                  \\\"flatness\\\": false,\\n                  \\\"name\\\": \\\"_plus\\\",\\n                  \\\"typ\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], \\\"Boolean\\\"]},\\n      \\\"_greaterequal\\\": {\\\"bind\\\": \\\"fun\\\",\\n                  \\\"flatness\\\": false,\\n                  \\\"name\\\": \\\"_plus\\\",\\n                  \\\"typ\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], \\\"Boolean\\\"]},\\n      \\\"not\\\": [\\\"arrow\\\", [\\\"Boolean\\\"], \\\"Boolean\\\"],\\n\\n      \\\"is-Equal\\\": [\\\"arrow\\\", [\\\"Any\\\"], \\\"Boolean\\\"],\\n      \\\"is-NotEqual\\\": [\\\"arrow\\\", [\\\"Any\\\"], \\\"Boolean\\\"],\\n      \\\"is-Unknown\\\": [\\\"arrow\\\", [\\\"Any\\\"], \\\"Boolean\\\"],\\n\\n      \\\"equal-always\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], \\\"Boolean\\\"],\\n      \\\"equal-always3\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"EqualityResult\\\"]],\\n      \\\"identical\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], \\\"Boolean\\\"],\\n      \\\"identical3\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"EqualityResult\\\"]],\\n      \\\"Equal\\\": [\\\"local\\\", \\\"EqualityResult\\\"],\\n      \\\"NotEqual\\\": [\\\"arrow\\\", [\\\"String\\\", \\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"EqualityResult\\\"]],\\n      \\\"Unknown\\\": [\\\"arrow\\\", [\\\"String\\\", \\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"EqualityResult\\\"]]\\n    },\\n    \\\"aliases\\\": {\\n      \\\"Any\\\": \\\"tany\\\",\\n      \\\"Method\\\": \\\"tany\\\",\\n      \\\"Object\\\": \\\"tany\\\",\\n      \\\"Function\\\": \\\"tany\\\",\\n      \\\"NumNonNegative\\\": \\\"Number\\\",\\n      \\\"NumNonPositive\\\": \\\"Number\\\",\\n      \\\"NumNegative\\\": \\\"Number\\\",\\n      \\\"NumPositive\\\": \\\"Number\\\",\\n      \\\"NumRational\\\": \\\"Number\\\",\\n      \\\"NumInteger\\\": \\\"Number\\\",\\n      \\\"Roughnum\\\": \\\"Number\\\",\\n      \\\"Exactnum\\\": \\\"Number\\\",\\n      \\\"Boolean\\\": \\\"Boolean\\\",\\n      \\\"Number\\\": \\\"Number\\\",\\n      \\\"String\\\": \\\"String\\\",\\n      \\\"Nothing\\\": \\\"Nothing\\\",\\n      \\\"RawArray\\\": { \\\"tag\\\": \\\"name\\\", \\n                    \\\"origin\\\": { \\\"import-type\\\": \\\"uri\\\", \\\"uri\\\": \\\"builtin://global\\\" },\\n                    \\\"name\\\": \\\"RawArray\\\" },\\n      \\\"Row\\\": { \\\"tag\\\": \\\"name\\\", \\n                    \\\"origin\\\": { \\\"import-type\\\": \\\"uri\\\", \\\"uri\\\": \\\"builtin://global\\\" },\\n                    \\\"name\\\": \\\"Row\\\" },\\n      \\\"Table\\\": { \\\"tag\\\": \\\"name\\\", \\n                    \\\"origin\\\": { \\\"import-type\\\": \\\"uri\\\", \\\"uri\\\": \\\"builtin://global\\\" },\\n                    \\\"name\\\": \\\"Table\\\" },\\n      \\\"EqualityResult\\\": [\\\"local\\\", \\\"EqualityResult\\\"]\\n    },\\n    \\\"datatypes\\\": {\\n      \\\"EqualityResult\\\": [\\n        \\\"data\\\", \\\"EqualityResult\\\", [], [\\n          [\\\"Equal\\\", {}],\\n          [\\\"NotEqual\\\",\\n            [\\n              [\\\"reason\\\", \\\"String\\\"],\\n              [\\\"value1\\\", \\\"tany\\\"],\\n              [\\\"value2\\\", \\\"tany\\\"]\\n            ], {}\\n          ],\\n          [\\\"Unknown\\\",\\n            [\\n              [\\\"reason\\\", \\\"String\\\"],\\n              [\\\"value1\\\", \\\"tany\\\"],\\n              [\\\"value2\\\", \\\"tany\\\"]\\n            ], {}\\n          ]\\n        ], {}\\n      ],\\n\\n      \\\"Number\\\": [\\\"data\\\", \\\"Number\\\", [], [], {\\n        \\\"_plus\\\": [\\\"arrow\\\", [\\\"Number\\\"], \\\"Number\\\"],\\n        \\\"_times\\\": [\\\"arrow\\\", [\\\"Number\\\"], \\\"Number\\\"],\\n        \\\"_divide\\\": [\\\"arrow\\\", [\\\"Number\\\"], \\\"Number\\\"],\\n        \\\"_minus\\\": [\\\"arrow\\\", [\\\"Number\\\"], \\\"Number\\\"],\\n        \\\"_lessthan\\\": [\\\"arrow\\\", [\\\"Number\\\"], \\\"Boolean\\\"],\\n        \\\"_lessequal\\\": [\\\"arrow\\\", [\\\"Number\\\"], \\\"Boolean\\\"],\\n        \\\"_greaterthan\\\": [\\\"arrow\\\", [\\\"Number\\\"], \\\"Boolean\\\"],\\n        \\\"_greaterequal\\\": [\\\"arrow\\\", [\\\"Number\\\"], \\\"Boolean\\\"]\\n      }],\\n      \\\"String\\\": [\\\"data\\\", \\\"String\\\", [], [], {\\n        \\\"_plus\\\": [\\\"arrow\\\", [\\\"String\\\"], \\\"String\\\"]\\n      }],\\n      \\\"Boolean\\\": [\\\"data\\\", \\\"Boolean\\\", [], [], {}],\\n      \\\"Nothing\\\": [\\\"data\\\", \\\"Nothing\\\", [], [], {}],\\n      \\\"RawArray\\\": [\\\"data\\\", \\\"RawArray\\\", [\\\"a\\\"], [], {\\n        \\\"length\\\": \\\"Number\\\"\\n      }]\\n    }\\n  }\\n}\\n\\n\",\"timestamp\":1568484216696},{\"key\":\"prewritten/image.arr.js\",\"content\":\"const jsnums = require(\\\"./js-numbers.js\\\");\\nvar RUNTIME = require('./runtime.js');\\n\\nvar hasOwnProperty = {}.hasOwnProperty;\\n\\n/* @stopify flat */\\nfunction Color(r, g, b, a) {\\n  this.red = r;\\n  this.green = g;\\n  this.blue = b;\\n  this.alpha = a;\\n}\\n\\n/* @stopify flat */\\nfunction isNum(n) { return typeof n === \\\"number\\\"; }\\n\\n//////////////////////////////////////////////////////////////////////\\nvar makeColor = /* @stopify flat */ function (r, g, b, a) {\\n  if (a === undefined) { a = 1; }\\n  if ([r, g, b, a].filter(isNum).length !== 4) {\\n    throw new Error(\\\"Internal error: non-number in makeColor argList \\\", [r, g, b, a]);\\n  }\\n  return new Color(r, g, b, a);\\n};\\n\\n\\n/* @stopify flat */\\nfunction clamp(num, min, max) {\\n  if (num < min) { return min; }\\n  else if (num > max) { return max; }\\n  else { return num; }\\n}\\nvar isColor = /* @stopify flat */ function (c) { return typeof c === \\\"object\\\" && c instanceof Color; };\\nvar colorRed = /* @stopify flat */ function (c) { return clamp(c.red); }\\nvar colorGreen = /* @stopify flat */ function (c) { return clamp(c.green); }\\nvar colorBlue = /* @stopify flat */ function (c) { return clamp(c.blue); }\\nvar colorAlpha = /* @stopify flat */ function (c) { return clamp(c.alpha); }\\n\\nvar convertColor = /* @stopify flat */ function (c) {\\n  if (typeof c === \\\"string\\\") { return colorDb.get(c); }\\n  else { return c; }\\n}\\n\\n// Color database\\nvar ColorDb = /* @stopify flat */ function () {\\n  this.colors = {};\\n  this.colorNames = {};\\n};\\n\\nColorDb.prototype.put = /* @stopify flat */ function (name, color) {\\n  this.colors[name] = color;\\n  var str = // NOTE(ben): Not flooring the numbers here, because they will all be integers anyway\\n    colorRed(color) + \\\", \\\" +\\n    colorGreen(color) + \\\", \\\" +\\n    colorBlue(color) + \\\", \\\" +\\n    colorAlpha(color);\\n  if (this.colorNames[str] === undefined) {\\n    this.colorNames[str] = name;\\n  }\\n};\\n\\nColorDb.prototype.get = /* @stopify flat */ function (name) {\\n  return this.colors[name.toString().toUpperCase()];\\n};\\n\\nColorDb.prototype.colorName = /* @stopify flat */ function colorName(colorStr) {\\n  var ans = this.colorNames[colorStr];\\n  if (ans !== undefined) ans = ans.toLowerCase();\\n  return ans;\\n}\\n\\n// FIXME: update toString to handle the primitive field values.\\n\\nvar colorDb = new ColorDb();\\ncolorDb.put(\\\"DARK-RED\\\", makeColor(139, 0, 0));\\ncolorDb.put(\\\"FIRE-BRICK\\\", makeColor(178, 34, 34));\\ncolorDb.put(\\\"DEEP-PINK\\\", makeColor(255, 20, 147));\\ncolorDb.put(\\\"INDIAN-RED\\\", makeColor(205, 92, 92));\\ncolorDb.put(\\\"MEDIUM-VIOLET-RED\\\", makeColor(199, 21, 133));\\ncolorDb.put(\\\"VIOLET-RED\\\", makeColor(208, 32, 144));\\ncolorDb.put(\\\"LIGHT-CORAL\\\", makeColor(240, 128, 128));\\ncolorDb.put(\\\"HOT-PINK\\\", makeColor(255, 105, 180));\\ncolorDb.put(\\\"PALE-VIOLET-RED\\\", makeColor(219, 112, 147));\\ncolorDb.put(\\\"LIGHT-PINK\\\", makeColor(255, 182, 193));\\ncolorDb.put(\\\"ROSY-BROWN\\\", makeColor(188, 143, 143));\\ncolorDb.put(\\\"LAVENDER-BLUSH\\\", makeColor(255, 240, 245));\\ncolorDb.put(\\\"SADDLE-BROWN\\\", makeColor(139, 69, 19));\\ncolorDb.put(\\\"DARK-ORANGE\\\", makeColor(255, 140, 0));\\ncolorDb.put(\\\"DARK-GOLDENRON\\\", makeColor(184, 134, 11));\\ncolorDb.put(\\\"SANDY-BROWN\\\", makeColor(244, 164, 96));\\ncolorDb.put(\\\"LIGHT-SALMON\\\", makeColor(255, 160, 122));\\ncolorDb.put(\\\"DARK-SALMON\\\", makeColor(233, 150, 122));\\ncolorDb.put(\\\"NAVAJO-WHITE\\\", makeColor(255, 222, 173));\\ncolorDb.put(\\\"PEACH-PUFF\\\", makeColor(255, 218, 185));\\ncolorDb.put(\\\"DARK-KHAKI\\\", makeColor(189, 183, 107));\\ncolorDb.put(\\\"PALE-GOLDENROD\\\", makeColor(238, 232, 170));\\ncolorDb.put(\\\"BLANCHE-DIAMOND\\\", makeColor(255, 235, 205));\\ncolorDb.put(\\\"MEDIUM-GOLDENROD\\\", makeColor(234, 234, 173));\\ncolorDb.put(\\\"PAPAYA-WHIP\\\", makeColor(255, 239, 213));\\ncolorDb.put(\\\"MISTY-ROSE\\\", makeColor(255, 228, 225));\\ncolorDb.put(\\\"LEMON-CHIFFON\\\", makeColor(255, 250, 205));\\ncolorDb.put(\\\"ANTIQUE-WHITE\\\", makeColor(250, 235, 215));\\ncolorDb.put(\\\"CORN-SILK\\\", makeColor(255, 248, 220));\\ncolorDb.put(\\\"LIGHT-GOLDENRON-YELLOW\\\", makeColor(250, 250, 210));\\ncolorDb.put(\\\"OLD-LACE\\\", makeColor(253, 245, 230));\\ncolorDb.put(\\\"LIGHT-YELLOW\\\", makeColor(255, 255, 224));\\ncolorDb.put(\\\"FLORAL-WHITE\\\", makeColor(255, 250, 240));\\ncolorDb.put(\\\"LAWN-GREEN\\\", makeColor(124, 252, 0));\\ncolorDb.put(\\\"GREEN-YELLOW\\\", makeColor(173, 255, 47));\\ncolorDb.put(\\\"YELLOW-GREEN\\\", makeColor(154, 205, 50));\\ncolorDb.put(\\\"MEDIUM-FOREST-GREEN\\\", makeColor(107, 142, 35));\\ncolorDb.put(\\\"OLIVE-DRAB\\\", makeColor(107, 142, 35));\\ncolorDb.put(\\\"MEDIUM-FOREST-GREEN\\\", makeColor(107, 142, 35));\\ncolorDb.put(\\\"DARK-OLIVE-GREEN\\\", makeColor(85, 107, 47));\\ncolorDb.put(\\\"DARK-SEA-GREEN\\\", makeColor(143, 188, 139));\\ncolorDb.put(\\\"DARK-GREEN\\\", makeColor(0, 100, 0));\\ncolorDb.put(\\\"LIME-GREEN\\\", makeColor(50, 205, 50));\\ncolorDb.put(\\\"FOREST-GREEN\\\", makeColor(34, 139, 34));\\ncolorDb.put(\\\"SPRING-GREEN\\\", makeColor(0, 255, 127));\\ncolorDb.put(\\\"MEDIUM-SPRING-GREEN\\\", makeColor(0, 250, 154));\\ncolorDb.put(\\\"SEA-GREEN\\\", makeColor(46, 139, 87));\\ncolorDb.put(\\\"MEDIUM-SEA-GREEN\\\", makeColor(60, 179, 113));\\ncolorDb.put(\\\"LIGHT-GREEN\\\", makeColor(144, 238, 144));\\ncolorDb.put(\\\"PALE-GREEN\\\", makeColor(152, 251, 152));\\ncolorDb.put(\\\"MEDIUM-AQUAMARINE\\\", makeColor(102, 205, 170));\\ncolorDb.put(\\\"LIGHT-SEA-GREEN\\\", makeColor(32, 178, 170));\\ncolorDb.put(\\\"MEDIUM-TURQUOISE\\\", makeColor(72, 209, 204));\\ncolorDb.put(\\\"MINT-CREAM\\\", makeColor(245, 255, 250));\\ncolorDb.put(\\\"ROYAL-BLUE\\\", makeColor(65, 105, 225));\\ncolorDb.put(\\\"DODGER-BLUE\\\", makeColor(30, 144, 255));\\ncolorDb.put(\\\"DEEP-SKY-BLUE\\\", makeColor(0, 191, 255));\\ncolorDb.put(\\\"CORNFLOWER-BLUE\\\", makeColor(100, 149, 237));\\ncolorDb.put(\\\"STEEL-BLUE\\\", makeColor(70, 130, 180));\\ncolorDb.put(\\\"LIGHT-SKY-BLUE\\\", makeColor(135, 206, 250));\\ncolorDb.put(\\\"DARK-TURQUOISE\\\", makeColor(0, 206, 209));\\ncolorDb.put(\\\"DARKTURQUOISE\\\", makeColor(0, 206, 209));\\ncolorDb.put(\\\"SKY-BLUE\\\", makeColor(135, 206, 235));\\ncolorDb.put(\\\"SKYBLUE\\\", makeColor(135, 206, 235));\\ncolorDb.put(\\\"CADET-BLUE\\\", makeColor(96, 160, 160));\\ncolorDb.put(\\\"DARK-SLATE-GRAY\\\", makeColor(47, 79, 79));\\ncolorDb.put(\\\"LIGHT-STEEL-BLUE\\\", makeColor(176, 196, 222));\\ncolorDb.put(\\\"LIGHT-BLUE\\\", makeColor(173, 216, 230));\\ncolorDb.put(\\\"POWDER-BLUE\\\", makeColor(176, 224, 230));\\ncolorDb.put(\\\"PALE-TURQUOISE\\\", makeColor(175, 238, 238));\\ncolorDb.put(\\\"LIGHT-CYAN\\\", makeColor(224, 255, 255));\\ncolorDb.put(\\\"ALICE-BLUE\\\", makeColor(240, 248, 255));\\ncolorDb.put(\\\"MEDIUM-BLUE\\\", makeColor(0, 0, 205));\\ncolorDb.put(\\\"DARK-BLUE\\\", makeColor(0, 0, 139));\\ncolorDb.put(\\\"MIDNIGHT-BLUE\\\", makeColor(25, 25, 112));\\ncolorDb.put(\\\"BLUE-VIOLET\\\", makeColor(138, 43, 226));\\ncolorDb.put(\\\"MEDIUM-SLATE-BLUE\\\", makeColor(123, 104, 238));\\ncolorDb.put(\\\"SLATE-BLUE\\\", makeColor(106, 90, 205));\\ncolorDb.put(\\\"DARK-SLATE-BLUE\\\", makeColor(72, 61, 139));\\ncolorDb.put(\\\"DARK-VIOLET\\\", makeColor(148, 0, 211));\\ncolorDb.put(\\\"DARK-ORCHID\\\", makeColor(153, 50, 204));\\ncolorDb.put(\\\"MEDIUM-PURPLE\\\", makeColor(147, 112, 219));\\ncolorDb.put(\\\"CORNFLOWER-BLUE\\\", makeColor(68, 64, 108));\\ncolorDb.put(\\\"MEDIUM-ORCHID\\\", makeColor(186, 85, 211));\\ncolorDb.put(\\\"DARK-MAGENTA\\\", makeColor(139, 0, 139));\\ncolorDb.put(\\\"GHOST-WHITE\\\", makeColor(248, 248, 255));\\ncolorDb.put(\\\"WHITE-SMOKE\\\", makeColor(245, 245, 245));\\ncolorDb.put(\\\"LIGHT-GRAY\\\", makeColor(211, 211, 211));\\ncolorDb.put(\\\"DARK-GRAY\\\", makeColor(169, 169, 169));\\ncolorDb.put(\\\"DIM-GRAY\\\", makeColor(105, 105, 105));\\n\\ncolorDb.put(\\\"ORANGE\\\", makeColor(255, 165, 0));\\ncolorDb.put(\\\"RED\\\", makeColor(255, 0, 0));\\ncolorDb.put(\\\"ORANGERED\\\", makeColor(255, 69, 0));\\ncolorDb.put(\\\"TOMATO\\\", makeColor(255, 99, 71));\\ncolorDb.put(\\\"DARKRED\\\", makeColor(139, 0, 0));\\ncolorDb.put(\\\"RED\\\", makeColor(255, 0, 0));\\ncolorDb.put(\\\"FIREBRICK\\\", makeColor(178, 34, 34));\\ncolorDb.put(\\\"CRIMSON\\\", makeColor(220, 20, 60));\\ncolorDb.put(\\\"DEEPPINK\\\", makeColor(255, 20, 147));\\ncolorDb.put(\\\"MAROON\\\", makeColor(176, 48, 96));\\ncolorDb.put(\\\"INDIAN RED\\\", makeColor(205, 92, 92));\\ncolorDb.put(\\\"INDIANRED\\\", makeColor(205, 92, 92));\\ncolorDb.put(\\\"MEDIUM VIOLET RED\\\", makeColor(199, 21, 133));\\ncolorDb.put(\\\"MEDIUMVIOLETRED\\\", makeColor(199, 21, 133));\\ncolorDb.put(\\\"VIOLET RED\\\", makeColor(208, 32, 144));\\ncolorDb.put(\\\"VIOLETRED\\\", makeColor(208, 32, 144));\\ncolorDb.put(\\\"LIGHTCORAL\\\", makeColor(240, 128, 128));\\ncolorDb.put(\\\"HOTPINK\\\", makeColor(255, 105, 180));\\ncolorDb.put(\\\"PALEVIOLETRED\\\", makeColor(219, 112, 147));\\ncolorDb.put(\\\"LIGHTPINK\\\", makeColor(255, 182, 193));\\ncolorDb.put(\\\"ROSYBROWN\\\", makeColor(188, 143, 143));\\ncolorDb.put(\\\"PINK\\\", makeColor(255, 192, 203));\\ncolorDb.put(\\\"ORCHID\\\", makeColor(218, 112, 214));\\ncolorDb.put(\\\"LAVENDERBLUSH\\\", makeColor(255, 240, 245));\\ncolorDb.put(\\\"SNOW\\\", makeColor(255, 250, 250));\\ncolorDb.put(\\\"CHOCOLATE\\\", makeColor(210, 105, 30));\\ncolorDb.put(\\\"SADDLEBROWN\\\", makeColor(139, 69, 19));\\ncolorDb.put(\\\"BROWN\\\", makeColor(132, 60, 36));\\ncolorDb.put(\\\"DARKORANGE\\\", makeColor(255, 140, 0));\\ncolorDb.put(\\\"CORAL\\\", makeColor(255, 127, 80));\\ncolorDb.put(\\\"SIENNA\\\", makeColor(160, 82, 45));\\ncolorDb.put(\\\"ORANGE\\\", makeColor(255, 165, 0));\\ncolorDb.put(\\\"SALMON\\\", makeColor(250, 128, 114));\\ncolorDb.put(\\\"PERU\\\", makeColor(205, 133, 63));\\ncolorDb.put(\\\"DARKGOLDENROD\\\", makeColor(184, 134, 11));\\ncolorDb.put(\\\"GOLDENROD\\\", makeColor(218, 165, 32));\\ncolorDb.put(\\\"SANDYBROWN\\\", makeColor(244, 164, 96));\\ncolorDb.put(\\\"LIGHTSALMON\\\", makeColor(255, 160, 122));\\ncolorDb.put(\\\"DARKSALMON\\\", makeColor(233, 150, 122));\\ncolorDb.put(\\\"GOLD\\\", makeColor(255, 215, 0));\\ncolorDb.put(\\\"YELLOW\\\", makeColor(255, 255, 0));\\ncolorDb.put(\\\"OLIVE\\\", makeColor(128, 128, 0));\\ncolorDb.put(\\\"BURLYWOOD\\\", makeColor(222, 184, 135));\\ncolorDb.put(\\\"TAN\\\", makeColor(210, 180, 140));\\ncolorDb.put(\\\"NAVAJOWHITE\\\", makeColor(255, 222, 173));\\ncolorDb.put(\\\"PEACHPUFF\\\", makeColor(255, 218, 185));\\ncolorDb.put(\\\"KHAKI\\\", makeColor(240, 230, 140));\\ncolorDb.put(\\\"DARKKHAKI\\\", makeColor(189, 183, 107));\\ncolorDb.put(\\\"MOCCASIN\\\", makeColor(255, 228, 181));\\ncolorDb.put(\\\"WHEAT\\\", makeColor(245, 222, 179));\\ncolorDb.put(\\\"BISQUE\\\", makeColor(255, 228, 196));\\ncolorDb.put(\\\"PALEGOLDENROD\\\", makeColor(238, 232, 170));\\ncolorDb.put(\\\"BLANCHEDALMOND\\\", makeColor(255, 235, 205));\\ncolorDb.put(\\\"MEDIUM GOLDENROD\\\", makeColor(234, 234, 173));\\ncolorDb.put(\\\"MEDIUMGOLDENROD\\\", makeColor(234, 234, 173));\\ncolorDb.put(\\\"PAPAYAWHIP\\\", makeColor(255, 239, 213));\\ncolorDb.put(\\\"MISTYROSE\\\", makeColor(255, 228, 225));\\ncolorDb.put(\\\"LEMONCHIFFON\\\", makeColor(255, 250, 205));\\ncolorDb.put(\\\"ANTIQUEWHITE\\\", makeColor(250, 235, 215));\\ncolorDb.put(\\\"CORNSILK\\\", makeColor(255, 248, 220));\\ncolorDb.put(\\\"LIGHTGOLDENRODYELLOW\\\", makeColor(250, 250, 210));\\ncolorDb.put(\\\"OLDLACE\\\", makeColor(253, 245, 230));\\ncolorDb.put(\\\"LINEN\\\", makeColor(250, 240, 230));\\ncolorDb.put(\\\"LIGHTYELLOW\\\", makeColor(255, 255, 224));\\ncolorDb.put(\\\"SEASHELL\\\", makeColor(255, 245, 238));\\ncolorDb.put(\\\"BEIGE\\\", makeColor(245, 245, 220));\\ncolorDb.put(\\\"FLORALWHITE\\\", makeColor(255, 250, 240));\\ncolorDb.put(\\\"IVORY\\\", makeColor(255, 255, 240));\\ncolorDb.put(\\\"GREEN\\\", makeColor(0, 255, 0));\\ncolorDb.put(\\\"LAWNGREEN\\\", makeColor(124, 252, 0));\\ncolorDb.put(\\\"CHARTREUSE\\\", makeColor(127, 255, 0));\\ncolorDb.put(\\\"GREEN YELLOW\\\", makeColor(173, 255, 47));\\ncolorDb.put(\\\"GREENYELLOW\\\", makeColor(173, 255, 47));\\ncolorDb.put(\\\"YELLOW GREEN\\\", makeColor(154, 205, 50));\\ncolorDb.put(\\\"YELLOWGREEN\\\", makeColor(154, 205, 50));\\ncolorDb.put(\\\"MEDIUM FOREST GREEN\\\", makeColor(107, 142, 35));\\ncolorDb.put(\\\"OLIVEDRAB\\\", makeColor(107, 142, 35));\\ncolorDb.put(\\\"MEDIUMFORESTGREEN\\\", makeColor(107, 142, 35));\\ncolorDb.put(\\\"DARK OLIVE GREEN\\\", makeColor(85, 107, 47));\\ncolorDb.put(\\\"DARKOLIVEGREEN\\\", makeColor(85, 107, 47));\\ncolorDb.put(\\\"DARKSEAGREEN\\\", makeColor(143, 188, 139));\\ncolorDb.put(\\\"LIME\\\", makeColor(0, 255, 0));\\ncolorDb.put(\\\"DARK GREEN\\\", makeColor(0, 100, 0));\\ncolorDb.put(\\\"DARKGREEN\\\", makeColor(0, 100, 0));\\ncolorDb.put(\\\"LIME GREEN\\\", makeColor(50, 205, 50));\\ncolorDb.put(\\\"LIMEGREEN\\\", makeColor(50, 205, 50));\\ncolorDb.put(\\\"FOREST GREEN\\\", makeColor(34, 139, 34));\\ncolorDb.put(\\\"FORESTGREEN\\\", makeColor(34, 139, 34));\\ncolorDb.put(\\\"SPRING GREEN\\\", makeColor(0, 255, 127));\\ncolorDb.put(\\\"SPRINGGREEN\\\", makeColor(0, 255, 127));\\ncolorDb.put(\\\"MEDIUM SPRING GREEN\\\", makeColor(0, 250, 154));\\ncolorDb.put(\\\"MEDIUMSPRINGGREEN\\\", makeColor(0, 250, 154));\\ncolorDb.put(\\\"SEA GREEN\\\", makeColor(46, 139, 87));\\ncolorDb.put(\\\"SEAGREEN\\\", makeColor(46, 139, 87));\\ncolorDb.put(\\\"MEDIUM SEA GREEN\\\", makeColor(60, 179, 113));\\ncolorDb.put(\\\"MEDIUMSEAGREEN\\\", makeColor(60, 179, 113));\\ncolorDb.put(\\\"AQUAMARINE\\\", makeColor(112, 216, 144));\\ncolorDb.put(\\\"LIGHTGREEN\\\", makeColor(144, 238, 144));\\ncolorDb.put(\\\"PALE GREEN\\\", makeColor(152, 251, 152));\\ncolorDb.put(\\\"PALEGREEN\\\", makeColor(152, 251, 152));\\ncolorDb.put(\\\"MEDIUM AQUAMARINE\\\", makeColor(102, 205, 170));\\ncolorDb.put(\\\"MEDIUMAQUAMARINE\\\", makeColor(102, 205, 170));\\ncolorDb.put(\\\"TURQUOISE\\\", makeColor(64, 224, 208));\\ncolorDb.put(\\\"LIGHTSEAGREEN\\\", makeColor(32, 178, 170));\\ncolorDb.put(\\\"MEDIUM TURQUOISE\\\", makeColor(72, 209, 204));\\ncolorDb.put(\\\"MEDIUMTURQUOISE\\\", makeColor(72, 209, 204));\\ncolorDb.put(\\\"HONEYDEW\\\", makeColor(240, 255, 240));\\ncolorDb.put(\\\"MINTCREAM\\\", makeColor(245, 255, 250));\\ncolorDb.put(\\\"ROYALBLUE\\\", makeColor(65, 105, 225));\\ncolorDb.put(\\\"DODGERBLUE\\\", makeColor(30, 144, 255));\\ncolorDb.put(\\\"DEEPSKYBLUE\\\", makeColor(0, 191, 255));\\ncolorDb.put(\\\"CORNFLOWERBLUE\\\", makeColor(100, 149, 237));\\ncolorDb.put(\\\"STEEL BLUE\\\", makeColor(70, 130, 180));\\ncolorDb.put(\\\"STEELBLUE\\\", makeColor(70, 130, 180));\\ncolorDb.put(\\\"LIGHTSKYBLUE\\\", makeColor(135, 206, 250));\\ncolorDb.put(\\\"DARK TURQUOISE\\\", makeColor(0, 206, 209));\\ncolorDb.put(\\\"DARKTURQUOISE\\\", makeColor(0, 206, 209));\\ncolorDb.put(\\\"CYAN\\\", makeColor(0, 255, 255));\\ncolorDb.put(\\\"AQUA\\\", makeColor(0, 255, 255));\\ncolorDb.put(\\\"DARKCYAN\\\", makeColor(0, 139, 139));\\ncolorDb.put(\\\"TEAL\\\", makeColor(0, 128, 128));\\ncolorDb.put(\\\"SKY BLUE\\\", makeColor(135, 206, 235));\\ncolorDb.put(\\\"SKYBLUE\\\", makeColor(135, 206, 235));\\ncolorDb.put(\\\"CADET BLUE\\\", makeColor(96, 160, 160));\\ncolorDb.put(\\\"CADETBLUE\\\", makeColor(95, 158, 160));\\ncolorDb.put(\\\"DARK SLATE GRAY\\\", makeColor(47, 79, 79));\\ncolorDb.put(\\\"DARKSLATEGRAY\\\", makeColor(47, 79, 79));\\ncolorDb.put(\\\"LIGHTSLATEGRAY\\\", makeColor(119, 136, 153));\\ncolorDb.put(\\\"SLATEGRAY\\\", makeColor(112, 128, 144));\\ncolorDb.put(\\\"LIGHT STEEL BLUE\\\", makeColor(176, 196, 222));\\ncolorDb.put(\\\"LIGHTSTEELBLUE\\\", makeColor(176, 196, 222));\\ncolorDb.put(\\\"LIGHT BLUE\\\", makeColor(173, 216, 230));\\ncolorDb.put(\\\"LIGHTBLUE\\\", makeColor(173, 216, 230));\\ncolorDb.put(\\\"POWDERBLUE\\\", makeColor(176, 224, 230));\\ncolorDb.put(\\\"PALETURQUOISE\\\", makeColor(175, 238, 238));\\ncolorDb.put(\\\"LIGHTCYAN\\\", makeColor(224, 255, 255));\\ncolorDb.put(\\\"ALICEBLUE\\\", makeColor(240, 248, 255));\\ncolorDb.put(\\\"AZURE\\\", makeColor(240, 255, 255));\\ncolorDb.put(\\\"MEDIUM BLUE\\\", makeColor(0, 0, 205));\\ncolorDb.put(\\\"MEDIUMBLUE\\\", makeColor(0, 0, 205));\\ncolorDb.put(\\\"DARKBLUE\\\", makeColor(0, 0, 139));\\ncolorDb.put(\\\"MIDNIGHT BLUE\\\", makeColor(25, 25, 112));\\ncolorDb.put(\\\"MIDNIGHTBLUE\\\", makeColor(25, 25, 112));\\ncolorDb.put(\\\"NAVY\\\", makeColor(36, 36, 140));\\ncolorDb.put(\\\"BLUE\\\", makeColor(0, 0, 255));\\ncolorDb.put(\\\"INDIGO\\\", makeColor(75, 0, 130));\\ncolorDb.put(\\\"BLUE VIOLET\\\", makeColor(138, 43, 226));\\ncolorDb.put(\\\"BLUEVIOLET\\\", makeColor(138, 43, 226));\\ncolorDb.put(\\\"MEDIUM SLATE BLUE\\\", makeColor(123, 104, 238));\\ncolorDb.put(\\\"MEDIUMSLATEBLUE\\\", makeColor(123, 104, 238));\\ncolorDb.put(\\\"SLATE BLUE\\\", makeColor(106, 90, 205));\\ncolorDb.put(\\\"SLATEBLUE\\\", makeColor(106, 90, 205));\\ncolorDb.put(\\\"PURPLE\\\", makeColor(160, 32, 240));\\ncolorDb.put(\\\"DARK SLATE BLUE\\\", makeColor(72, 61, 139));\\ncolorDb.put(\\\"DARKSLATEBLUE\\\", makeColor(72, 61, 139));\\ncolorDb.put(\\\"DARKVIOLET\\\", makeColor(148, 0, 211));\\ncolorDb.put(\\\"DARK ORCHID\\\", makeColor(153, 50, 204));\\ncolorDb.put(\\\"DARKORCHID\\\", makeColor(153, 50, 204));\\ncolorDb.put(\\\"MEDIUMPURPLE\\\", makeColor(147, 112, 219));\\ncolorDb.put(\\\"CORNFLOWER BLUE\\\", makeColor(68, 64, 108));\\ncolorDb.put(\\\"MEDIUM ORCHID\\\", makeColor(186, 85, 211));\\ncolorDb.put(\\\"MEDIUMORCHID\\\", makeColor(186, 85, 211));\\ncolorDb.put(\\\"MAGENTA\\\", makeColor(255, 0, 255));\\ncolorDb.put(\\\"FUCHSIA\\\", makeColor(255, 0, 255));\\ncolorDb.put(\\\"DARKMAGENTA\\\", makeColor(139, 0, 139));\\ncolorDb.put(\\\"VIOLET\\\", makeColor(238, 130, 238));\\ncolorDb.put(\\\"PLUM\\\", makeColor(221, 160, 221));\\ncolorDb.put(\\\"LAVENDER\\\", makeColor(230, 230, 250));\\ncolorDb.put(\\\"THISTLE\\\", makeColor(216, 191, 216));\\ncolorDb.put(\\\"GHOSTWHITE\\\", makeColor(248, 248, 255));\\ncolorDb.put(\\\"WHITE\\\", makeColor(255, 255, 255));\\ncolorDb.put(\\\"WHITESMOKE\\\", makeColor(245, 245, 245));\\ncolorDb.put(\\\"GAINSBORO\\\", makeColor(220, 220, 220));\\ncolorDb.put(\\\"LIGHT GRAY\\\", makeColor(211, 211, 211));\\ncolorDb.put(\\\"LIGHTGRAY\\\", makeColor(211, 211, 211));\\ncolorDb.put(\\\"SILVER\\\", makeColor(192, 192, 192));\\ncolorDb.put(\\\"GRAY\\\", makeColor(190, 190, 190));\\ncolorDb.put(\\\"DARK GRAY\\\", makeColor(169, 169, 169));\\ncolorDb.put(\\\"DARKGRAY\\\", makeColor(169, 169, 169));\\ncolorDb.put(\\\"DIM GRAY\\\", makeColor(105, 105, 105));\\ncolorDb.put(\\\"DIMGRAY\\\", makeColor(105, 105, 105));\\ncolorDb.put(\\\"BLACK\\\", makeColor(0, 0, 0));\\ncolorDb.put(\\\"TRANSPARENT\\\", makeColor(0, 0, 0, 0));\\n\\n// clone: object -> object\\n// Copies an object.  The new object should respond like the old\\n// object, including to things like instanceof.\\n\\n// NOTE(joe): There are much better ways to do this.  This is from\\n// whalesong/whalesong/js-assembler/runtime-src/baselib.js\\n// and we're keeping it for now (March 31, 2014) to avoid changing\\n// potentially fragile prototype semantics\\nvar clone = function (obj) {\\n  var property;\\n  var C = function () { };\\n  C.prototype = obj;\\n  var c = new C();\\n  for (property in obj) {\\n    if (hasOwnProperty.call(obj, property)) {\\n      c[property] = obj[property];\\n    }\\n  }\\n  return c;\\n};\\n// TODO(joe): not sufficient\\nvar equals = /* @stopify flat */ function (v1, v2) { return v1 === v2; };\\n\\nvar imageEquals = /* @stopify flat */ function (left, right) {\\n  if (!isImage(left) || !isImage(right)) { return false; }\\n  return left.equals(right);\\n}\\nvar imageDifference = /* @stopify flat */ function (left, right) {\\n  if (!isImage(left) || !isImage(right)) { return false; }\\n  return left.difference(right);\\n}\\n//////////////////////////////////////////////////////////////////////\\n\\nvar heir = Object.create;\\n\\nvar isAngle = /* @stopify flat */ function (x) {\\n  return jsnums.isReal(x) &&\\n    jsnums.greaterThanOrEqual(x, 0, RUNTIME.NumberErrbacks) &&\\n    jsnums.lessThan(x, 360, RUNTIME.NumberErrbacks);\\n};\\n\\n// Produces true if the value is a color or a color string.\\n// On the Racket side of things, this is exposed as image-color?.\\nvar isColorOrColorString = /* @stopify flat */ function (thing) {\\n  return (isColor(thing) ||\\n    ((typeof (thing) === \\\"string\\\" &&\\n      typeof (colorDb.get(thing)) != 'undefined')));\\n};\\n\\n//////////////////////////////////////////////////////////////////////\\n// colorString : hexColor Style -> rgba\\n// Style can be a number (0-1), \\\"solid\\\", \\\"outline\\\" or null\\n// The above value which is non-number is equivalent to a number 255\\nvar colorString = /* @stopify flat */ function (aColor, aStyle) {\\n  var styleAlpha = isNaN(aStyle) ? 1.0 : aStyle,\\n    cAlpha = colorAlpha(aColor);\\n  // NOTE(ben): Flooring the numbers here so that it's a valid RGBA style string\\n  return \\\"rgba(\\\" + Math.floor(colorRed(aColor)) + \\\", \\\" +\\n    Math.floor(colorGreen(aColor)) + \\\", \\\" +\\n    Math.floor(colorBlue(aColor)) + \\\", \\\" +\\n    styleAlpha * cAlpha + \\\")\\\";\\n};\\n\\n/* @stopify flat */\\nfunction RGBtoLAB(r, g, b) {\\n  /* @stopify flat */ function RGBtoXYZ(r, g, b) {\\n      /* @stopify flat */ function process(v) {\\n      v = parseFloat(v / 255);\\n      return (v > 0.04045 ? Math.pow((v + 0.055) / 1.055, 2.4) : v / 12.92) * 100;\\n    }\\n    var var_R = process(r), var_G = process(g), var_B = process(b);\\n    //Observer. = 2°, Illuminant = D65\\n    var X = var_R * 0.4124 + var_G * 0.3576 + var_B * 0.1805;\\n    var Y = var_R * 0.2126 + var_G * 0.7152 + var_B * 0.0722;\\n    var Z = var_R * 0.0193 + var_G * 0.1192 + var_B * 0.9505;\\n    return [X, Y, Z];\\n  }\\n\\n  /* @stopify flat */ function XYZtoLAB(x, y, z) {\\n    var var_X = x / 95.047;           //ref_X =  95.047   Observer= 2°, Illuminant= D65\\n    var var_Y = y / 100.000;          //ref_Y = 100.000\\n    var var_Z = z / 108.883;          //ref_Z = 108.883\\n    function process(v) { return v > 0.008856 ? Math.pow(v, 1 / 3) : (7.787 * v) + (16 / 116); }\\n    var_X = process(var_X); var_Y = process(var_Y); var_Z = process(var_Z);\\n    var CIE_L = (116 * var_Y) - 16;\\n    var CIE_a = 500 * (var_X - var_Y);\\n    var CIE_b = 200 * (var_Y - var_Z);\\n    return [CIE_L, CIE_a, CIE_b];\\n  }\\n  var xyz = RGBtoXYZ(r, g, b), lab = XYZtoLAB(xyz[0], xyz[1], xyz[2]);\\n  return { l: lab[0], a: lab[1], b: lab[2] };\\n}\\nvar colorLabs = [], colorRgbs = colorDb.colors;\\nfor (var p in colorRgbs) {\\n  if (colorRgbs.hasOwnProperty(p)) {\\n    // NOTE(ben): Not flooring numbers here, since RGBtoLAB supports float values\\n    var lab = RGBtoLAB(colorRed(colorRgbs[p]),\\n      colorGreen(colorRgbs[p]),\\n      colorBlue(colorRgbs[p]));\\n    colorLabs.push({ name: p, l: lab.l, a: lab.a, b: lab.b });\\n  }\\n}\\n\\n//////////////////////////////////////////////////////////////////////\\n// colorToSpokenString : hexColor Style -> String\\n// Describes the color using the nearest HTML color name\\n// Style can be \\\"solid\\\" (1.0), \\\"outline\\\" (1.0), a number (0-1.0) or null (1.0)\\n/* @stopify flat */ function colorToSpokenString(aColor, aStyle) {\\n  if (aStyle === 0) return \\\" transparent \\\";\\n  // NOTE(ben): Not flooring numbers here, since RGBtoLAB supports float values\\n  var lab1 = RGBtoLAB(colorRed(aColor),\\n    colorGreen(aColor),\\n    colorBlue(aColor));\\n  var distances = colorLabs.map(/* @stopify flat */ function (lab2) {\\n    return {\\n      l: lab2.l, a: lab2.a, b: lab2.b, name: lab2.name,\\n      d: Math.sqrt(Math.pow(lab1.l - lab2.l, 2)\\n        + Math.pow(lab1.a - lab2.a, 2)\\n        + Math.pow(lab1.b - lab2.b, 2))\\n    }\\n  });\\n  distances = distances.sort(/* @stopify flat */ function (a, b) { return a.d < b.d ? -1 : a.d > b.d ? 1 : 0; });\\n  var match = distances[0].name;\\n  var style = isNaN(aStyle) ? (aStyle === \\\"solid\\\" ? \\\" solid\\\" : \\\"n outline\\\") : \\\" translucent \\\";\\n  return style + \\\" \\\" + match.toLowerCase();\\n}\\n\\n\\nvar isSideCount = /* @stopify flat */ function (x) {\\n  return typeof x === \\\"number\\\" && x >= 3;\\n  // return jsnums.isInteger(x) && jsnums.greaterThanOrEqual(x, 3, RUNTIME.NumberErrbacks);\\n};\\n\\nvar isStepCount = /* @stopify flat */ function (x) {\\n  return typeof x === \\\"number\\\" && x >= 1;\\n  // return jsnums.isInteger(x) && jsnums.greaterThanOrEqual(x, 1, RUNTIME.NumberErrbacks);\\n};\\n\\nvar isPointsCount = /* @stopify flat */ function (x) {\\n  return typeof x === \\\"number\\\" && x >= 2;\\n  // return jsnums.isInteger(x) && jsnums.greaterThanOrEqual(x, 2, RUNTIME.NumberErrbacks);\\n};\\n\\n// Produces true if thing is an image-like object.\\nvar isImage = /* @stopify flat */ function (thing) {\\n  if (typeof (thing.getHeight) !== 'function')\\n    return false;\\n  if (typeof (thing.getWidth) !== 'function')\\n    return false;\\n  if (typeof (thing.getBaseline) !== 'function')\\n    return false;\\n  if (typeof (thing.updatePinhole) !== 'function')\\n    return false;\\n  if (typeof (thing.render) !== 'function')\\n    return false;\\n  return true;\\n};\\n\\n\\n// given two arrays of {x,y} structs, determine their equivalence\\nvar verticesEqual = /* @stopify flat */ function (v1, v2) {\\n  if (v1.length !== v2.length) { return false; }\\n  var v1_str = v1.map(/* @stopify flat */ function (o) { return \\\"x:\\\" + o.x + \\\",y:\\\" + o.y }).join(\\\",\\\"),\\n    v2_str = v2.map(/* @stopify flat */ function (o) { return \\\"x:\\\" + o.x + \\\",y:\\\" + o.y }).join(\\\",\\\");\\n  // v1 == rot(v2) if append(v1,v1) includes v2\\n  return (v1_str + \\\",\\\" + v1_str).includes(v2_str);\\n};\\n\\n// given an array of (x, y) pairs, unzip them into separate arrays\\nvar unzipVertices = /* @stopify flat */ function (vertices) {\\n  return {\\n    xs: vertices.map(/* @stopify flat */ function (v) { return v.x }),\\n    ys: vertices.map(/* @stopify flat */ function (v) { return v.y })\\n  };\\n};\\n// given an array of vertices, find the width of the shape\\nvar findWidth = /* @stopify flat */ function (vertices) {\\n  var xs = unzipVertices(vertices).xs;\\n  return Math.max.apply(Math, xs) - Math.min.apply(Math, xs);\\n}\\n// given an array of vertices, find the height of the shape\\nvar findHeight = /* @stopify flat */ function (vertices) {\\n  var ys = unzipVertices(vertices).ys;\\n  return Math.max.apply(Math, ys) - Math.min.apply(Math, ys);\\n}\\n// given a list of vertices and a translationX/Y, shift them\\nvar translateVertices = /* @stopify flat */ function (vertices) {\\n  var vs = unzipVertices(vertices);\\n  var translateX = -Math.min.apply(Math, vs.xs);\\n  var translateY = -Math.min.apply(Math, vs.ys);\\n  return vertices.map(/* @stopify flat */ function (v) {\\n    return { x: v.x + translateX, y: v.y + translateY };\\n  })\\n}\\n\\n\\n// Base class for all images.\\nvar BaseImage = /* @stopify flat */ function () { this.$brand = \\\"image\\\"; };\\n\\nBaseImage.prototype.updatePinhole = /* @stopify flat */ function (x, y) {\\n  var aCopy = clone(this);\\n  aCopy.pinholeX = x;\\n  aCopy.pinholeY = y;\\n  return aCopy;\\n};\\n\\nBaseImage.prototype.getHeight = /* @stopify flat */ function () {\\n  return Math.round(this.height);\\n};\\n\\nBaseImage.prototype.getWidth = /* @stopify flat */ function () {\\n  return Math.round(this.width);\\n};\\n\\nBaseImage.prototype.getBaseline = /* @stopify flat */ function () {\\n  return Math.round(this.height);\\n};\\n\\n// return the vertex array if it exists, otherwise make one using height and width\\nBaseImage.prototype.getVertices = /* @stopify flat */ function () {\\n  if (this.vertices) { return this.vertices; }\\n  else {\\n    return [{ x: 0, y: 0 },\\n    { x: this.width, y: 0 },\\n    { x: 0, y: this.height },\\n    { x: this.width, y: this.height }];\\n  }\\n};\\n\\n// render: context fixnum fixnum: -> void\\n// Render the image, where the upper-left corner of the image is drawn at\\n// (x, y).\\n// If the image isn't vertex-based, throw an error\\n// Otherwise, stroke and fill the vertices.\\nBaseImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {\\n  if (!this.vertices) {\\n    throw new Error('BaseImage.render is not implemented for this type!');\\n  }\\n  ctx.save();\\n  ctx.beginPath();\\n\\n  // we care about the stroke because drawing to a canvas is *different* for\\n  // fill v. stroke! If it's outline, we can draw on the pixel boundaries and\\n  // stroke within them. If it's stroke, we need to draw _inside_ those \\n  // boundaries, adjusting by a half-pixel towards the center.\\n  var isSolid = this.style.toString().toLowerCase() !== \\\"outline\\\";\\n\\n  var vertices;\\n  // pixel-perfect vertices fail on Chrome, and certain versions of FF,\\n  // so we disable the offset for equality tests and solid images\\n  if (ctx.isEqualityTest || isSolid) {\\n    vertices = this.vertices;\\n  } else {\\n    // find the midpoint of the xs and ys from vertices\\n    var midX = findWidth(this.vertices) / 2;\\n    var midY = findHeight(this.vertices) / 2;\\n\\n    // compute 0.5px offsets to ensure that we draw on the pixel\\n    // and not the pixel boundary\\n    vertices = this.vertices.map(/* @stopify flat */ function (v) {\\n      return {\\n        x: v.x + (v.x < midX ? 0.5 : -0.5),\\n        y: v.y + (v.y < midY ? 0.5 : -0.5)\\n      };\\n    });\\n  }\\n\\n  ctx.moveTo(x + vertices[0].x, y + vertices[0].y);\\n  vertices.forEach(/* @stopify flat */ function (v) { ctx.lineTo(x + v.x, y + v.y); });\\n  ctx.closePath();\\n\\n  if (isSolid) {\\n    ctx.fillStyle = colorString(this.color, this.style);\\n    ctx.fill();\\n  } else {\\n    ctx.strokeStyle = colorString(this.color);\\n    ctx.stroke();\\n  }\\n  ctx.restore();\\n};\\n\\n// makeCanvas: number number -> canvas\\n// Constructs a canvas object of a particular width and height.\\nvar makeCanvas = /* @stopify flat */ function (width, height) {\\n  var canvas = document.createElement(\\\"canvas\\\");\\n  canvas.width = width;\\n  canvas.height = height;\\n  canvas.style.width = canvas.width + \\\"px\\\";\\n  canvas.style.height = canvas.height + \\\"px\\\";\\n  return canvas;\\n};\\n\\n// Images are expected to define a render() method, which is used\\n// here to draw to the canvas.\\nBaseImage.prototype.toDomNode = function (params) {\\n  var that = this;\\n  var width = that.getWidth();\\n  var height = that.getHeight();\\n  var canvas = makeCanvas(width, height);\\n  var ctx;\\n  console.log(\\\"In toDomNode\\\");\\n  // KLUDGE: on IE, the canvas rendering functions depend on a\\n  // context where the canvas is attached to the DOM tree.\\n  // We initialize an afterAttach hook; the client's responsible\\n  // for calling this after the dom node is attached to the\\n  // document.\\n  var onAfterAttach = function (event) {\\n    // jQuery(canvas).unbind('afterAttach', onAfterAttach);\\n    ctx = this.getContext(\\\"2d\\\");\\n    that.render(ctx, 0, 0);\\n  };\\n  //jQuery(canvas).bind('afterAttach', onAfterAttach);\\n\\n  // Canvases lose their drawn content on cloning.  data may help us to preserve it.\\n  //jQuery(canvas).data('toRender', onAfterAttach);\\n  // ARIA: use \\\"image\\\" as default text.\\n  canvas.ariaText = this.ariaText || \\\"image\\\";\\n  return canvas;\\n};\\n\\nBaseImage.prototype.toWrittenString = /* @stopify flat */ function (cache) { return \\\"<image>\\\"; }\\nBaseImage.prototype.toDisplayedString = /* @stopify flat */ function (cache) { return \\\"<image>\\\"; }\\n\\n\\n// Best-Guess equivalence for images. If they're vertex-based we're in luck,\\n// otherwise we go pixel-by-pixel. It's up to exotic image types to provide\\n// more efficient ways of comparing one another\\nBaseImage.prototype.equals = /* @stopify flat */ function (other) {\\n  if (this.getWidth() !== other.getWidth() ||\\n    this.getHeight() !== other.getHeight()) { return false; }\\n  // if they're both vertex-based images, all we need to compare are\\n  // their styles, vertices and color\\n  if (this.vertices && other.vertices) {\\n    return (this.style === other.style &&\\n      verticesEqual(this.vertices, other.vertices) &&\\n      equals(this.color, other.color));\\n  }\\n  // if it's something more sophisticated, render both images to canvases\\n  // First check canvas dimensions, then go pixel-by-pixel\\n  var c1 = this.toDomNode(), c2 = other.toDomNode();\\n  c1.style.visibility = c2.style.visibility = \\\"hidden\\\";\\n  if (c1.width !== c2.width || c1.height !== c2.height) { return false; }\\n  try {\\n    var ctx1 = c1.getContext('2d'), ctx2 = c2.getContext('2d');\\n    ctx1.isEqualityTest = true;\\n    ctx2.isEqualityTest = true;\\n    this.render(ctx1, 0, 0); other.render(ctx2, 0, 0);\\n    // create temporary canvases\\n    var slice1 = document.createElement('canvas').getContext('2d'),\\n      slice2 = document.createElement('canvas').getContext('2d');\\n    var tileW = Math.min(10000, c1.width); // use only the largest tiles we need for these images\\n    var tileH = Math.min(10000, c1.height);\\n    for (var y = 0; y < c1.height; y += tileH) {\\n      for (var x = 0; x < c1.width; x += tileW) {\\n        tileW = Math.min(tileW, c1.width - x); // can we use smaller tiles for what's left?\\n        tileH = Math.min(tileH, c1.height - y);\\n        slice1.canvas.width = slice2.canvas.width = tileW;\\n        slice1.canvas.height = slice2.canvas.height = tileH;\\n        console.log('processing chunk from (' + x + ',' + y + ') to (' + (x + tileW) + ',' + (y + tileH) + ')');\\n        slice1.clearRect(0, 0, tileW, tileH);\\n        slice1.drawImage(c1, x, y, tileW, tileH, 0, 0, tileW, tileH);\\n        slice2.clearRect(0, 0, tileW, tileH);\\n        slice2.drawImage(c2, x, y, tileW, tileH, 0, 0, tileW, tileH);\\n        var d1 = slice1.canvas.toDataURL(),\\n          d2 = slice2.canvas.toDataURL(),\\n          h1 = md5(d1), h2 = md5(d2);\\n        if (h1 !== h2) return false;\\n      }\\n    }\\n    // Slow-path can fail with CORS or image-loading problems\\n  } catch (e) {\\n    console.log('Couldn\\\\'t compare images:', e);\\n    return false;\\n  }\\n  // if, after all this, we're still good...then they're equal!\\n  return true;\\n};\\n\\n/* Calculates the difference between two images, and returns it\\n       as a Pyret Either<String, Number>\\n       The difference is calculated from the formula at\\n       http://stackoverflow.com/questions/9136524/are-there-any-javascript-libs-to-pixel-compare-images-using-html5-canvas-or-any\\n       values in the low double digits indicate pretty similar images, in the\\n       low hundreds something is clearly off.\\n    */\\nBaseImage.prototype.difference = function (other) {\\n  if (Math.floor(this.width) !== Math.floor(other.getWidth()) ||\\n    Math.floor(this.height) !== Math.floor(other.getHeight())) {\\n    return RUNTIME.ffi.makeLeft(\\\"different-size([\\\" + this.width + \\\", \\\" + this.height + \\\"], [\\\" +\\n      other.getWidth() + \\\", \\\" + other.getHeight() + \\\"])\\\");\\n  }\\n\\n  // http://stackoverflow.com/questions/9136524/are-there-any-javascript-libs-to-pixel-compare-images-using-html5-canvas-or-any\\n  function rmsDiff(data1, data2) {\\n    var squares = 0;\\n    for (var i = 0; i < data1.length; i++) {\\n      squares += (data1[i] - data2[i]) * (data1[i] - data2[i]);\\n    }\\n    var rms = Math.sqrt(squares / data1.length);\\n    return rms;\\n  }\\n\\n  // if it's something more sophisticated, render both images to canvases\\n  // First check canvas dimensions, then go pixel-by-pixel\\n  var c1 = this.toDomNode(), c2 = other.toDomNode();\\n  c1.style.visibility = c2.style.visibility = \\\"hidden\\\";\\n  var w1 = Math.floor(c1.width),\\n    h1 = Math.floor(c1.height),\\n    w2 = Math.floor(c2.width),\\n    h2 = Math.floor(c2.height);\\n  if (w1 !== w2 || h1 !== h2) {\\n    return RUNTIME.makeLeft(\\\"different-size-dom([\\\" + c1.width + \\\", \\\" + c1.height + \\\"], [\\\" +\\n      c2.width + \\\", \\\" + c2.height + \\\"])\\\");\\n  }\\n  var ctx1 = c1.getContext('2d'), ctx2 = c2.getContext('2d');\\n  this.render(ctx1, 0, 0);\\n  other.render(ctx2, 0, 0);\\n  try {\\n    var data1 = ctx1.getImageData(0, 0, w1, h1),\\n      data2 = ctx2.getImageData(0, 0, w2, h2);\\n    var pixels1 = data1.data,\\n      pixels2 = data2.data;\\n    return RUNTIME.ffi.makeRight(rmsDiff(pixels1, pixels2));\\n  } catch (e) {\\n    // if we violate CORS, just bail\\n    return RUNTIME.ffi.makeLeft(\\\"exception: \\\" + String(e));\\n  }\\n};\\n\\nvar isMode = /* @stopify flat */ function (x) {\\n  return ((typeof (x) === 'string' || x instanceof String) &&\\n    (x.toString().toLowerCase() == \\\"solid\\\" ||\\n      x.toString().toLowerCase() == \\\"outline\\\")) ||\\n    ((jsnums.isReal(x)) &&\\n      (jsnums.greaterThanOrEqual(x, 0, RUNTIME.NumberErrbacks) &&\\n        jsnums.lessThanOrEqual(x, 1, RUNTIME.NumberErrbacks)));\\n};\\n\\nvar isPlaceX = /* @stopify flat */ function (x) {\\n  return ((typeof (x) === 'string' || x instanceof String) &&\\n    (x.toString().toLowerCase() === \\\"left\\\" ||\\n      x.toString().toLowerCase() === \\\"right\\\" ||\\n      x.toString().toLowerCase() === \\\"center\\\" ||\\n      x.toString().toLowerCase() === \\\"middle\\\"));\\n};\\n\\nvar isPlaceY = /* @stopify flat */ function (x) {\\n  return ((typeof (x) === 'string' || x instanceof String) &&\\n    (x.toString().toLowerCase() === \\\"top\\\" ||\\n      x.toString().toLowerCase() === \\\"bottom\\\" ||\\n      x.toString().toLowerCase() === \\\"baseline\\\" ||\\n      x.toString().toLowerCase() === \\\"center\\\" ||\\n      x.toString().toLowerCase() === \\\"middle\\\"));\\n};\\n\\n// isScene: any -> boolean\\n// Produces true when x is a scene.\\nvar isScene = /* @stopify flat */ function (x) {\\n  return ((x != undefined) && (x != null) && (x instanceof SceneImage));\\n};\\n\\n//////////////////////////////////////////////////////////////////////\\n// SceneImage: primitive-number primitive-number (listof image) -> Scene\\nvar SceneImage = /* @stopify flat */ function (width, height, children, withBorder) {\\n  BaseImage.call(this);\\n  this.width = width;\\n  this.height = height;\\n  this.children = children; // arrayof [image, number, number]\\n  this.withBorder = withBorder;\\n  this.ariaText = \\\" scene that is \\\" + width + \\\" by \\\" + height + \\\". children are: \\\";\\n  this.ariaText += children.map(function (c, i) {\\n    return \\\"child \\\" + (i + 1) + \\\": \\\" + c[0].ariaText + \\\", positioned at \\\" + c[1] + \\\",\\\" + c[2] + \\\" \\\";\\n  }).join(\\\". \\\");\\n};\\nSceneImage.prototype = heir(BaseImage.prototype);\\n\\n// add: image primitive-number primitive-number -> Scene\\nSceneImage.prototype.add = /* @stopify flat */ function (anImage, x, y) {\\n  return new SceneImage(this.width,\\n    this.height,\\n    this.children.concat([[anImage,\\n      x - anImage.getWidth() / 2,\\n      y - anImage.getHeight() / 2]]),\\n    this.withBorder);\\n};\\n\\n// render: 2d-context primitive-number primitive-number -> void\\nSceneImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {\\n  var childImage, childX, childY;\\n  // create a clipping region around the boundaries of the Scene\\n  ctx.save();\\n  ctx.fillStyle = \\\"rgba(0,0,0,0)\\\";\\n  ctx.fillRect(x, y, this.width, this.height);\\n  ctx.restore();\\n  // save the context, reset the path, and clip to the path around the scene edge\\n  ctx.save();\\n  ctx.beginPath();\\n  ctx.rect(x, y, this.width, this.height);\\n  ctx.clip();\\n  // Ask every object to render itself inside the region\\n  this.children.forEach(function (child) {\\n    // then, render the child images\\n    childImage = child[0];\\n    childX = child[1];\\n    childY = child[2];\\n    childImage.render(ctx, childX + x, childY + y);\\n  });\\n  // unclip\\n  ctx.restore();\\n\\n  if (this.withBorder) {\\n    ctx.strokeStyle = 'black';\\n    ctx.strokeRect(x, y, this.width, this.height);\\n  }\\n};\\n\\nSceneImage.prototype.equals = /* @stopify flat */ function (other) {\\n  return (other instanceof SceneImage &&\\n    this.width == other.width &&\\n    this.height == other.height &&\\n    this.children.length == other.children.length &&\\n    this.children.every(function (child1, i) {\\n      var child2 = other.children[i];\\n      return (child1[1] == child2[1] &&\\n        child1[2] == child2[2] &&\\n        child1[0].equals(child2[0]));\\n    }))\\n    || BaseImage.prototype.equals.call(this, other);\\n};\\n\\n//////////////////////////////////////////////////////////////////////\\n// OverlayImage: image image placeX placeY -> image\\n// Creates an image that overlays img1 on top of the\\n// other image img2.\\nvar OverlayImage = /* @stopify flat */ function (img1, img2, placeX, placeY) {\\n  BaseImage.call(this);\\n\\n  // An overlay image consists of width, height, x1, y1, x2, and\\n  // y2.  We need to compute these based on the inputs img1,\\n  // img2, placex, and placey.\\n\\n  // placeX and placeY may be non-numbers, in which case their values\\n  // depend on the img1 and img2 geometry.\\n\\n  var x1, y1, x2, y2;\\n\\n  if (placeX === \\\"left\\\") {\\n    x1 = 0;\\n    x2 = 0;\\n  } else if (placeX === \\\"right\\\") {\\n    x1 = Math.max(img1.width, img2.width) - img1.width;\\n    x2 = Math.max(img1.width, img2.width) - img2.width;\\n  } else if (placeX === \\\"beside\\\") {\\n    x1 = 0;\\n    x2 = img1.width;\\n  } else if (placeX === \\\"middle\\\" || placeX === \\\"center\\\") {\\n    x1 = Math.max(img1.width, img2.width) / 2 - img1.width / 2;\\n    x2 = Math.max(img1.width, img2.width) / 2 - img2.width / 2;\\n  } else {\\n    x1 = Math.max(placeX, 0) - placeX;\\n    x2 = Math.max(placeX, 0);\\n  }\\n\\n  if (placeY === \\\"top\\\") {\\n    y1 = 0;\\n    y2 = 0;\\n  } else if (placeY === \\\"bottom\\\") {\\n    y1 = Math.max(img1.height, img2.height) - img1.height;\\n    y2 = Math.max(img1.height, img2.height) - img2.height;\\n  } else if (placeY === \\\"above\\\") {\\n    y1 = 0;\\n    y2 = img1.height;\\n  } else if (placeY === \\\"baseline\\\") {\\n    y1 = Math.max(img1.getBaseline(), img2.getBaseline()) - img1.getBaseline();\\n    y2 = Math.max(img1.getBaseline(), img2.getBaseline()) - img2.getBaseline();\\n  } else if (placeY === \\\"middle\\\" || placeY === \\\"center\\\") {\\n    y1 = Math.max(img1.height, img2.height) / 2 - img1.height / 2;\\n    y2 = Math.max(img1.height, img2.height) / 2 - img2.height / 2;\\n  } else {\\n    y1 = Math.max(placeY, 0) - placeY;\\n    y2 = Math.max(placeY, 0);\\n  }\\n\\n  // calculate the vertices of this image by translating the vertices of the sub-images\\n  var i, v1 = img1.getVertices(), v2 = img2.getVertices(), xs = [], ys = [];\\n  v1 = v1.map(function (v) { return { x: v.x + x1, y: v.y + y1 }; });\\n  v2 = v2.map(function (v) { return { x: v.x + x2, y: v.y + y2 }; });\\n\\n  // store the vertices as something private, so this.getVertices() will still return undefined\\n  this._vertices = v1.concat(v2);\\n\\n  // store the offsets for rendering\\n  this.x1 = x1;\\n  this.y1 = y1;\\n  this.x2 = x2;\\n  this.y2 = y2;\\n  this.img1 = img1;\\n  this.img2 = img2;\\n  var positionText;\\n  if (([\\\"middle\\\", \\\"center\\\"].indexOf(placeX) > -1) && ([\\\"middle\\\", \\\"center\\\"].indexOf(placeY) > -1)) {\\n    positionText = \\\" centered above \\\";\\n  } else if (placeX === \\\"left\\\") {\\n    positionText = \\\" left-aligned \\\";\\n  } else if (placeX === \\\"right\\\") {\\n    positionText = \\\" right-aligned \\\";\\n  } else if (placeX === \\\"beside\\\") {\\n    positionText = \\\" beside \\\";\\n  } else if (!isNaN(placeX)) {\\n    positionText = \\\" shifted left by \\\" + placeX;\\n  }\\n  if (placeY === \\\"top\\\") {\\n    positionText += \\\" top-aligned \\\";\\n  } else if (placeY === \\\"bottom\\\") {\\n    positionText += \\\" bottom-aligned \\\";\\n  } else if (placeY === \\\"above\\\") {\\n    positionText += \\\" above \\\";\\n  } else if (!isNaN(placeY)) {\\n    positionText += \\\" , shifted up by \\\" + placeY;\\n  }\\n  this.width = findWidth(this._vertices);\\n  this.height = findHeight(this._vertices);\\n  this.ariaText = \\\" an overlay: first image is\\\" + img1.ariaText + positionText + img2.ariaText;\\n};\\n\\nOverlayImage.prototype = heir(BaseImage.prototype);\\n\\nOverlayImage.prototype.getVertices = /* @stopify flat */ function () { return this._vertices; };\\n\\nOverlayImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {\\n  ctx.save();\\n  this.img2.render(ctx, x + this.x2, y + this.y2);\\n  this.img1.render(ctx, x + this.x1, y + this.y1);\\n  ctx.restore();\\n};\\n\\nOverlayImage.prototype.equals = /* @stopify flat */ function (other) {\\n  return (other instanceof OverlayImage &&\\n    this.width === other.width &&\\n    this.height === other.height &&\\n    this.x1 === other.x1 &&\\n    this.y1 === other.y1 &&\\n    this.x2 === other.x2 &&\\n    this.y2 === other.y2 &&\\n    imageEquals(this.img1, other.img1) &&\\n    imageEquals(this.img2, other.img2))\\n    || BaseImage.prototype.equals.call(this, other);\\n};\\n\\n//////////////////////////////////////////////////////////////////////\\n// rotate: angle image -> image\\n// Rotates image by angle degrees in a counter-clockwise direction.\\n// TODO: special case for ellipse?\\nvar RotateImage = /* @stopify flat */ function (angle, img) {\\n  BaseImage.call(this);\\n  // optimization for trying to rotate a circle\\n  if ((img instanceof EllipseImage) && (img.width == img.height)) {\\n    angle = 0;\\n  }\\n  var sin = Math.sin(angle * Math.PI / 180);\\n  var cos = Math.cos(angle * Math.PI / 180);\\n\\n  // rotate each point as if it were rotated about (0,0)\\n  var vertices = img.getVertices().map(function (v) {\\n    return { x: v.x * cos - v.y * sin, y: v.x * sin + v.y * cos };\\n  });\\n\\n  // extract the xs and ys separately\\n  var vs = unzipVertices(vertices);\\n\\n  // store the vertices as something private, so this.getVertices() will still return undefined\\n  this._vertices = translateVertices(vertices);\\n  this.img = img;\\n  this.width = findWidth(vertices);\\n  this.height = findHeight(vertices);\\n  this.angle = Math.round(angle);\\n  this.translateX = -Math.min.apply(Math, vs.xs);\\n  this.translateY = -Math.min.apply(Math, vs.ys);\\n  this.ariaText = \\\"Rotated image, \\\" + angle + \\\" degrees: \\\" + img.ariaText;\\n};\\n\\nRotateImage.prototype = heir(BaseImage.prototype);\\n\\nRotateImage.prototype.getVertices = /* @stopify flat */ function () { return this._vertices; };\\n\\n// translate the canvas using the calculated values, then draw at the rotated (x,y) offset.\\nRotateImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {\\n  ctx.save();\\n  ctx.translate(x + this.translateX, y + this.translateY);\\n  ctx.rotate(this.angle * Math.PI / 180);\\n  this.img.render(ctx, 0, 0);\\n  ctx.restore();\\n};\\n\\nRotateImage.prototype.equals = /* @stopify flat */ function (other) {\\n  return (other instanceof RotateImage &&\\n    this.width === other.width &&\\n    this.height === other.height &&\\n    this.angle === other.angle &&\\n    this.translateX === other.translateX &&\\n    this.translateY === other.translateY &&\\n    imageEquals(this.img, other.img))\\n    || BaseImage.prototype.equals.call(this, other);\\n};\\n\\n//////////////////////////////////////////////////////////////////////\\n// FlipImage: image string -> image\\n// Flip an image either horizontally or vertically\\nvar FlipImage = /* @stopify flat */ function (img, direction) {\\n  BaseImage.call(this);\\n  this.img = img;\\n  this.width = img.getWidth();\\n  this.height = img.getHeight();\\n  this.direction = direction;\\n  this.ariaText = direction + \\\"ly flipped image: \\\" + img.ariaText;\\n};\\n\\nFlipImage.prototype = heir(BaseImage.prototype);\\n\\nFlipImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {\\n  // when flipping an image of dimension M and offset by N across an axis,\\n  // we need to translate the canvas by M+2N in the opposite direction\\n  ctx.save();\\n  if (this.direction === \\\"horizontal\\\") {\\n    ctx.scale(-1, 1);\\n    ctx.translate(-(this.width + 2 * x), 0);\\n    this.img.render(ctx, x, y);\\n  }\\n  if (this.direction === \\\"vertical\\\") {\\n    ctx.scale(1, -1);\\n    ctx.translate(0, -(this.height + 2 * y));\\n    this.img.render(ctx, x, y);\\n  }\\n  ctx.restore();\\n};\\n\\nFlipImage.prototype.equals = /* @stopify flat */ function (other) {\\n  return (other instanceof FlipImage &&\\n    this.width === other.width &&\\n    this.height === other.height &&\\n    this.direction === other.direction &&\\n    imageEquals(this.img, other.img))\\n    || BaseImage.prototype.equals.call(this, other);\\n};\\n\\n//////////////////////////////////////////////////////////////////////\\n// FrameImage: factor factor image -> image\\n// Stick a frame around the image\\nvar FrameImage = /* @stopify flat */ function (img) {\\n  BaseImage.call(this);\\n  this.img = img;\\n  this.width = img.width;\\n  this.height = img.height;\\n  this.ariaText = \\\" Framed image: \\\" + img.ariaText;\\n};\\n\\nFrameImage.prototype = heir(BaseImage.prototype);\\n\\n// scale the context, and pass it to the image's render function\\nFrameImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {\\n  ctx.save();\\n  this.img.render(ctx, x, y);\\n  ctx.beginPath();\\n  ctx.strokeStyle = \\\"black\\\";\\n  ctx.strokeRect(x, y, this.width, this.height);\\n  ctx.closePath();\\n  ctx.restore();\\n};\\n\\nFrameImage.prototype.equals = /* @stopify flat */ function (other) {\\n  return (other instanceof FrameImage &&\\n    BaseImage.prototype.equals.call(this, other))\\n    || imageEquals(this.img, other.img);\\n};\\n\\n//////////////////////////////////////////////////////////////////////\\n// CropImage: startX startY width height image -> image\\n// Crop an image\\nvar CropImage = /* @stopify flat */ function (x, y, width, height, img) {\\n  BaseImage.call(this);\\n  this.x = x;\\n  this.y = y;\\n  this.width = width;\\n  this.height = height;\\n  this.img = img;\\n  this.ariaText = \\\"Cropped image, from \\\" + x + \\\", \\\" + y + \\\" to \\\" + (x + width) + \\\", \\\" + (y + height) + \\\": \\\" + img.ariaText;\\n};\\n\\nCropImage.prototype = heir(BaseImage.prototype);\\n\\nCropImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {\\n  ctx.save();\\n  ctx.beginPath();\\n  ctx.rect(x, y, this.width, this.height);\\n  ctx.clip();\\n  ctx.translate(-this.x, -this.y);\\n  this.img.render(ctx, x, y);\\n  ctx.restore();\\n};\\n\\nCropImage.prototype.equals = /* @stopify flat */ function (other) {\\n  return (other instanceof CropImage &&\\n    this.width === other.width &&\\n    this.height === other.height &&\\n    this.x === other.x &&\\n    this.y === other.y &&\\n    imageEquals(this.img, other.img))\\n    || BaseImage.prototype.equals.call(this, other);\\n};\\n\\n//////////////////////////////////////////////////////////////////////\\n// ScaleImage: factor factor image -> image\\n// Scale an image\\nvar ScaleImage = /* @stopify flat */ function (xFactor, yFactor, img) {\\n  BaseImage.call(this);\\n  // grab the img vertices, scale them, and save the result to this_vertices\\n  this._vertices = img.getVertices().map(function (v) {\\n    return { x: v.x * xFactor, y: v.y * yFactor };\\n  });\\n\\n  this.img = img;\\n  this.width = img.width * xFactor;\\n  this.height = img.height * yFactor;\\n  this.xFactor = xFactor;\\n  this.yFactor = yFactor;\\n  this.ariaText = \\\"Scaled Image, \\\" + (xFactor === yFactor ? \\\"by \\\" + xFactor\\n    : \\\"horizontally by \\\" + xFactor + \\\" and vertically by \\\" + yFactor) + \\\". \\\" + img.ariaText;\\n};\\n\\nScaleImage.prototype = heir(BaseImage.prototype);\\n\\nScaleImage.prototype.getVertices = function () { return this._vertices; };\\n\\n// scale the context, and pass it to the image's render function\\nScaleImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {\\n  ctx.save();\\n  ctx.scale(this.xFactor, this.yFactor);\\n  this.img.render(ctx, x / this.xFactor, y / this.yFactor);\\n  ctx.restore();\\n};\\n\\nScaleImage.prototype.equals = /* @stopify flat */ function (other) {\\n  return (other instanceof ScaleImage &&\\n    this.width === other.width &&\\n    this.height === other.height &&\\n    this.xFactor === other.xFactor &&\\n    this.yFactor === other.yFactor &&\\n    imageEquals(this.img, other.img))\\n    || BaseImage.prototype.equals.call(this, other);\\n};\\n\\n\\nvar textContainer, textParent;\\n//////////////////////////////////////////////////////////////////////\\n// TextImage: String Number Color String String String String any/c -> Image\\nvar TextImage = /* @stopify flat */ function (str, size, color, face, family, style, weight, underline) {\\n  BaseImage.call(this);\\n  this.str = str;\\n  this.size = size;   // 18\\n  this.color = color;  // red\\n  this.face = face;   // Gill Sans\\n  this.family = family; // 'swiss\\n  this.style = (style === \\\"slant\\\") ? \\\"oblique\\\" : style;  // Racket's \\\"slant\\\" -> CSS's \\\"oblique\\\"\\n  this.weight = (weight === \\\"light\\\") ? \\\"lighter\\\" : weight; // Racket's \\\"light\\\" -> CSS's \\\"lighter\\\"\\n  this.underline = underline;\\n  // NOTE: we *ignore* font-family, as it causes a number of font bugs due the browser inconsistencies\\n  // example: \\\"bold italic 20px 'Times', sans-serif\\\".\\n  // Default weight is \\\"normal\\\", face is \\\"Arial\\\"\\n  this.font = (this.style + \\\" \\\" + this.weight + \\\" \\\" + this.size + \\\"px \\\" + '\\\"' + this.face + '\\\", ' + this.family);\\n\\n  // We don't trust ctx.measureText, since (a) it's buggy and (b) it doesn't measure height\\n  // based off of the amazing work at http://mudcu.be/journal/2011/01/html5-typographic-metrics/#baselineCanvas\\n  // PENDING CANVAS V5 API: http://www.whatwg.org/specs/web-apps/current-work/#textmetrics\\n\\n  // build a DOM node with the same styling as the canvas, then measure it\\n  if (textContainer === undefined) {\\n    textContainer = document.createElement(\\\"div\\\");\\n    textContainer.style.cssText = \\\"position: absolute; top: 0px; left: 0px; visibility: hidden; white-space: pre;\\\";\\n    textParent = document.createElement(\\\"span\\\");\\n    textParent.style.display = \\\"inline\\\";\\n    textContainer.appendChild(textParent);\\n    document.body.appendChild(textContainer);\\n  }\\n  textParent.style.font = this.font;                // use the same font settings as the context\\n  textParent.textContent = str; // this will blow away any old content\\n\\n  // getting (more accurate) css equivalent of ctx.measureText()\\n  var bounds = textParent.getBoundingClientRect(); // make a single blocking call\\n  this.width = bounds.width;\\n  this.height = bounds.height;\\n  this.alphaBaseline = 0;\\n\\n  this.ariaText = \\\" the string \\\" + str + \\\", colored \\\" + colorToSpokenString(color, 'solid') + \\\" of size \\\" + size;\\n};\\n\\nTextImage.prototype = heir(BaseImage.prototype);\\n\\nTextImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {\\n  ctx.save();\\n  ctx.textAlign = 'left';\\n  ctx.textBaseline = 'top';\\n  ctx.font = this.font;\\n\\n  // if 'outline' is enabled, use strokeText. Otherwise use fillText\\n  ctx.fillStyle = this.outline ? 'white' : colorString(this.color);\\n  ctx.fillText(this.str, x, y);\\n  if (this.outline) {\\n    ctx.strokeStyle = colorString(this.color);\\n    ctx.strokeText(this.str, x, y);\\n  }\\n  if (this.underline) {\\n    ctx.beginPath();\\n    ctx.moveTo(x, y + this.size);\\n    // we use this.size, as it is more accurate for underlining than this.height\\n    ctx.lineTo(x + this.width, y + this.size);\\n    ctx.closePath();\\n    ctx.strokeStyle = colorString(this.color);\\n    ctx.stroke();\\n  }\\n  ctx.restore();\\n};\\n\\nTextImage.prototype.getBaseline = /* @stopify flat */ function () {\\n  return this.alphaBaseline;\\n};\\n\\nTextImage.prototype.equals = /* @stopify flat */ function (other) {\\n  return (other instanceof TextImage &&\\n    this.str === other.str &&\\n    this.size === other.size &&\\n    this.face === other.face &&\\n    this.family === other.family &&\\n    this.style === other.style &&\\n    this.weight === other.weight &&\\n    this.font === other.font &&\\n    this.underline === other.underline &&\\n    equals(this.color, other.color))\\n    || BaseImage.prototype.equals.call(this, other);\\n};\\n\\n/////////////////////////////////////////////////////////////////////\\n//TriangleImage: Number Number Number Mode Color -> Image\\n// Draws a triangle with the base = sideC, and the angle between sideC\\n// and sideB being angleA\\n// See http://docs.racket-lang.org/teachpack/2htdpimage.html#(def._((lib._2htdp/image..rkt)._triangle))\\nvar TriangleImage = /* @stopify flat */ function (sideC, angleA, sideB, style, color) {\\n  BaseImage.call(this);\\n  var thirdX = sideB * Math.cos(angleA * Math.PI / 180);\\n  var thirdY = sideB * Math.sin(angleA * Math.PI / 180);\\n  var offsetX = 0 - Math.min(0, thirdX); // angleA could be obtuse\\n\\n  var vertices = [];\\n  // if angle < 180 start at the top of the canvas, otherwise start at the bottom\\n  if (thirdY > 0) {\\n    vertices.push({ x: offsetX + 0, y: 0 });\\n    vertices.push({ x: offsetX + sideC, y: 0 });\\n    vertices.push({ x: offsetX + thirdX, y: thirdY });\\n  } else {\\n    vertices.push({ x: offsetX + 0, y: -thirdY });\\n    vertices.push({ x: offsetX + sideC, y: -thirdY });\\n    vertices.push({ x: offsetX + thirdX, y: 0 });\\n  }\\n\\n  this.width = Math.max(sideC, thirdX) + offsetX;\\n  this.height = Math.abs(thirdY);\\n  this.style = style;\\n  this.color = color;\\n  this.vertices = vertices;\\n  this.ariaText = \\\" a\\\" + colorToSpokenString(color, style) + \\\" triangle whose base is of length \\\" + sideC\\n    + \\\", with an angle of \\\" + (angleA % 180) + \\\" degrees between it and a side of length \\\" + sideB;\\n};\\nTriangleImage.prototype = heir(BaseImage.prototype);\\n\\nvar less = function (lhs, rhs) {\\n  return (rhs - lhs) > 0.00001;\\n};\\n\\n// excess : compute the Euclidean excess\\n//  Note: If the excess is 0, then C is 90 deg.\\n//        If the excess is negative, then C is obtuse.\\n//        If the excess is positive, then C is acuse.\\nfunction excess(sideA, sideB, sideC) {\\n  return sideA * sideA + sideB * sideB - sideC * sideC;\\n};\\n\\nvar TriangleSSS = /* @stopify flat */ function (sideA, sideB, sideC, style, color) {\\n  if (less(sideA + sideB, sideC) ||\\n    less(sideB + sideC, sideA) ||\\n    less(sideA + sideC, sideB)) {\\n    throw new Error(\\\"The given sides will not form a triangle: \\\"\\n      + sideA + \\\", \\\" + sideB + \\\", \\\" + sideC);\\n  }\\n  var angleA = Math.acos(excess(sideB, sideC, sideA)\\n    / (2 * sideB * sideC)) * (180 / Math.PI);\\n\\n  return new TriangleImage(sideC, angleA, sideB, style, color);\\n};\\n\\nvar TriangleASS = /* @stopify flat */ function (angleA, sideB, sideC, style, color) {\\n  if (less(180, angleA)) {\\n    throw new Error(\\\"The given angle, side and side will not form a triangle: \\\"\\n      + angleA + \\\", \\\" + sideB + \\\", \\\" + sideC);\\n  }\\n\\n  return new TriangleImage(sideC, angleA, sideB, style, color);\\n};\\n\\n// return c^2 = a^2 + b^2 - 2ab cos(C)\\nfunction cosRel(sideA, sideB, angleC) {\\n  return (sideA * sideA) + (sideB * sideB) - (2 * sideA * sideB * Math.cos(angleC * Math.PI / 180));\\n}\\n\\nvar TriangleSAS = /* @stopify flat */ function (sideA, angleB, sideC, style, color) {\\n  var sideB2 = cosRel(sideA, sideC, angleB);\\n  var sideB = Math.sqrt(sideB2);\\n\\n  if (sideB2 <= 0) {\\n    throw new Error(\\\"The given side, angle and side will not form a triangle: \\\"\\n      + sideA + \\\", \\\" + angleB + \\\", \\\" + sideC);\\n  } else {\\n    if (less(sideA + sideC, sideB) ||\\n      less(sideB + sideC, sideA) ||\\n      less(sideA + sideB, sideC)) {\\n      throw new Error(\\\"The given side, angle and side will not form a triangle: \\\"\\n        + sideA + \\\", \\\" + angleB + \\\", \\\" + sideC);\\n    } else {\\n      if (less(sideA + sideC, sideB) ||\\n        less(sideB + sideC, sideA) ||\\n        less(sideA + sideB, sideC)) {\\n        throw new Error(\\\"The given side, angle and side will not form a triangle: \\\"\\n          + sideA + \\\", \\\" + angleB + \\\", \\\" + sideC);\\n      }\\n    }\\n  }\\n\\n  var angleA = Math.acos(excess(sideB, sideC, sideA) / (2 * sideB * sideC))\\n    * (180 / Math.PI);\\n\\n  return new TriangleImage(sideC, angleA, sideB, style, color);\\n};\\n\\nvar TriangleSSA = /* @stopify flat */ function (sideA, sideB, angleC, style, color) {\\n  if (less(180, angleC)) {\\n    throw new Error(\\\"The given side, side and angle will not form a triangle: \\\"\\n      + sideA + \\\", \\\" + sideB + \\\", \\\" + angleC);\\n  }\\n  var sideC2 = cosRel(sideA, sideB, angleC);\\n  var sideC = Math.sqrt(sideC2);\\n\\n  if (sideC2 <= 0) {\\n    throw new Error(\\\"The given side, side and angle will not form a triangle: \\\"\\n      + sideA + \\\", \\\" + sideB + \\\", \\\" + angleC);\\n  } else {\\n    if (less(sideA + sideB, sideC) ||\\n      less(sideC + sideB, sideA) ||\\n      less(sideA + sideC, sideB)) {\\n      throw new Error(\\\"The given side, side and angle will not form a triangle: \\\"\\n        + sideA + \\\", \\\" + sideB + \\\", \\\" + angleC);\\n    }\\n  }\\n\\n  var angleA = Math.acos(excess(sideB, sideC, sideA) / (2 * sideB * sideC))\\n    * (180 / Math.PI);\\n\\n  return new TriangleImage(sideC, angleA, sideB, style, color);\\n};\\n\\nvar TriangleAAS = /* @stopify flat */ function (angleA, angleB, sideC, style, color) {\\n  var angleC = (180 - angleA - angleB);\\n  if (less(angleC, 0)) {\\n    throw new Error(\\\"The given angle, angle and side will not form a triangle: \\\"\\n      + angleA + \\\", \\\" + angleB + \\\", \\\" + sideC);\\n  }\\n  var hypotenuse = sideC / (Math.sin(angleC * Math.PI / 180))\\n  var sideB = hypotenuse * Math.sin(angleB * Math.PI / 180);\\n\\n  return new TriangleImage(sideC, angleA, sideB, style, color);\\n};\\n\\nvar TriangleASA = /* @stopify flat */ function (angleA, sideB, angleC, style, color) {\\n  var angleB = 180 - angleA - angleC;\\n  if (less(angleB, 0)) {\\n    throw new Error(\\\"The given angle, side and angle will not form a triangle: \\\"\\n      + angleA + \\\", \\\" + sideB + \\\", \\\" + angleC);\\n  }\\n  var base = (sideB * Math.sin(angleA * Math.PI / 180))\\n    / (Math.sin(angleB * Math.PI / 180));\\n  var sideC = (sideB * Math.sin(angleC * Math.PI / 180))\\n    / (Math.sin(angleB * Math.PI / 180));\\n\\n  return new TriangleImage(sideC, angleA, sideB, style, color);\\n};\\n\\nvar TriangleSAA = /* @stopify flat */ function (sideA, angleB, angleC, style, color) {\\n  var angleA = (180 - angleC - angleB);\\n  if (less(angleA, 0)) {\\n    throw new Error(\\\"The given side, angle and angle will not form a triangle: \\\"\\n      + sideA + \\\", \\\" + angleB + \\\", \\\" + angleC);\\n  }\\n  var hypotenuse = sideA / (Math.sin(angleA * Math.PI / 180));\\n  var sideC = hypotenuse * Math.sin(angleC * Math.PI / 180);\\n  var sideB = hypotenuse * Math.sin(angleB * Math.PI / 180);\\n\\n  return new TriangleImage(sideC, angleA, sideB, style, color);\\n};\\n\\n//////////////////////////////////////////////////////////////////////\\n//Ellipse : Number Number Mode Color -> Image\\nvar EllipseImage = /* @stopify flat */ function (width, height, style, color) {\\n  BaseImage.call(this);\\n  this.width = width;\\n  this.height = height;\\n  this.style = style;\\n  this.color = color;\\n  this.ariaText = \\\" a\\\" + colorToSpokenString(color, style) + ((width === height) ? \\\" circle of radius \\\" + (width / 2)\\n    : \\\" ellipse of width \\\" + width + \\\" and height \\\" + height);\\n};\\n\\nEllipseImage.prototype = heir(BaseImage.prototype);\\n\\nEllipseImage.prototype.render = /* @stopify flat */ function (ctx, aX, aY) {\\n  ctx.save();\\n  ctx.beginPath();\\n\\n  // if it's a solid ellipse...\\n  var isSolid = this.style.toString().toLowerCase() !== \\\"outline\\\";\\n  var adjust = isSolid ? 0 : 0.5;\\n  // ...account for the 1px border width\\n  var width = this.width - 2 * adjust, height = this.height - 2 * adjust;\\n  aX += adjust; aY += adjust;\\n\\n  // Most of this code is taken from:\\n  // http://webreflection.blogspot.com/2009/01/ellipse-and-circle-for-canvas-2d.html\\n  var hB = (width / 2) * 0.5522848,\\n    vB = (height / 2) * 0.5522848,\\n    eX = aX + width,\\n    eY = aY + height,\\n    mX = aX + width / 2,\\n    mY = aY + height / 2;\\n  ctx.moveTo(aX, mY);\\n  ctx.bezierCurveTo(aX, mY - vB, mX - hB, aY, mX, aY);\\n  ctx.bezierCurveTo(mX + hB, aY, eX, mY - vB, eX, mY);\\n  ctx.bezierCurveTo(eX, mY + vB, mX + hB, eY, mX, eY);\\n  ctx.bezierCurveTo(mX - hB, eY, aX, mY + vB, aX, mY);\\n  ctx.closePath();\\n  if (this.style.toString().toLowerCase() === \\\"outline\\\") {\\n    ctx.strokeStyle = colorString(this.color);\\n    ctx.stroke();\\n  } else {\\n    ctx.fillStyle = colorString(this.color, this.style);\\n    ctx.fill();\\n  }\\n\\n  ctx.restore();\\n};\\n\\nEllipseImage.prototype.equals = /* @stopify flat */ function (other) {\\n  return ((other instanceof EllipseImage) &&\\n    this.width === other.width &&\\n    this.height === other.height &&\\n    this.style === other.style &&\\n    equals(this.color, other.color))\\n    || BaseImage.prototype.equals.call(this, other);\\n};\\n\\n//////////////////////////////////////////////////////////////////////\\n// RectangleImage: Number Number Mode Color -> Image\\nvar RectangleImage = /* @stopify flat */ function (width, height, style, color) {\\n  BaseImage.call(this);\\n  this.width = Math.max(1, width);   // an outline rectangle with no delta X or delta Y\\n  this.height = Math.max(1, height);  // should still take up one visible pixel\\n  this.style = style;\\n  this.color = color;\\n  this.vertices = [{ x: 0, y: height }, { x: 0, y: 0 }, { x: width, y: 0 }, { x: width, y: height }];\\n  this.ariaText = \\\" a\\\" + colorToSpokenString(color, style) + ((width === height) ? \\\" square of size \\\" + width\\n    : \\\" rectangle of width \\\" + width + \\\" and height \\\" + height);\\n};\\nRectangleImage.prototype = heir(BaseImage.prototype);\\n\\n//////////////////////////////////////////////////////////////////////\\n// RhombusImage: Number Number Mode Color -> Image\\nvar RhombusImage = /* @stopify flat */ function (side, angle, style, color) {\\n  BaseImage.call(this);\\n  // sin(angle/2-in-radians) * side = half of base\\n  // cos(angle/2-in-radians) * side = half of height\\n  this.width = Math.sin(angle / 2 * Math.PI / 180) * side * 2;\\n  this.height = Math.abs(Math.cos(angle / 2 * Math.PI / 180)) * side * 2;\\n  this.style = style;\\n  this.color = color;\\n  this.vertices = [{ x: this.width / 2, y: 0 },\\n  { x: this.width, y: this.height / 2 },\\n  { x: this.width / 2, y: this.height },\\n  { x: 0, y: this.height / 2 }];\\n  this.ariaText = \\\" a\\\" + colorToSpokenString(color, style) + \\\" rhombus of size \\\" + side + \\\" and angle \\\" + angle;\\n};\\nRhombusImage.prototype = heir(BaseImage.prototype);\\n\\n//////////////////////////////////////////////////////////////////////\\n// Line: Number Number Color Boolean -> Image\\nvar LineImage = /* @stopify flat */ function (x, y, color) {\\n  BaseImage.call(this);\\n  var vertices;\\n  if (x >= 0) {\\n    if (y >= 0) { vertices = [{ x: 0, y: 0 }, { x: x, y: y }]; }\\n    else { vertices = [{ x: 0, y: -y }, { x: x, y: 0 }]; }\\n  } else {\\n    if (y >= 0) { vertices = [{ x: -x, y: 0 }, { x: 0, y: y }]; }\\n    else { vertices = [{ x: -x, y: -y }, { x: 0, y: 0 }]; }\\n  }\\n\\n  this.width = Math.max(1, Math.abs(x)); // a line with no delta X should still take up one visible pixel\\n  this.height = Math.max(1, Math.abs(y)); // a line with no delta Y should still take up one visible pixel\\n  this.style = \\\"outline\\\"; // all vertex-based images must have a style\\n  this.color = color;\\n  this.vertices = vertices;\\n  this.ariaText = \\\" a\\\" + colorToSpokenString(color, 'solid') + \\\" line of width \\\" + x + \\\" and height \\\" + y;\\n};\\nLineImage.prototype = heir(BaseImage.prototype);\\n\\n//////////////////////////////////////////////////////////////////////\\n// StarImage: fixnum fixnum fixnum color -> image\\n// Most of this code here adapted from the Canvas tutorial at:\\n// http://developer.apple.com/safari/articles/makinggraphicswithcanvas.html\\nvar StarImage = /* @stopify flat */ function (points, outer, inner, style, color) {\\n  BaseImage.call(this);\\n  var maxRadius = Math.max(inner, outer);\\n  var vertices = [];\\n\\n  var oneDegreeAsRadian = Math.PI / 180;\\n  for (var pt = 0; pt < (points * 2) + 1; pt++) {\\n    var rads = ((360 / (2 * points)) * pt) * oneDegreeAsRadian - 0.5;\\n    var whichRadius = (pt % 2 === 1) ? outer : inner;\\n    vertices.push({\\n      x: maxRadius + (Math.sin(rads) * whichRadius),\\n      y: maxRadius + (Math.cos(rads) * whichRadius)\\n    });\\n  }\\n  // calculate width and height of the bounding box\\n  this.width = findWidth(vertices);\\n  this.height = findHeight(vertices);\\n  this.style = style;\\n  this.color = color;\\n  this.vertices = translateVertices(vertices);\\n  this.ariaText = \\\" a\\\" + colorToSpokenString(color, style) + \\\", \\\" + points +\\n    \\\"pointed star with inner radius \\\" + inner + \\\" and outer radius \\\" + outer;\\n};\\nStarImage.prototype = heir(BaseImage.prototype);\\n\\n//////////////////////////////////////////////////////////////////////\\n// PolygonImage: Number Count Step Mode Color -> Image\\n//\\n// See http://www.algebra.com/algebra/homework/Polygons/Inscribed-and-circumscribed-polygons.lesson\\n// the polygon is inscribed in a circle, whose radius is length/2sin(pi/count)\\n// another circle is inscribed in the polygon, whose radius is length/2tan(pi/count)\\n// rotate a 3/4 quarter turn plus half the angle length to keep bottom base level\\nvar PolygonImage = /* @stopify flat */ function (length, count, step, style, color) {\\n  BaseImage.call(this);\\n  this.outerRadius = Math.round(length / (2 * Math.sin(Math.PI / count)));\\n  this.innerRadius = Math.round(length / (2 * Math.tan(Math.PI / count)));\\n  var adjust = (3 * Math.PI / 2) + Math.PI / count;\\n\\n  // rotate around outer circle, storing x and y coordinates\\n  var radians = 0, vertices = [];\\n  for (var i = 0; i < count; i++) {\\n    radians = radians + (step * 2 * Math.PI / count);\\n    vertices.push({\\n      x: Math.round(this.outerRadius * Math.cos(radians - adjust)),\\n      y: Math.round(this.outerRadius * Math.sin(radians - adjust))\\n    });\\n  }\\n\\n  this.width = findWidth(vertices);\\n  this.height = findHeight(vertices);\\n  this.style = style;\\n  this.color = color;\\n  this.vertices = translateVertices(vertices);\\n  this.ariaText = \\\" a\\\" + colorToSpokenString(color, style) + \\\", \\\" + count\\n    + \\\" sided polygon with each side of length \\\" + length;\\n};\\nPolygonImage.prototype = heir(BaseImage.prototype);\\n\\nvar colorAtPosition = /* @stopify flat */ function (img, x, y) {\\n  var width = img.getWidth(),\\n    height = img.getHeight(),\\n    canvas = makeCanvas(width, height),\\n    ctx = canvas.getContext(\\\"2d\\\"),\\n    r, g, b, a;\\n\\n  if (x >= width) {\\n    throw new Error(\\\"color-at-position: The given x coordinate, \\\" + x\\n      + \\\", must be between 0 (inclusive) and the image width (exclusive), which is \\\" + img.getWidth());\\n  }\\n  if (y >= height) {\\n    throw new Error(\\\"color-at-position: The given y coordinate, \\\" + y\\n      + \\\", must be between 0 (inclusive) and the image height (exclusive), which is \\\" + img.getHeight());\\n  }\\n\\n  img.render(ctx, 0, 0);\\n  imageData = ctx.getImageData(0, 0, width, height);\\n  data = imageData.data,\\n    index = (y * width + x) * 4;\\n\\n  r = data[index]\\n  g = data[index + 1];\\n  b = data[index + 2];\\n  a = data[index + 3] / 255;\\n\\n  return makeColor(r, g, b, a);\\n};\\n\\nvar imageToColorList = /* @stopify flat */ function (img) {\\n  var width = img.getWidth(),\\n    height = img.getHeight(),\\n    canvas = makeCanvas(width, height),\\n    ctx = canvas.getContext(\\\"2d\\\"),\\n    imageData,\\n    data,\\n    i,\\n    r, g, b, a;\\n  img.render(ctx, 0, 0);\\n  imageData = ctx.getImageData(0, 0, width, height);\\n  data = imageData.data;\\n  var colors = [];\\n  for (i = 0; i < data.length; i += 4) {\\n    r = data[i];\\n    g = data[i + 1];\\n    b = data[i + 2];\\n    a = data[i + 3] / 255;\\n    colors.push(makeColor(r, g, b, a));\\n  }\\n  return colors;\\n};\\n\\nvar colorListToImage = /* @stopify flat */ function (listOfColors,\\n  width,\\n  height,\\n  pinholeX,\\n  pinholeY) {\\n  // make list of color names to list of colors\\n  var lOfC = [];\\n  if (typeof listOfColors[0] === \\\"string\\\") {\\n    for (let i = 0; i < listOfColors.length; i++) {\\n      lOfC.push(colorDb.get(String(listOfColors[i])));\\n    }\\n  } else if (isColor(listOfColors[0])) {\\n    lOfC = listOfColors;\\n  } else {\\n    throw new Error(\\\"List is not made of Colors or name of colors\\\");\\n  }\\n  var canvas = makeCanvas(jsnums.toFixnum(width),\\n    jsnums.toFixnum(height)),\\n    ctx = canvas.getContext(\\\"2d\\\"),\\n    imageData = ctx.createImageData(jsnums.toFixnum(width),\\n      jsnums.toFixnum(height)),\\n    aColor,\\n    data = imageData.data,\\n    jsLOC = lOfC;\\n  for (var i = 0; i < jsLOC.length * 4; i += 4) {\\n    aColor = jsLOC[i / 4];\\n    // NOTE(ben): Flooring colors here to make this a proper RGBA image\\n    data[i] = Math.floor(colorRed(aColor));\\n    data[i + 1] = Math.floor(colorGreen(aColor));\\n    data[i + 2] = Math.floor(colorBlue(aColor));\\n    data[i + 3] = colorAlpha(aColor) * 255;\\n  }\\n\\n  return new ImageDataImage(imageData);\\n};\\n\\n//////////////////////////////////////////////////////////////////////\\n// ImageDataImage: imageData -> image\\n// Given an array of pixel data, create an image\\nvar ImageDataImage = /* @stopify flat */ function (imageData) {\\n  BaseImage.call(this);\\n  this.imageData = imageData;\\n  this.width = imageData.width;\\n  this.height = imageData.height;\\n};\\n\\nImageDataImage.prototype = heir(BaseImage.prototype);\\n\\nImageDataImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {\\n  ctx.putImageData(this.imageData, x, y);\\n};\\n\\nvar PutImage = /* @stopify flat */ function (img, x, y, bg) {\\n  if (isScene(bg)) {\\n    return bg.add(img, x, bg.getHeight() - y);\\n  } else {\\n    var newScene = new ScaleImage(bg.getWidth(), bg.getHeight(), [], false);\\n    newScene = newScene.add(bg, bg.getWidth() / 2, bg.getHeight() / 2);\\n    newScene = newScene.add(img, x, bg.getHeight() - y);\\n    return newScene;\\n  }\\n};\\n\\nvar PlaceImage = /* @stopify flat */ function (img, x, y, bg) {\\n  if (isScene(bg)) {\\n    return bg.add(img, x, y);\\n  } else {\\n    var newScene = new ScaleImage(bg.getWidth(), bg.getHeight(), [], false);\\n    newScene = newScene.add(bg, bg.getWidth() / 2, bg.getHeight() / 2);\\n    newScene = newScene.add(img, x, y);\\n    return newScene;\\n  }\\n};\\n\\nvar PlaceImageAlign = /* @stopify flat */ function (img, x, y, placeX, placeY, bg) {\\n  if (placeX == \\\"left\\\") { x = x + img.getWidth() / 2; }\\n  else if (placeX == \\\"right\\\") { x = x - img.getWidth() / 2; }\\n  if (placeY == \\\"top\\\") { y = y + img.getHeight() / 2; }\\n  else if (placeY == \\\"bottom\\\") { y = y - img.getHeight() / 2; }\\n\\n  if (isScene(bg)) {\\n    return bg.add(img, x, y);\\n  } else {\\n    var newScene = new ScaleImage(bg.getWidth(), bg.getHeight(), [], false);\\n    newScene = newScene.add(bg, bg.getWidth() / 2, bg.getHeight() / 2);\\n    newScene = newScene.add(img, x, y);\\n    return newScene;\\n  }\\n};\\n\\nvar SceneLineImage = /* @stopify flat */ function (img, x1, y1, x2, y2, color) {\\n  var line = new LineImage(x2 - x1, y2 - y1, color);\\n  var newScene = new SceneImage(img.getWidth(), img.getHeight(), [], true);\\n  newScene = newScene.add(img, img.getWidth()/2, img.getHeight()/2);\\n  return newScene.add(line, line.getWidth()/2+Math.min(x1, x2),\\n    line.getHeight()/2+Math.min(y1, y2));\\n};\\n\\nvar ImageUrlImage = function (url) {\\n  return RUNTIME.pauseStack(function (restarter) {\\n    console.log(\\\"Before rawImage = Image\\\");\\n    var rawImage = Image();\\n    console.log(\\\"After rawImage = Image: \\\", rawImage);\\n    debugger;\\n    /*if (RUNTIME.hasParam(\\\"imgUrlProxy\\\")) {\\n      url = RUNTIME.getParam(\\\"imgUrlProxy\\\")(url);\\n    }*/\\n    rawImage.onload = function () {\\n      restarter.resume(new FileImage(String(url), rawImage));\\n    };\\n    rawImage.onerror = function (e) {\\n      restarter.error(new Error(\\\"unable to load \\\" + url + \\\": \\\" + e.message));\\n    };\\n    rawImage.src = String(url);\\n  });\\n};\\n\\n//////////////////////////////////////////////////////////////////////\\n// FileImage: string node -> Image\\nvar FileImage = /* @stopify flat */ function (src, rawImage) {\\n  BaseImage.call(this);\\n  var self = this;\\n  this.src = src;\\n  this.isLoaded = false;\\n  this.ariaText = \\\" image file from \\\" + decodeURIComponent(src).slice(16);\\n\\n  // animationHack: see installHackToSupportAnimatedGifs() for details.\\n  this.animationHackImg = undefined;\\n\\n  if (rawImage && rawImage.complete) {\\n    this.img = rawImage;\\n    this.isLoaded = true;\\n    self.width = self.img.width;\\n    self.height = self.img.height;\\n  } else {\\n    // fixme: we may want to do something blocking here for\\n    // onload, since we don't know at this time what the file size\\n    // should be, nor will drawImage do the right thing until the\\n    // file is loaded.\\n    this.img = Image();\\n    this.img.onload = function () {\\n      self.isLoaded = true;\\n      self.width = self.img.width;\\n      self.height = self.img.height;\\n    };\\n    this.img.onerror = function (e) {\\n      self.img.onerror = \\\"\\\";\\n      self.img.src = \\\"http://www.wescheme.org/images/broken.png\\\";\\n    }\\n    this.img.src = src;\\n  }\\n}\\nFileImage.prototype = heir(BaseImage.prototype);\\n\\nvar imageCache = {};\\nFileImage.makeInstance = /* @stopify flat */ function (path, rawImage) {\\n  if (!(path in imageCache)) {\\n    imageCache[path] = new FileImage(path, rawImage);\\n  }\\n  return imageCache[path];\\n};\\n\\nFileImage.installInstance = /* @stopify flat */ function (path, rawImage) {\\n  imageCache[path] = new FileImage(path, rawImage);\\n};\\n\\nFileImage.installBrokenImage = /* @stopify flat */ function (path) {\\n  imageCache[path] = new TextImage(\\\"Unable to load \\\" + path, 10, colorDb.get(\\\"red\\\"),\\n    \\\"normal\\\", \\\"Optimer\\\", \\\"\\\", \\\"\\\", false);\\n};\\n\\nFileImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {\\n  this.installHackToSupportAnimatedGifs();\\n  ctx.drawImage(this.animationHackImg, x, y);\\n};\\n\\n// The following is a hack that we use to allow animated gifs to show\\n// as animating on the canvas.\\nFileImage.prototype.installHackToSupportAnimatedGifs = /* @stopify flat */ function () {\\n  if (this.animationHackImg) { return; }\\n  this.animationHackImg = this.img.cloneNode(true);\\n  document.body.appendChild(this.animationHackImg);\\n  this.animationHackImg.style.position = 'absolute';\\n  this.animationHackImg.style.top = '-50000px';\\n};\\n\\nFileImage.prototype.getWidth = /* @stopify flat */ function () {\\n  return Math.round(this.img.width);\\n};\\n\\nFileImage.prototype.getHeight = /* @stopify flat */ function () {\\n  return Math.round(this.img.height);\\n};\\n\\nFileImage.prototype.equals = /* @stopify flat */ function (other) {\\n  return (other instanceof FileImage) && this.src === other.src\\n    || BaseImage.prototype.equals.call(this, other);\\n};\\n\\nvar DoesPauseAndKontinue = function (mode) {\\n  return RUNTIME.pauseStack(function (restarter) {\\n    if (mode === \\\"error\\\") {\\n      restarter.error(new Error(\\\"Error case of DoesPauseAndKontinue\\\"));\\n    } else {\\n      restarter.resume(new EllipseImage(30, 30, \\\"solid\\\", convertColor(\\\"green\\\")));\\n    }\\n  });\\n}\\n\\n\\nreturn module.exports = {\\n  triangle: /* @stopify flat */ function (size, style, color) {\\n    return new TriangleImage(size, 360 - 60, size, style, convertColor(color));\\n  },\\n  \\\"right-triangle\\\": /* @stopify flat */ function (side1, side2, style, color) {\\n    return new TriangleImage(side1, 360 - 90, side2, style, convertColor(color));\\n  },\\n  \\\"isosceles-triangle\\\": /* @stopify flat */ function (side, angle, style, color) {\\n    return new TriangleImage((2 * side * Math.sin((angle * Math.PI / 180) / 2)),\\n      360 - ((180 - angle) / 2), side, style, convertColor(color));\\n  },\\n  \\\"triangle-sss\\\": /* @stopify flat */ function (sideA, sideB, sideC, style, color) {\\n    return TriangleSSS(sideA, sideB, sideC, style, convertColor(color));\\n  },\\n  \\\"triangle-ass\\\": /* @stopify flat */ function (angleA, sideB, sideC, style, color) {\\n    return TriangleASS(angleA, sideB, sideC, style, convertColor(color));\\n  },\\n  \\\"triangle-sas\\\": /* @stopify flat */ function (sideA, angleB, sideC, style, color) {\\n    return TriangleSAS(sideA, angleB, sideC, style, convertColor(color));\\n  },\\n  \\\"triangle-ssa\\\": /* @stopify flat */ function (sideA, sideB, angleC, style, color) {\\n    return TriangleSSA(sideA, sideB, angleC, style, convertColor(color));\\n  },\\n  \\\"triangle-aas\\\": /* @stopify flat */ function (angleA, angleB, sideC, style, color) {\\n    return TriangleAAS(angleA, angleB, sideC, style, convertColor(color));\\n  },\\n  \\\"triangle-asa\\\": /* @stopify flat */ function (angleA, sideB, angleC, style, color) {\\n    return TriangleASA(angleA, sideB, angleC, style, convertColor(color));\\n  },\\n  \\\"triangle-saa\\\": /* @stopify flat */ function (sideA, angleB, angleC, style, color) {\\n    return TriangleSAA(sideA, angleB, angleC, style, convertColor(color));\\n  },\\n  ellipse: /* @stopify flat */ function (width, height, style, color) {\\n    return new EllipseImage(width, height, style, convertColor(color));\\n  },\\n  circle: /* @stopify flat */ function (radius, style, color) {\\n    return new EllipseImage(2 * radius, 2 * radius, style, convertColor(color));\\n  },\\n  rectangle: /* @stopify flat */ function (width, height, style, color) {\\n    return new RectangleImage(width, height, style, convertColor(color));\\n  },\\n  square: /* @stopify flat */ function (length, style, color) {\\n    return new RectangleImage(length, length, style, convertColor(color));\\n  },\\n  rhombus: /* @stopify flat */ function (side, angle, style, color) {\\n    return new RhombusImage(side, angle, style, convertColor(color));\\n  },\\n  line: /* @stopify flat */ function (x, y, color) {\\n    return new LineImage(x, y, convertColor(color));\\n  },\\n  \\\"add-line\\\": /* @stopify flat */ function (img, x1, y1, x2, y2, color) {\\n    return new OverlayImage(new LineImage((x2 - x1), (y2 - y1), convertColor(color)), img, Math.min(x1, x2), Math.min(y1, y2));\\n  },\\n  star: /* @stopify flat */ function (side, style, color) {\\n    return new PolygonImage(side, 5, 2, style, convertColor(color));\\n  },\\n  \\\"radial-star\\\": /* @stopify flat */ function (points, outer, inner, style, color) {\\n    return new StarImage(points, inner, outer, style, convertColor(color));\\n  },\\n  \\\"star-sized\\\": /* @stopify flat */ function (points, outer, inner, style, color) {\\n    return new StarImage(points, inner, outer, style, convertColor(color));\\n  },\\n  \\\"star-polygon\\\": /* @stopify flat */ function (length, count, step, style, color) {\\n    return new PolygonImage(length, count, step, style, convertColor(color));\\n  },\\n  \\\"regular-polygon\\\": /* @stopify flat */ function (length, count, style, color) {\\n    return new PolygonImage(length, count, 1, style, convertColor(color));\\n  },\\n  overlay: /* @stopify flat */ function (img1, img2) {\\n    return new OverlayImage(img1, img2, \\\"center\\\", \\\"center\\\");\\n  },\\n  \\\"overlay-align\\\": /* @stopify flat */ function (X, Y, img1, img2) {\\n    return new OverlayImage(img1, img2, X, Y);\\n  },\\n  \\\"overlay-xy\\\": /* @stopify flat */ function (img1, dx, dy, img2) {\\n    return new OverlayImage(img1, img2, dx, dy);\\n  },\\n  underlay: /* @stopify flat */ function (img1, img2) {\\n    return new OverlayImage(img2, img1, \\\"center\\\", \\\"center\\\");\\n  },\\n  \\\"underlay-align\\\": /* @stopify flat */ function (X, Y, img1, img2) {\\n    return new OverlayImage(img2, img1, X, Y);\\n  },\\n  \\\"underlay-xy\\\": /* @stopify flat */ function (img1, dx, dy, img2) {\\n    return new OverlayImage(img2, img1, -dx, -dy);\\n  },\\n  beside: /* @stopify flat */ function (img1, img2) {\\n    return new OverlayImage(img1, img2, \\\"beside\\\", \\\"middle\\\");\\n  },\\n  \\\"beside-align\\\": /* @stopify flat */ function (Y, img1, img2) {\\n    return new OverlayImage(img1, img2, \\\"beside\\\", Y);\\n  },\\n  above: /* @stopify flat */ function (img1, img2) {\\n    return new OverlayImage(img1, img2, \\\"middle\\\", \\\"above\\\");\\n  },\\n  \\\"above-align\\\": /* @stopify flat */ function (X, img1, img2) {\\n    return new OverlayImage(img1, img2, X, \\\"above\\\");\\n  },\\n  rotate: /* @stopify flat */ function (angle, img) {\\n    return new RotateImage(angle, img);\\n  },\\n  text: /* @stopify flat */ function (str, size, color) {\\n    return new TextImage(str, size, convertColor(color), \\\"normal\\\", \\\"Optimizer\\\", \\\"normal\\\", \\\"normal\\\", false);\\n  },\\n  \\\"text-font\\\": /* @stopify flat */ function (str, size, color, face, family, style, weight, underline) {\\n    return new TextImage(str, size, convertColor(color), face, family, style, weight, underline);\\n  },\\n  \\\"flip-horizontal\\\": /* @stopify flat */  function (img) {\\n    return new FlipImage(img, \\\"horizontal\\\");\\n  },\\n  \\\"flip-vertical\\\": /* @stopify flat */  function (img) {\\n    return new FlipImage(img, \\\"vertical\\\");\\n  },\\n  frame: /* @stopify flat */  function (img) {\\n    return new FrameImage(img);\\n  },\\n  crop: /* @stopify flat */ function (x, y, width, height, img) {\\n    return new CropImage(x, y, width, height, img);\\n  },\\n  scale: /* @stopify flat */ function (factor, img) {\\n    return new ScaleImage(factor, factor, img);\\n  },\\n  \\\"scale-xy\\\": /* @stopify flat */ function (xFactor, yFactor, img) {\\n    return new ScaleImage(xFactor, yFactor, img);\\n  },\\n  \\\"empty-scene\\\": /* @stopify flat */ function (width, height) {\\n    return new SceneImage(width, height, [], true);\\n  },\\n  \\\"empty-image\\\": new SceneImage(0, 0, [], true),\\n  \\\"is-image\\\": /* @stopify flat */ function (img) {\\n    return isImage(img);\\n  },\\n  \\\"image-width\\\": /* @stopify flat */ function (img) {\\n    return img.getWidth();\\n  },\\n  \\\"image-height\\\": /* @stopify flat */ function (img) {\\n    return img.getHeight();\\n  },\\n  \\\"image-baseline\\\": /* @stopify flat */ function (img) {\\n    return img.getBaseline();\\n  },\\n  \\\"name-to-color\\\": /* @stopify flat */ function (name) {\\n    return colorDb.get(String(name)) || false;\\n  },\\n  \\\"is-image-color\\\": /* @stopify flat */ function (c) {\\n    return isColorOrColorString(c);\\n  },\\n  \\\"images-equal\\\": /* @stopify flat */ function (img1, img2) {\\n    return imageEquals(img1, img2);\\n  },\\n  \\\"images-difference\\\": /* @stopify flat */ function (img1, img2) {\\n    return imageDifference(img1, img2);\\n  },\\n  \\\"is-side-count\\\": /* @stopify flat */ function (sth) {\\n    return isSideCount(sth);\\n  },\\n  \\\"is-step-count\\\": /* @stopify flat */ function (num) {\\n    return isStepCount(num);\\n  },\\n  \\\"is-mode\\\": /* @stopify flat */ function (x) {\\n    return isMode(x);\\n  },\\n  \\\"is-angle\\\": /* @stopify flat */ function (x) {\\n    return isAngle(x);\\n  },\\n  \\\"is-x-place\\\": /* @stopify flat */ function (x) {\\n    return isPlaceX(x);\\n  },\\n  \\\"is-y-place\\\": /* @stopify flat */ function (x) {\\n    return isPlaceY(x);\\n  },\\n  \\\"color-at-position\\\": /* @stopify flat */ function (img, x, y) {\\n    return colorAtPosition(img, x, y);\\n  },\\n  \\\"image-to-color-list\\\": /* @stopify flat */ function (img) {\\n    return imageToColorList(img);\\n  },\\n  \\\"color-list-to-image\\\": /* @stopify flat */ function (lOfC, width, height, pinX, pinY) {\\n    return colorListToImage(lOfC, width, height, pinX, pinY);\\n  },\\n  \\\"color-list-to-bitmap\\\": /* @stopify flat */ function (lOfC, width, height) {\\n    return colorListToImage(lOfC, width, height, 0, 0);\\n  },\\n  \\\"put-image\\\": /* @stopify flat */ function (image, x, y, background) {\\n    return PutImage(image, x, y, background);\\n  },\\n  \\\"place-image\\\": /* @stopify flat */ function (image, x, y, background) {\\n    return PlaceImage(image, x, y, background);\\n  },\\n  \\\"place-image-align\\\": /* @stopify flat */ function (image, x, y, placeX, placeY, background) {\\n    return PlaceImageAlign(image, x, y, placeX, placeY, background);\\n  },\\n  \\\"scene-line\\\": /* @stopify flat */ function (img, x1, y1, x2, y2, color) {\\n    return SceneLineImage(img, x1, y1, x2, y2, convertColor(color));\\n  },\\n  \\\"image-url\\\": /* @stopify flat */ function (url) {\\n    return ImageUrlImage(url);\\n  },\\n  \\\"bitmap-url\\\": /* @stopify flat */ function (url) {\\n    return ImageUrlImage(url);\\n  }\\n};\\n\",\"timestamp\":1568484216653},{\"key\":\"prewritten/image.arr.json\",\"content\":\"{\\n  \\\"requires\\\": [ ],\\n  \\\"provides\\\": {\\n    \\\"shorthands\\\": {\\n      \\\"Image\\\": [\\\"local\\\", \\\"Image\\\"],\\n      \\\"lOfC\\\": [\\\"tyapp\\\", { \\\"tag\\\": \\\"name\\\",\\n                \\\"origin\\\": { \\\"import-type\\\": \\\"uri\\\", \\\"uri\\\": \\\"builtin://list\\\" },\\n                \\\"name\\\": \\\"List\\\" }, [[\\\"local\\\", \\\"Color\\\"]]]\\n    },\\n    \\\"values\\\": {\\n      \\\"triangle\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"right-triangle\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"isosceles-triangle\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"triangle-sss\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"triangle-ass\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"triangle-sas\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"triangle-ssa\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"triangle-aas\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"triangle-asa\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"triangle-saa\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"ellipse\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"circle\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"rectangle\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"square\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"rhombus\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"line\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"add-line\\\": [\\\"arrow\\\", [\\\"Image\\\", \\\"Number\\\", \\\"Number\\\", \\\"Number\\\", \\\"Number\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"star\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"radial-star\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"star-sized\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"star-polygon\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"regular-polygon\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"overlay\\\": [\\\"arrow\\\", [\\\"Image\\\", \\\"Image\\\"], \\\"Image\\\"],\\n      \\\"overlay-align\\\": [\\\"arrow\\\", [\\\"String\\\", \\\"String\\\", \\\"Image\\\", \\\"Image\\\"], \\\"Image\\\"],\\n      \\\"overlay-xy\\\": [\\\"arrow\\\", [\\\"Image\\\", \\\"Number\\\", \\\"Number\\\", \\\"Image\\\"], \\\"Image\\\"],\\n      \\\"underlay\\\": [\\\"arrow\\\", [\\\"Image\\\", \\\"Image\\\"], \\\"Image\\\"],\\n      \\\"underlay-align\\\": [\\\"arrow\\\", [\\\"String\\\", \\\"String\\\", \\\"Image\\\", \\\"Image\\\"], \\\"Image\\\"],\\n      \\\"underlay-xy\\\": [\\\"arrow\\\", [\\\"Image\\\", \\\"Number\\\", \\\"Number\\\", \\\"Image\\\"], \\\"Image\\\"],\\n      \\\"beside\\\": [\\\"arrow\\\", [\\\"Image\\\", \\\"Image\\\"], \\\"Image\\\"],\\n      \\\"beside-align\\\": [\\\"arrow\\\", [\\\"String\\\", \\\"Image\\\", \\\"Image\\\"], \\\"Image\\\"],\\n      \\\"above\\\": [\\\"arrow\\\", [\\\"Image\\\", \\\"Image\\\"], \\\"Image\\\"],\\n      \\\"above-align\\\": [\\\"arrow\\\", [\\\"String\\\", \\\"Image\\\", \\\"Image\\\"], \\\"Image\\\"],\\n      \\\"rotate\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Image\\\"], \\\"Image\\\"],\\n      \\\"text\\\": [\\\"arrow\\\", [\\\"String\\\", \\\"Number\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"text-font\\\": [\\\"arrow\\\", [\\\"String\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\", \\\"String\\\", \\\"String\\\", \\n        \\\"String\\\", \\\"Boolean\\\"], \\\"Image\\\"],\\n      \\\"flip-horizontal\\\": [\\\"arrow\\\", [\\\"Image\\\"], \\\"Image\\\"],\\n      \\\"flip-vertical\\\": [\\\"arrow\\\", [\\\"Image\\\"], \\\"Image\\\"],\\n      \\\"frame\\\": [\\\"arrow\\\", [\\\"Image\\\"], \\\"Image\\\"],\\n      \\\"crop\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"Number\\\", \\\"Number\\\", \\\"Image\\\"], \\\"Image\\\"],\\n      \\\"scale\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Image\\\"], \\\"Image\\\"],\\n      \\\"scale-xy\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\", \\\"Image\\\"], \\\"Image\\\"],\\n      \\\"empty-scene\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\"], \\\"Image\\\"],\\n      \\\"empty-image\\\": \\\"Image\\\",\\n      \\\"is-image\\\": [\\\"arrow\\\", [\\\"Any\\\"], \\\"Boolean\\\"],\\n      \\\"image-width\\\": [\\\"arrow\\\", [\\\"Image\\\"], \\\"Number\\\"],\\n      \\\"image-height\\\": [\\\"arrow\\\", [\\\"Image\\\"], \\\"Number\\\"],\\n      \\\"image-baseline\\\": [\\\"arrow\\\", [\\\"Image\\\"], \\\"Number\\\"],\\n      \\\"name-to-color\\\": [\\\"arrow\\\", [\\\"String\\\"], [\\\"local\\\", \\\"Color\\\"]],\\n      \\\"is-image-color\\\": [\\\"arrow\\\", [\\\"Any\\\"], \\\"Boolean\\\"],\\n      \\\"images-equal\\\": [\\\"arrow\\\", [\\\"Image\\\", \\\"Image\\\"], \\\"Boolean\\\"],\\n      \\\"images-difference\\\": [\\\"arrow\\\", [\\\"Image\\\", \\\"Image\\\"], \\\"Boolean\\\"],\\n      \\\"is-side-count\\\": [\\\"arrow\\\", [\\\"Any\\\"], \\\"Boolean\\\"],\\n      \\\"is-step-count\\\": [\\\"arrow\\\", [\\\"Number\\\"], \\\"Boolean\\\"],\\n      \\\"is-mode\\\": [\\\"arrow\\\", [\\\"Any\\\"], \\\"Boolean\\\"],\\n      \\\"is-angle\\\": [\\\"arrow\\\", [\\\"Number\\\"], \\\"Boolean\\\"],\\n      \\\"is-x-place\\\": [\\\"arrow\\\", [\\\"String\\\"], \\\"Boolean\\\"],\\n      \\\"is-y-place\\\": [\\\"arrow\\\", [\\\"Any\\\"], \\\"Boolean\\\"],\\n      \\\"color-at-position\\\": [\\\"arrow\\\", [\\\"Image\\\", \\\"Number\\\", \\\"Number\\\"], [\\\"local\\\", \\\"Color\\\"]],\\n      \\\"image-to-color-list\\\": [\\\"arrow\\\", [\\\"Image\\\"], \\\"lOfC\\\"],\\n      \\\"color-list-to-image\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Number\\\", \\\"Number\\\", \\\"Number\\\", \\\"Number\\\"], \\\"Image\\\"],\\n      \\\"color-list-to-bitmap\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Number\\\", \\\"Number\\\"], \\\"Image\\\"],\\n      \\\"put-image\\\": [\\\"arrow\\\", [\\\"Image\\\", \\\"Number\\\", \\\"Number\\\", \\\"Image\\\"], \\\"Image\\\"],\\n      \\\"place-image\\\": [\\\"arrow\\\", [\\\"Image\\\", \\\"Number\\\", \\\"Number\\\", \\\"Image\\\"], \\\"Image\\\"],\\n      \\\"place-image-align\\\": [\\\"arrow\\\", [\\\"Image\\\", \\\"Number\\\", \\\"Number\\\", \\\"String\\\", \\\"String\\\", \\\"Image\\\"], \\\"Image\\\"],\\n      \\\"scene-line\\\": [\\\"arrow\\\", [\\\"Image\\\", \\\"Number\\\", \\\"Number\\\", \\\"Number\\\", \\\"Number\\\", \\\"String\\\"], \\\"Image\\\"],\\n      \\\"image-url\\\": [\\\"arrow\\\", [\\\"String\\\"], \\\"Image\\\"],\\n      \\\"bitmap-url\\\": [\\\"arrow\\\", [\\\"String\\\"], \\\"Image\\\"]\\n    },\\n    \\\"aliases\\\": {\\n      \\\"Image\\\": { \\\"tag\\\": \\\"name\\\", \\n                 \\\"origin\\\": { \\\"import-type\\\": \\\"uri\\\", \\\"uri\\\": \\\"builtin://image\\\" },\\n                 \\\"name\\\": \\\"Image\\\" }\\n    },\\n    \\\"datatypes\\\": {\\n      \\\"Image\\\": [\\\"data\\\", \\\"Image\\\", [], [], {}],\\n      \\\"Color\\\": [\\\"data\\\", \\\"Color\\\", [], [], {}]\\n    }\\n  }\\n}\\n\",\"timestamp\":1568484216704},{\"key\":\"prewritten/js-numbers.js\",\"content\":\"// Modifications to Danny Yoo's js-numbers library, whose LICENSE is:\\n\\n/*\\n\\nLicensing\\n---------\\n\\nThis software is covered under the following copyright:\\n\\n *\\n * Copyright (c) 2010  Danny Yoo\\n * All Rights Reserved.\\n *\\n * Permission is hereby granted, free of charge, to any person obtaining\\n * a copy of this software and associated documentation files (the\\n * \\\"Software\\\"), to deal in the Software without restriction, including\\n * without limitation the rights to use, copy, modify, merge, publish,\\n * distribute, sublicense, and/or sell copies of the Software, and to\\n * permit persons to whom the Software is furnished to do so, subject to\\n * the following conditions:\\n *\\n * The above copyright notice and this permission notice shall be\\n * included in all copies or substantial portions of the Software.\\n *\\n * THE SOFTWARE IS PROVIDED \\\"AS-IS\\\" AND WITHOUT WARRANTY OF ANY KIND,\\n * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY\\n * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.\\n *\\n * IN NO EVENT SHALL TOM WU BE LIABLE FOR ANY SPECIAL, INCIDENTAL,\\n * INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND, OR ANY DAMAGES WHATSOEVER\\n * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER OR NOT ADVISED OF\\n * THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF LIABILITY, ARISING OUT\\n * OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.\\n *\\n * In addition, the following condition applies:\\n *\\n * All redistributions must retain an intact copy of this copyright notice\\n * and disclaimer.\\n *\\n\\n======================================================================\\n\\njs-numbers uses code from the jsbn library.  The LICENSE to it is:\\n\\nLicensing\\n---------\\n\\nThis software is covered under the following copyright:\\n\\n *\\n * Copyright (c) 2003-2005  Tom Wu\\n * All Rights Reserved.\\n *\\n * Permission is hereby granted, free of charge, to any person obtaining\\n * a copy of this software and associated documentation files (the\\n * \\\"Software\\\"), to deal in the Software without restriction, including\\n * without limitation the rights to use, copy, modify, merge, publish,\\n * distribute, sublicense, and/or sell copies of the Software, and to\\n * permit persons to whom the Software is furnished to do so, subject to\\n * the following conditions:\\n *\\n * The above copyright notice and this permission notice shall be\\n * included in all copies or substantial portions of the Software.\\n *\\n * THE SOFTWARE IS PROVIDED \\\"AS-IS\\\" AND WITHOUT WARRANTY OF ANY KIND,\\n * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY\\n * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.\\n *\\n * IN NO EVENT SHALL TOM WU BE LIABLE FOR ANY SPECIAL, INCIDENTAL,\\n * INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND, OR ANY DAMAGES WHATSOEVER\\n * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER OR NOT ADVISED OF\\n * THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF LIABILITY, ARISING OUT\\n * OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.\\n *\\n * In addition, the following condition applies:\\n *\\n * All redistributions must retain an intact copy of this copyright notice\\n * and disclaimer.\\n *\\n\\nAddress all questions regarding this license to:\\n\\n  Tom Wu\\n  tjw@cs.Stanford.EDU\\n\\n*/\\n\\n/*\\n\\nModified to be Node-module compatible\\n\\nNo notion of levels (complex, exact, inexact, flonum).\\n\\nNo complex numbers.\\n\\nAdded roughnums.\\n\\npyretnum := fixnum | boxnum\\n\\nA fixnum is simply a JS double, and we prefer to use them\\nwhenever possible, viz., for integers that are small enough.\\n\\nboxnum := BigInteger | Rational | Roughnum.\\n\\nAn integer is either a fixnum or a BigInteger.\\n\\n*/\\n\\n// TODO(alex): Attempting to use the Stopified version of this code throw an exception:\\n// 'Type error: constr is undefined'\\n\\n\\n/* @stopify flat */\\n// Abbreviation\\nvar Numbers = {};\\n\\n// makeNumericBinop: (fixnum fixnum -> any) (pyretnum pyretnum -> any) -> (pyretnum pyretnum) X\\n// Creates a binary function that works either on fixnums or boxnums.\\n// Applies the appropriate binary function, ensuring that both pyretnums are\\n// coerced to be the same kind.\\n/* @stopify flat */ \\nfunction makeNumericBinop(onFixnums, onBoxednums, options) {\\n  options = options || {};\\n  return /* @stopify flat */ function(x, y, errbacks) {\\n\\n    if (options.isXSpecialCase && options.isXSpecialCase(x, errbacks))\\n      return options.onXSpecialCase(x, y, errbacks);\\n    if (options.isYSpecialCase && options.isYSpecialCase(y, errbacks))\\n      return options.onYSpecialCase(x, y, errbacks);\\n\\n    if (typeof(x) === 'number' &&\\n        typeof(y) === 'number') {\\n      return onFixnums(x, y, errbacks);\\n    }\\n\\n    if (typeof(x) === 'number') {\\n      x = liftFixnumInteger(x, y);\\n    }\\n    if (typeof(y) === 'number') {\\n      y = liftFixnumInteger(y, x);\\n    }\\n\\n    if (x instanceof Roughnum) {\\n      // y is rough, rat or bigint\\n      if (!(y instanceof Roughnum)) {\\n        // y is rat or bigint\\n        y = y.toRoughnum(errbacks);\\n      }\\n    } else if (y instanceof Roughnum) {\\n      // x is rat or bigint\\n      x = x.toRoughnum(errbacks);\\n    } else if (x instanceof Rational) {\\n      // y is rat or bigint\\n      if (!(y instanceof Rational)) {\\n        // y is bigint\\n        y = new Rational(y, 1);\\n      }\\n    } else if (y instanceof Rational) {\\n      // x is bigint\\n      x = new Rational(x, 1);\\n    }\\n\\n    return onBoxednums(x, y, errbacks);\\n  };\\n};\\n\\n// fromFixnum: fixnum -> pyretnum\\n/* @stopify flat */\\nfunction fromFixnum(x, errbacks) {\\n  if (!isFinite(x)) {\\n    return Roughnum.makeInstance(x, errbacks);\\n  }\\n  var nf = Math.floor(x);\\n  if (nf === x) {\\n    if (isOverflow(nf)) {\\n      return makeBignum(expandExponent(x+''));\\n    } else {\\n      return nf;\\n    }\\n  } else {\\n    //  used to return float, now rational\\n    var stringRep = x.toString();\\n    var match = stringRep.match(/^(.*)\\\\.(.*)$/);\\n    if (match) {\\n      var afterDecimal = parseInt(match[2]);\\n      var factorToInt = Math.pow(10, match[2].length);\\n      var extraFactor = _integerGcd(factorToInt, afterDecimal);\\n      var multFactor = factorToInt / extraFactor;\\n      return Rational.makeInstance(Math.round(x*multFactor), Math.round(factorToInt/extraFactor), errbacks);\\n    } else {\\n      return Rational.makeInstance(x, 1, errbacks);\\n    }\\n\\n  }\\n};\\n\\n/* @stopify flat */ \\nfunction expandExponent(s) {\\n  var match = s.match(scientificPattern), mantissaChunks, exponent;\\n  if (match) {\\n    mantissaChunks = match[1].match(/^([^.]*)(.*)$/);\\n    exponent = Number(match[2]);\\n\\n    if (mantissaChunks[2].length === 0) {\\n      return mantissaChunks[1] + zfill(exponent);\\n    }\\n\\n    if (exponent >= mantissaChunks[2].length - 1) {\\n      return (mantissaChunks[1] +\\n              mantissaChunks[2].substring(1) +\\n              zfill(exponent - (mantissaChunks[2].length - 1)));\\n    } else {\\n      return (mantissaChunks[1] +\\n              mantissaChunks[2].substring(1, 1+exponent));\\n    }\\n  } else {\\n    return s;\\n  }\\n};\\n\\n// zfill: integer -> string\\n// builds a string of \\\"0\\\"'s of length n.\\n/* @stopify flat */ \\nfunction zfill(n) {\\n  var buffer = [];\\n  buffer.length = n;\\n  for (var i = 0; i < n; i++) {\\n    buffer[i] = '0';\\n  }\\n  return buffer.join('');\\n};\\n\\n// liftFixnumInteger: fixnum-integer boxed-pyretnum -> boxed-pyretnum\\n// Lifts up fixnum integers to a boxed type.\\n\\n/* @stopify flat */\\nfunction liftFixnumInteger(x, other, errbacks) {\\n  if (other instanceof Roughnum)\\n    return new Roughnum(x, errbacks);\\n  else if (other instanceof BigInteger)\\n    return makeBignum(x);\\n  else\\n    return new Rational(x, 1, errbacks);\\n};\\n\\n\\n// isPyretNumber: any -> boolean\\n// Returns true if the thing is a pyretnum\\n/* @stopify flat */ \\nfunction isPyretNumber(thing) {\\n  return (typeof(thing) === 'number'\\n          || (thing instanceof Rational ||\\n              thing instanceof Roughnum ||\\n              thing instanceof BigInteger));\\n};\\n\\n// isRational: pyretnum -> boolean\\n/* @stopify flat */ \\nfunction isRational(n) {\\n  return (typeof(n) === 'number' ||\\n          (isPyretNumber(n) && n.isRational()));\\n};\\n\\nvar isExact = isRational;\\n\\n// isReal: pyretnum -> boolean\\n/* @stopify flat */ \\nfunction isReal(n) {\\n  return (typeof(n) === 'number' ||\\n          (isPyretNumber(n) && n.isReal()));\\n};\\n\\n// isInteger: pyretnum -> boolean\\n/* @stopify flat */ \\nfunction isInteger(n) {\\n  return (typeof(n) === 'number' ||\\n          (isPyretNumber(n) && n.isInteger()));\\n};\\n\\n/* @stopify flat */ \\nfunction isRoughnum(n) {\\n  if (typeof(n) === 'number') {\\n    return false;\\n  } else {\\n    return (isPyretNumber(n) && n.isRoughnum());\\n  }\\n};\\n\\n/* @stopify flat */ \\nfunction isPositive(n) {\\n  if (typeof(n) === 'number') {\\n    return n > 0;\\n  } else {\\n    return (isPyretNumber(n) && n.isPositive());\\n  }\\n};\\n\\n/* @stopify flat */ \\nfunction isNonPositive(n) {\\n  if (typeof(n) === 'number') {\\n    return n <= 0;\\n  } else {\\n    return (isPyretNumber(n) && n.isNonPositive());\\n  }\\n};\\n\\n/* @stopify flat */ \\nfunction isNegative(n) {\\n  if (typeof(n) === 'number') {\\n    return n < 0;\\n  } else {\\n    return (isPyretNumber(n) && n.isNegative());\\n  }\\n};\\n\\n/* @stopify flat */ \\nfunction isNonNegative(n) {\\n  if (typeof(n) === 'number') {\\n    return n >= 0;\\n  } else {\\n    return (isPyretNumber(n) && n.isNonNegative());\\n  }\\n};\\n\\n// toFixnum: pyretnum -> javascript-number\\n/* @stopify flat */ \\nfunction toFixnum(n) {\\n  if (typeof(n) === 'number')\\n    return n;\\n  return n.toFixnum();\\n};\\n\\n// toRational: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction toRational(n, errbacks) {\\n  if (typeof(n) === 'number')\\n    return n;\\n  return n.toRational(errbacks);\\n};\\n\\nvar toExact = toRational;\\n\\n// toRoughnum: pyretnum -> pyretnum\\n\\n/* @stopify flat */ \\nfunction toRoughnum(n, errbacks) {\\n  if (typeof(n) === 'number') {\\n    return Roughnum.makeInstance(n, errbacks);\\n  } else {\\n    return n.toRoughnum(errbacks);\\n  }\\n};\\n\\n//////////////////////////////////////////////////////////////////////\\n\\n// add: pyretnum pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction add(x, y, errbacks) {\\n  var sum;\\n  if (typeof(x) === 'number' && typeof(y) === 'number') {\\n    sum = x + y;\\n    if (isOverflow(sum)) {\\n      return (makeBignum(x)).add(makeBignum(y));\\n    }\\n    else {\\n      return sum;\\n    }\\n  }\\n  return addSlow(x, y, errbacks);\\n};\\n\\nvar addSlow = makeNumericBinop(\\n  /* @stopify flat */ function(x, y, errbacks) {\\n    var sum = x + y;\\n    if (isOverflow(sum)) {\\n      return (makeBignum(x)).add(makeBignum(y));\\n    } else {\\n      return sum;\\n    }\\n  },\\n  /* @stopify flat */ function(x, y, errbacks) {\\n    return x.add(y);\\n  },\\n  {isXSpecialCase: /* @stopify flat */ function(x, errbacks) {\\n    return isInteger(x) && _integerIsZero(x) },\\n   onXSpecialCase: /* @stopify flat */ function(x, y, errbacks) { return y; },\\n   isYSpecialCase: /* @stopify flat */ function(y, errbacks) {\\n     return isInteger(y) && _integerIsZero(y) },\\n   onYSpecialCase: /* @stopify flat */ function(x, y, errbacks) { return x; }\\n  });\\n\\nvar subtract = /* @stopify flat */ function(x, y, errbacks) {\\n  if (typeof(x) === 'number' && typeof(y) === 'number') {\\n    var diff = x - y;\\n    if (isOverflow(diff)) {\\n      return (makeBignum(x)).subtract(makeBignum(y));\\n    } else {\\n      return diff;\\n    }\\n  }\\n  return subtractSlow(x, y, errbacks);\\n};\\n\\n// subtract: pyretnum pyretnum -> pyretnum\\nvar subtractSlow = makeNumericBinop(\\n  /* @stopify flat */ function(x, y, errbacks) {\\n    var diff = x - y;\\n    if (isOverflow(diff)) {\\n      return (makeBignum(x)).subtract(makeBignum(y));\\n    } else {\\n      return diff;\\n    }\\n  },\\n  /* @stopify flat */ function(x, y, errbacks) {\\n    return x.subtract(y);\\n  },\\n  {isXSpecialCase: /* @stopify flat */ function(x, errbacks) {\\n    return isInteger(x) && _integerIsZero(x) },\\n   onXSpecialCase: /* @stopify flat */ function(x, y, errbacks) { return negate(y, errbacks); },\\n   isYSpecialCase: /* @stopify flat */ function(y, errbacks) {\\n     return isInteger(y) && _integerIsZero(y) },\\n   onYSpecialCase: /* @stopify flat */ function(x, y, errbacks) { return x; }\\n  });\\n\\n// mulitply: pyretnum pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction multiply(x, y, errbacks) {\\n  var prod;\\n  if (typeof(x) === 'number' && typeof(y) === 'number') {\\n    prod = x * y;\\n    if (isOverflow(prod)) {\\n      return (makeBignum(x)).multiply(makeBignum(y));\\n    } else {\\n      return prod;\\n    }\\n  }\\n  return multiplySlow(x, y, errbacks);\\n};\\nvar multiplySlow = makeNumericBinop(\\n  /* @stopify flat */ function(x, y, errbacks) {\\n    var prod = x * y;\\n    if (isOverflow(prod)) {\\n      return (makeBignum(x)).multiply(makeBignum(y), errbacks);\\n    } else {\\n      return prod;\\n    }\\n  },\\n  /* @stopify flat */ function(x, y, errbacks) {\\n    return x.multiply(y, errbacks);\\n  },\\n  {isXSpecialCase: /* @stopify flat */ function(x, errbacks) {\\n    return (isInteger(x) &&\\n            (_integerIsZero(x) || _integerIsOne(x) || _integerIsNegativeOne(x))) },\\n   onXSpecialCase: /* @stopify flat */ function(x, y, errbacks) {\\n     if (_integerIsZero(x))\\n       return 0;\\n     if (_integerIsOne(x))\\n       return y;\\n     if (_integerIsNegativeOne(x))\\n       return negate(y, errbacks);\\n   },\\n   isYSpecialCase: /* @stopify flat */ function(y, errbacks) {\\n     return (isInteger(y) &&\\n             (_integerIsZero(y) || _integerIsOne(y) || _integerIsNegativeOne(y)))},\\n   onYSpecialCase: /* @stopify flat */ function(x, y, errbacks) {\\n     if (_integerIsZero(y))\\n       return 0;\\n     if (_integerIsOne(y))\\n       return x;\\n     if (_integerIsNegativeOne(y))\\n       return negate(x, errbacks);\\n   }\\n  });\\n\\n// divide: pyretnum pyretnum -> pyretnum\\nvar divide = makeNumericBinop(\\n  /* @stopify flat */ function(x, y, errbacks) {\\n    if (_integerIsZero(y))\\n      errbacks.throwDivByZero(\\\"/: division by zero, \\\" + x + ' ' + y);\\n    var div = x / y;\\n    if (isOverflow(div)) {\\n      return (makeBignum(x)).divide(makeBignum(y), errbacks);\\n    } else if (Math.floor(div) !== div) {\\n      return Rational.makeInstance(x, y, errbacks);\\n    } else {\\n      return div;\\n    }\\n  },\\n  /* @stopify flat */ function(x, y, errbacks) {\\n    if (equalsAnyZero(y, errbacks)) {\\n      errbacks.throwDivByZero('/: division by zero, ' + x + ' ' + y);\\n    }\\n    return x.divide(y, errbacks);\\n  },\\n  {\\n    isXSpecialCase: /* @stopify flat */ function(x, errbacks) {\\n      return equalsAnyZero(x, errbacks);\\n    },\\n    onXSpecialCase: /* @stopify flat */ function(x, y, errbacks) {\\n      if (equalsAnyZero(y, errbacks)) {\\n        errbacks.throwDivByZero(\\\"/: division by zero, \\\" + x + ' ' + y);\\n      }\\n      return 0;\\n    },\\n    isYSpecialCase: /* @stopify flat */ function(y, errbacks) {\\n      return equalsAnyZero(y, errbacks);\\n    },\\n    onYSpecialCase: /* @stopify flat */ function(x, y, errbacks) {\\n      errbacks.throwDivByZero(\\\"/: division by zero, \\\" + x + ' ' + y);\\n    }\\n  });\\n\\n/* @stopify flat */ \\nfunction equals(x, y, errbacks) {\\n  if (x === y) { return true; }\\n  else {\\n    if (typeof x === \\\"number\\\" && typeof y === \\\"number\\\") { return false; }\\n    else {\\n      return equalsSlow(x, y, errbacks);\\n    }\\n  }\\n};\\n// equals: pyretnum pyretnum -> boolean\\nvar equalsSlow = makeNumericBinop(\\n  /* @stopify flat */ function(x, y, errbacks) {\\n    return x === y;\\n  },\\n  /* @stopify flat */ function(x, y, errbacks) {\\n    return x.equals(y, errbacks);\\n  });\\n\\n/* @stopify flat */ \\nfunction equalsAnyZero(x, errbacks) {\\n  if (typeof(x) === 'number') return x === 0;\\n  if (isRoughnum(x)) return x.n === 0;\\n  return x.equals(0, errbacks);\\n};\\n\\n// eqv: pyretnum pyretnum -> boolean\\n/* @stopify flat */ \\nfunction eqv(x, y, errbacks) {\\n  if (x === y)\\n    return true;\\n  if (typeof(x) === 'number' && typeof(y) === 'number')\\n    return x === y;\\n  var ex = isRational(x), ey = isRational(y);\\n  return (((ex && ey) || (!ex && !ey)) && equals(x, y, errbacks));\\n};\\n\\n// approxEqual: pyretnum pyretnum pyretnum -> boolean\\n/* @stopify flat */ \\nfunction approxEquals(x, y, delta, errbacks) {\\n  return lessThanOrEqual(abs(subtract(x, y, errbacks), errbacks),\\n                         delta, errbacks);\\n};\\n\\n// used for within\\n/* @stopify flat */ \\nfunction roughlyEquals(x, y, delta, errbacks) {\\n  if (isNegative(delta)) {\\n    errbacks.throwToleranceError(\\\"negative tolerance \\\" + delta);\\n  }\\n\\n  if (x === y) return true;\\n\\n  if (isRoughnum(delta) && delta.n === Number.MIN_VALUE) {\\n    if ((isRoughnum(x) || isRoughnum(y)) &&\\n          (Math.abs(subtract(x,y).n) === Number.MIN_VALUE)) {\\n      errbacks.throwToleranceError(\\\"roughnum tolerance too small for meaningful comparison, \\\" + x + ' ' + y + ' ' + delta);\\n    }\\n  }\\n\\n  var ratx = isRoughnum(x) ? x.toRational(errbacks) : x;\\n  var raty = isRoughnum(y) ? y.toRational(errbacks) : y;\\n\\n  var ratdelta = isRoughnum(delta) ? delta.toRational(errbacks) : delta;\\n  return approxEquals(ratx, raty, ratdelta, errbacks);\\n};\\n\\n/* @stopify flat */ function roughlyEqualsRel(computedValue, trueValue, delta, errbacks) {\\n  if (isNegative(delta)) {\\n    errbacks.throwRelToleranceError('negative relative tolerance ' + delta)\\n  }\\n\\n  if (computedValue === trueValue) {\\n    return true\\n  }\\n\\n  var deltaIsRough = isRoughnum(delta)\\n  var argNumsAreRough = isRoughnum(computedValue) || isRoughnum(trueValue)\\n\\n  var ratCv = isRoughnum(computedValue) ? computedValue.toRational(errbacks) : computedValue\\n  var ratTv = isRoughnum(trueValue) ? trueValue.toRational(errbacks) : trueValue\\n\\n  var ratDelta = isRoughnum(delta) ? delta.toRational(errbacks): delta\\n\\n  var err = abs(subtract(ratCv, ratTv, errbacks), errbacks)\\n\\n  if (lessThanOrEqual(ratDelta, 1, errbacks)) {\\n    var absDelta = multiply(ratDelta, abs(ratTv, errbacks), errbacks)\\n    if (deltaIsRough && toRoughnum(absDelta, errbacks).n === Number.MIN_VALUE) {\\n      if (argNumsAreRough && Math.abs(toRoughnum(err, errbacks).n) === Number.MIN_VALUE) {\\n        errbacks.throwRelToleranceError('roughnum tolerance too small for meaningful comparison, ' +\\n                          computedValue + ' ' + trueValue + ' ' + delta)\\n      }\\n    }\\n\\n    return lessThanOrEqual(err, absDelta, errbacks)\\n  } else {\\n    var errRatio = divide(err, abs(ratTv, errbacks), errbacks)\\n\\n    if (deltaIsRough && delta.n === Number.MIN_VALUE) {\\n      if (argNumsAreRough && Math.abs(toRoughnum(errRatio, errbacks).n) === Number.MIN_VALUE) {\\n        errbacks.throwRelToleranceError('roughnum tolerance too small for meaningful comparison, ' +\\n                          computedValue + ' ' + trueValue + ' ' + delta)\\n      }\\n    }\\n\\n    return lessThanOrEqual(errRatio, ratDelta, errbacks)\\n  }\\n}\\n\\n// greaterThanOrEqual: pyretnum pyretnum -> boolean\\n/* @stopify flat */ \\nfunction greaterThanOrEqual(x, y, errbacks) {\\n  if(typeof x === \\\"number\\\" && typeof y === \\\"number\\\") {\\n    return x >= y;\\n  }\\n  return makeNumericBinop(undefined, /* @stopify flat */ function(x, y, errbacks) {\\n    return x.greaterThanOrEqual(y);\\n  })(x, y, errbacks);\\n}\\n\\n// lessThanOrEqual: pyretnum pyretnum -> boolean\\n/* @stopify flat */ \\nfunction lessThanOrEqual(x, y, errbacks) {\\n  if(typeof x === \\\"number\\\" && typeof y === \\\"number\\\") {\\n    return x <= y;\\n  }\\n  return makeNumericBinop(undefined, /* @stopify flat */ function(x, y, errbacks) {\\n    return x.lessThanOrEqual(y);\\n  })(x, y, errbacks);\\n};\\n\\n// greaterThan: pyretnum pyretnum -> boolean\\n/* @stopify flat */ \\nfunction greaterThan(x, y, errbacks) {\\n  if(typeof x === \\\"number\\\" && typeof y === \\\"number\\\") {\\n    return x > y;\\n  }\\n  return makeNumericBinop(undefined, /* @stopify flat */ function(x, y, errbacks) {\\n    return x.greaterThan(y);\\n  })(x, y, errbacks);\\n};\\n\\n// lessThan: pyretnum pyretnum -> boolean\\n/* @stopify flat */ \\nfunction lessThan(x, y, errbacks) {\\n  if(typeof x === \\\"number\\\" && typeof y === \\\"number\\\") {\\n    return x < y;\\n  }\\n  return makeNumericBinop(undefined, /* @stopify flat */ function(x, y, errbacks) {\\n    return x.lessThan(y);\\n  })(x, y, errbacks);\\n};\\n\\n// expt: pyretnum pyretnum -> pyretnum\\nvar expt = makeNumericBinop(\\n  /* @stopify flat */ function(x, y, errbacks) {\\n    var pow = Math.pow(x, y);\\n    if (isOverflow(pow)) {\\n      return (makeBignum(x)).expt(makeBignum(y));\\n    } else {\\n      return pow;\\n    }\\n  },\\n  /* @stopify flat */ function(x, y, errbacks) {\\n    return x.expt(y, errbacks);\\n  },\\n  {\\n    isXSpecialCase: /* @stopify flat */ function(x, errbacks) {\\n      return eqv(x, 0, errbacks) || eqv(x, 1, errbacks);\\n    },\\n    onXSpecialCase: /* @stopify flat */ function(x, y, errbacks) {\\n      if (eqv(x, 0, errbacks)) {\\n        if (eqv(y, 0, errbacks)) {\\n          return 1;\\n        } else if (lessThan(y, 0, errbacks)) {\\n          errbacks.throwDivByZero(\\\"expt: division by zero\\\");\\n        } else {\\n          return 0;\\n        }\\n      } else { // i.e., x is 1\\n        return 1;\\n      }\\n    },\\n\\n    isYSpecialCase: /* @stopify flat */ function(y, errbacks) {\\n      return eqv(y, 0, errbacks) || lessThan(y, 0, errbacks);\\n    },\\n    onYSpecialCase: /* @stopify flat */ function(x, y, errbacks) {\\n      if (eqv(y, 0, errbacks)) {\\n        return 1;\\n      } else { // i.e., y is negative\\n        return expt(divide(1, x, errbacks), negate(y, errbacks), errbacks);\\n      }\\n    }\\n  });\\n\\n// exp: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction exp(n, errbacks) {\\n  if ( eqv(n, 0, errbacks) ) {\\n    return 1;\\n  }\\n  if (typeof(n) === 'number') {\\n    var res = Math.exp(n);\\n    if (!isFinite(res))\\n      errbacks.throwGeneralError('exp: argument too large: ' + n);\\n    return Roughnum.makeInstance(res, errbacks);\\n  }\\n  return n.exp(errbacks);\\n};\\n\\n// modulo: pyretnum pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction modulo(m, n, errbacks) {\\n  if (! isInteger(m)) {\\n    errbacks.throwDomainError('modulo: the first argument '\\n                              + m + \\\" is not an integer.\\\", m, n);\\n  }\\n  if (! isInteger(n)) {\\n    errbacks.throwDomainError('modulo: the second argument '\\n                              + n + \\\" is not an integer.\\\", m, n);\\n  }\\n  if (_integerIsZero(n)) {\\n    errbacks.throwDomainError('modulo: the second argument is zero');\\n  }\\n  var result;\\n  if (typeof(m) === 'number') {\\n    result = m % n;\\n    if (n < 0) {\\n      if (result <= 0)\\n        return result;\\n      else\\n        return result + n;\\n    } else {\\n      if (result < 0)\\n        return result + n;\\n      else\\n        return result;\\n    }\\n  }\\n  result = _integerModulo(floor(m), floor(n));\\n  // The sign of the result should match the sign of n.\\n  if (lessThan(n, 0, errbacks)) {\\n    if (lessThanOrEqual(result, 0, errbacks)) {\\n      return result;\\n    }\\n    return add(result, n, errbacks);\\n\\n  } else {\\n    if (lessThan(result, 0, errbacks)) {\\n      return add(result, n, errbacks);\\n    }\\n    return result;\\n  }\\n};\\n\\n// numerator: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction numerator(n, errbacks) {\\n  if (typeof(n) === 'number')\\n    return n;\\n  return n.numerator();\\n};\\n\\n// denominator: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction denominator(n, errbacks) {\\n  if (typeof(n) === 'number')\\n    return 1;\\n  return n.denominator();\\n};\\n\\n// sqrt: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction sqrt(n, errbacks) {\\n  if (lessThan(n, 0, errbacks)) {\\n    errbacks.throwSqrtNegative('sqrt: negative argument ' + n);\\n  }\\n  if (typeof(n) === 'number') {\\n    var result = Math.sqrt(n);\\n    if (Math.floor(result) === result) {\\n      return result;\\n    } else {\\n      return Roughnum.makeInstance(result, errbacks);\\n    }\\n  }\\n  return n.sqrt(errbacks);\\n};\\n\\n// abs: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction abs(n, errbacks) {\\n  if (typeof(n) === 'number') {\\n    return Math.abs(n);\\n  }\\n  return n.abs(errbacks);\\n};\\n\\n// floor: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction floor(n, errbacks) {\\n  if (typeof(n) === 'number')\\n    return Math.floor(n);\\n  return n.floor(errbacks);\\n};\\n\\n// ceiling: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction ceiling(n, errbacks) {\\n  if (typeof(n) === 'number')\\n    return Math.ceil(n);\\n  return n.ceiling(errbacks);\\n};\\n\\n// round: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction round(n, errbacks) {\\n  if (typeof(n) === 'number') {\\n    return n;\\n  }\\n  return n.round(errbacks);\\n};\\n\\n/* @stopify flat */ \\nfunction roundEven(n, errbacks) {\\n  if (typeof(n) === 'number') return n;\\n  return n.roundEven(errbacks);\\n};\\n\\n// NB: all of these trig-gy generic /* @stopify flat */ functions should now return roughnum rather than float\\n// (except for an arg of 0, etc)\\n\\n// log: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction log(n, errbacks) {\\n  if ( eqv(n, 1, errbacks) ) {\\n    return 0;\\n  }\\n  if (lessThanOrEqual(n, 0, errbacks)) {\\n    errbacks.throwLogNonPositive('log: non-positive argument ' + n);\\n  }\\n  if (typeof(n) === 'number') {\\n    return Roughnum.makeInstance(Math.log(n), errbacks);\\n  }\\n  return n.log(errbacks);\\n};\\n\\n// tan: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction tan(n, errbacks) {\\n  if (eqv(n, 0, errbacks)) { return 0; }\\n  if (typeof(n) === 'number') {\\n    return Roughnum.makeInstance(Math.tan(n), errbacks);\\n  }\\n  return n.tan(errbacks);\\n};\\n\\n// atan: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction atan(n, errbacks) {\\n  if (eqv(n, 0, errbacks)) { return 0; }\\n  if (typeof(n) === 'number') {\\n    return Roughnum.makeInstance(Math.atan(n), errbacks);\\n  }\\n  return n.atan(errbacks);\\n};\\n\\n/* @stopify flat */ \\nfunction atan2(y, x, errbacks) {\\n  if (eqv(x, 0, errbacks)) { // x = 0\\n    if (eqv(y, 0, errbacks)) { // x = 0, y = 0\\n      //return Roughnum.makeInstance(Infinity, errbacks);\\n      errbacks.throwDomainError('atan2: out of domain argument (0, 0)');\\n    } else if (greaterThan(y, 0, errbacks)) { // x = 0, y > 0\\n      return Roughnum.makeInstance(Math.PI/2, errbacks);\\n    } else { // x = 0, y < 0\\n      return Roughnum.makeInstance(3*Math.PI/2, errbacks);\\n    }\\n  } else if (greaterThan(x, 0, errbacks)) { // x > 0\\n    if (greaterThanOrEqual(y, 0, errbacks)) { // x > 0, y >= 0, 1st qdt\\n      // atan(y/x) is already in the right qdt\\n      return atan(divide(y, x, errbacks), errbacks);\\n    } else { // x > 0, y < 0, 4th qdt\\n      // atan(y/x) is the 4th qdt and negative, so make it positive by adding 2pi\\n      return add(atan(divide(y, x, errbacks), errbacks), 2*Math.PI, errbacks);\\n    }\\n  } else { // x < 0\\n    // either x < 0, y >= 0 (2nd qdt), in which case\\n    //        atan(y/x) must be reflected from 4th to 2nd qdt, by adding pi\\n    //     or x < 0, y < 0  (3rd qdt), in which case\\n    //        atan(y/x) must be reflected from 1st to 3rd qdt, again by adding pi\\n    return add(atan(divide(y, x, errbacks), errbacks), Math.PI, errbacks);\\n  }\\n};\\n\\n// cos: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction cos(n, errbacks) {\\n  if (eqv(n, 0, errbacks)) { return 1; }\\n  if (typeof(n) === 'number') {\\n    return Roughnum.makeInstance(Math.cos(n), errbacks);\\n  }\\n  return n.cos(errbacks);\\n};\\n\\n// sin: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction sin(n, errbacks) {\\n  if (eqv(n, 0, errbacks)) { return 0; }\\n  if (typeof(n) === 'number') {\\n    return Roughnum.makeInstance(Math.sin(n), errbacks);\\n  }\\n  return n.sin(errbacks);\\n};\\n\\n// acos: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction acos(n, errbacks) {\\n  if (eqv(n, 1, errbacks)) { return 0; }\\n  if (lessThan(n, -1, errbacks) || greaterThan(n, 1, errbacks)) {\\n    errbacks.throwDomainError('acos: out of domain argument ' + n);\\n  }\\n  if (typeof(n) === 'number') {\\n    return Roughnum.makeInstance(Math.acos(n), errbacks);\\n  }\\n  return n.acos(errbacks);\\n};\\n\\n// asin: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction asin(n, errbacks) {\\n  if (eqv(n, 0, errbacks)) { return 0; }\\n  if (lessThan(n, -1, errbacks) || greaterThan(n, 1, errbacks)) {\\n    errbacks.throwDomainError('asin: out of domain argument ' + n);\\n  }\\n  if (typeof(n) === 'number') {\\n    return Roughnum.makeInstance(Math.asin(n), errbacks);\\n  }\\n  return n.asin(errbacks);\\n};\\n\\n// sqr: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction sqr(x, errbacks) {\\n  return multiply(x, x, errbacks);\\n};\\n\\n// integerSqrt: pyretnum -> pyretnum\\n/* @stopify flat */ \\nfunction integerSqrt(x, errbacks) {\\n  if (! isInteger(x)) {\\n    errbacks.throwDomainError('integer-sqrt: the argument ' + x.toString() +\\n                      \\\" is not an integer.\\\", x);\\n  }\\n  if (typeof (x) === 'number') {\\n    if(x < 0) {\\n      errbacks.throwSqrtNegative('integerSqrt of negative number', x);\\n    } else {\\n      return Math.floor(Math.sqrt(x));\\n    }\\n  }\\n  return x.integerSqrt(errbacks);\\n};\\n\\n// gcd: pyretnum [pyretnum ...] -> pyretnum\\n/* @stopify flat */ \\nfunction gcd(first, rest, errbacks) {\\n  if (! isInteger(first)) {\\n    errbacks.throwDomainError('gcd: the argument ' + first.toString() +\\n                              \\\" is not an integer.\\\", first);\\n  }\\n  var a = abs(first, errbacks), t, b;\\n  for(var i = 0; i < rest.length; i++) {\\n    b = abs(rest[i], errbacks);\\n    if (! isInteger(b)) {\\n      errbacks.throwDomainError('gcd: the argument ' + b.toString() +\\n                                \\\" is not an integer.\\\", b);\\n    }\\n    while (! _integerIsZero(b)) {\\n      t = a;\\n      a = b;\\n      b = _integerModulo(t, b);\\n    }\\n  }\\n  return a;\\n};\\n\\n// lcm: pyretnum [pyretnum ...] -> pyretnum\\n/* @stopify flat */ \\nfunction lcm(first, rest, errbacks) {\\n  if (! isInteger(first)) {\\n    errbacks.throwDomainError('lcm: the argument ' + first.toString() +\\n                              \\\" is not an integer.\\\", first);\\n  }\\n  var result = abs(first, errbacks);\\n  if (_integerIsZero(result)) { return 0; }\\n  for (var i = 0; i < rest.length; i++) {\\n    if (! isInteger(rest[i])) {\\n      errbacks.throwDomainError('lcm: the argument ' + rest[i].toString() +\\n                                \\\" is not an integer.\\\", rest[i]);\\n    }\\n    var divisor = _integerGcd(result, rest[i]);\\n    if (_integerIsZero(divisor)) {\\n      return 0;\\n    }\\n    result = divide(multiply(result, rest[i], errbacks), divisor, errbacks);\\n  }\\n  return result;\\n};\\n\\n/* @stopify flat */ \\nfunction quotient(x, y, errbacks) {\\n  if (! isInteger(x)) {\\n    errbacks.throwDomainError('quotient: the first argument ' + x.toString() +\\n                              \\\" is not an integer.\\\", x);\\n  }\\n  if (! isInteger(y)) {\\n    errbacks.throwDomainError('quotient: the second argument ' + y.toString() +\\n                              \\\" is not an integer.\\\", y);\\n  }\\n  return _integerQuotient(x, y);\\n};\\n\\n/* @stopify flat */ \\nfunction remainder(x, y, errbacks) {\\n  if (isInteger(x) && isInteger(y)) {\\n    return _integerRemainder(x, y);\\n  } else if (isRational(x) && isRational(y)) {\\n    var xn = numerator(x); var xd = denominator(x);\\n    var yn = numerator(y); var yd = denominator(y);\\n    var new_d = lcm(xd, [yd], errbacks);\\n    var new_xn = multiply(xn, divide(new_d, xd, errbacks), errbacks);\\n    var new_yn = multiply(yn, divide(new_d, yd, errbacks), errbacks);\\n    return divide(remainder(new_xn, new_yn, errbacks), new_d, errbacks);\\n  } else {\\n    var res = toFixnum(x) % toFixnum(y);\\n    return Roughnum.makeInstance(res, errbacks);\\n  }\\n};\\n\\n//////////////////////////////////////////////////////////////////////\\n\\n// Helpers\\n\\n// isOverflow: javascript-number -> boolean\\n// Returns true if we consider the number an overflow.\\nvar MIN_FIXNUM = -(9e15);\\nvar MAX_FIXNUM = (9e15);\\n/* @stopify flat */ \\nfunction isOverflow(n) {\\n  return (n < MIN_FIXNUM ||  MAX_FIXNUM < n);\\n};\\n\\n// negate: pyretnum -> pyretnum\\n// multiplies a number times -1.\\n/* @stopify flat */ \\nfunction negate(n, errbacks) {\\n  if (typeof(n) === 'number') {\\n    return -n;\\n  }\\n  return n.negate(errbacks);\\n};\\n\\n// halve: pyretnum -> pyretnum\\n// Divide a number by 2.\\n/* @stopify flat */ \\nfunction halve(n, errbacks) {\\n  return divide(n, 2, errbacks);\\n};\\n\\n// fastExpt: computes n^k by squaring.\\n// n^k = (n^2)^(k/2)\\n// Assumes k is non-negative integer.\\n/* @stopify flat */ \\nfunction fastExpt(n, k, errbacks) {\\n  var acc = 1;\\n  while (true) {\\n    if (_integerIsZero(k)) {\\n      return acc;\\n    }\\n    if (equals(modulo(k, 2, errbacks), 0, errbacks)) {\\n      n = multiply(n, n, errbacks);\\n      k = divide(k, 2, errbacks);\\n    } else {\\n      acc = multiply(acc, n, errbacks);\\n      k = subtract(k, 1, errbacks);\\n    }\\n  }\\n};\\n\\n//////////////////////////////////////////////////////////////////////\\n//////////////////////////////////////////////////////////////////////\\n//////////////////////////////////////////////////////////////////////\\n//////////////////////////////////////////////////////////////////////\\n//////////////////////////////////////////////////////////////////////\\n\\n// Integer operations\\n// Integers are either represented as fixnums or as BigIntegers.\\n\\n// makeIntegerBinop: (fixnum fixnum -> X) (BigInteger BigInteger -> X) -> X\\n// Helper to collect the common logic for coercing integer fixnums or bignums to a\\n// common type before doing an operation.\\n/* @stopify flat */ \\nfunction makeIntegerBinop(onFixnums, onBignums, options) {\\n  options = options || {};\\n  return (/* @stopify flat */ function(m, n) {\\n    if (m instanceof Rational) {\\n      m = numerator(m);\\n    }\\n\\n    if (n instanceof Rational) {\\n      n = numerator(n);\\n    }\\n\\n    if (typeof(m) === 'number' && typeof(n) === 'number') {\\n      var result = onFixnums(m, n);\\n      if (! isOverflow(result) ||\\n          (options.ignoreOverflow)) {\\n        return result;\\n      }\\n    }\\n    if (m instanceof Roughnum || n instanceof Roughnum) {\\n      return Roughnum.makeInstance(\\n        onFixnums(toFixnum(m), toFixnum(n)), errbacks);\\n    }\\n    if (typeof(m) === 'number') {\\n      m = makeBignum(m);\\n    }\\n    if (typeof(n) === 'number') {\\n      n = makeBignum(n);\\n    }\\n    return onBignums(m, n);\\n  });\\n};\\n\\n/* @stopify flat */ \\nfunction makeIntegerUnOp(onFixnums, onBignums, options, errbacks) {\\n  options = options || {};\\n  return (/* @stopify flat */ function(m) {\\n    if (m instanceof Rational) {\\n      m = numerator(m);\\n    }\\n\\n    if (typeof(m) === 'number') {\\n      var result = onFixnums(m);\\n      if (! isOverflow(result) ||\\n          (options.ignoreOverflow)) {\\n        return result;\\n      }\\n    }\\n    if (m instanceof Roughnum) {\\n      return Roughnum.makeInstance(onFixnums(toFixnum(m)), errbacks);\\n    }\\n    if (typeof(m) === 'number') {\\n      m = makeBignum(m);\\n    }\\n    return onBignums(m);\\n  });\\n};\\n\\n// _integerModulo: integer-pyretnum integer-pyretnum -> integer-pyretnum\\nvar _integerModulo = makeIntegerBinop(\\n  /* @stopify flat */ function(m, n) {\\n    return m % n;\\n  },\\n  /* @stopify flat */ function(m, n) {\\n    return bnMod.call(m, n);\\n  });\\n\\n// _integerGcd: integer-pyretnum integer-pyretnum -> integer-pyretnum\\nvar _integerGcd = makeIntegerBinop(\\n  /* @stopify flat */ function(a, b) {\\n    var t;\\n    while (b !== 0) {\\n      t = a;\\n      a = b;\\n      b = t % b;\\n    }\\n    return a;\\n  },\\n  /* @stopify flat */ function(m, n) {\\n    return bnGCD.call(m, n);\\n  });\\n\\n// _integerIsZero: integer-pyretnum -> boolean\\n// Returns true if the number is zero.\\nvar _integerIsZero = makeIntegerUnOp(\\n  /* @stopify flat */ function(n){\\n    return n === 0;\\n  },\\n  /* @stopify flat */ function(n) {\\n    return bnEquals.call(n, BigInteger.ZERO);\\n  }\\n);\\n\\n// _integerIsOne: integer-pyretnum -> boolean\\nvar _integerIsOne = makeIntegerUnOp(\\n  /* @stopify flat */ function(n) {\\n    return n === 1;\\n  },\\n  /* @stopify flat */ function(n) {\\n    return bnEquals.call(n, BigInteger.ONE);\\n  });\\n\\n// _integerIsNegativeOne: integer-pyretnum -> boolean\\nvar _integerIsNegativeOne = makeIntegerUnOp(\\n  /* @stopify flat */ function(n) {\\n    return n === -1;\\n  },\\n  /* @stopify flat */ function(n) {\\n    return bnEquals.call(n, BigInteger.NEGATIVE_ONE);\\n  });\\n\\n// _integerAdd: integer-pyretnum integer-pyretnum -> integer-pyretnum\\nvar _integerAdd = makeIntegerBinop(\\n  /* @stopify flat */ function(m, n) {\\n    return m + n;\\n  },\\n  /* @stopify flat */ function(m, n) {\\n    return bnAdd.call(m, n);\\n  });\\n\\n// _integerSubtract: integer-pyretnum integer-pyretnum -> integer-pyretnum\\nvar _integerSubtract = makeIntegerBinop(\\n  /* @stopify flat */ function(m, n) {\\n    return m - n;\\n  },\\n  /* @stopify flat */ function(m, n) {\\n    return bnSubtract.call(m, n);\\n  });\\n\\n// _integerMultiply: integer-pyretnum integer-pyretnum -> integer-pyretnum\\nvar _integerMultiply = makeIntegerBinop(\\n  /* @stopify flat */ function(m, n) {\\n    return m * n;\\n  },\\n  /* @stopify flat */ function(m, n) {\\n    return bnMultiply.call(m, n);\\n  });\\n\\n//_integerQuotient: integer-pyretnum integer-pyretnum -> integer-pyretnum\\nvar _integerQuotient = makeIntegerBinop(\\n  /* @stopify flat */ function(m, n) {\\n    return ((m - (m % n))/ n);\\n  },\\n  /* @stopify flat */ function(m, n) {\\n    return bnDivide.call(m, n);\\n  });\\n\\nvar _integerRemainder = makeIntegerBinop(\\n  /* @stopify flat */ function(m, n) {\\n    return m % n;\\n  },\\n  /* @stopify flat */ function(m, n) {\\n    return bnRemainder.call(m, n);\\n  });\\n\\n// splitIntIntoMantissaExpt: integer-pyretnum -> [JS-double, JS-int]\\n// \\n// splitIntIntoMantissaExpt takes an integer s (either unboxed or BigInteger)\\n//   and returns [mantissa, exponent]\\n//   such that s ~= mantissa * 10^exponent.\\n// mantissa is a JS-double, and is chosen to have one non-zero digit\\n//   to the left of the decimal point.\\n// Because mantissa is a JS-double, there is in general a loss of precision.\\n// splitIntIntoMantissaExpt is used to create a best-possible JS-double version\\n//   of its argument arbitrarily precise integer.\\n// E.g., splitIntIntoMantissaExpt(256) returns\\n//   [2.56, 2]\\n// splitIntIntoMantissaExpt(111222333444555666777888999) returns\\n//   [1.1122233344455567, 26]\\n//\\n/* @stopify flat */ \\nfunction splitIntIntoMantissaExpt(s) {\\n  var str = s.toString();\\n  var c0 = str[0];\\n  var sign = '';\\n\\n  if (c0 === '-' || c0 === '+') {\\n    str = str.substring(1);\\n    if (c0 === '-') {\\n      sign = '-';\\n    }\\n  }\\n\\n  var d0 = str[0];\\n  str = str.substring(1);\\n\\n  var mantissa = Number(sign + d0 + '.' + str);\\n  var expt = str.length;\\n\\n  return [mantissa, expt];\\n};\\n\\n// _integerDivideToFixnum: integer-pyretnum integer-pyretnum -> fixnum\\n//\\n// _integerDivideToFixnum takes two integers (possibly BigIntegers) and \\n//   returns the best fixnum representing their quotient.\\n// If the args are both JS-doubles, the JS quotient is returned if it\\n//   doesn't overflow.\\n// If it does overflow, or if at least one of the args is a BigInt, then\\n//   splitIntIntoMantissaExpt is used to convert the args to\\n//   [mantissa, exponent] form. The result a*10^b, where\\n//   a = the mantissae's quotient, and\\n//   b = the exponents' difference\\n//\\nvar _integerDivideToFixnum = makeIntegerBinop(\\n  /* @stopify flat */ function(m, n) {\\n    return m / n;\\n  },\\n  /* @stopify flat */ function(m, n) {\\n    var xm = splitIntIntoMantissaExpt(m);\\n    var xn = splitIntIntoMantissaExpt(n);\\n    var r = Number(String(xm[0] / xn[0]) + 'e' + \\n      String(xm[1] - xn[1]));\\n    return r;\\n  },\\n  { ignoreOverflow: false,\\n    doNotCoerceToFloating: true\\n  }\\n);\\n\\n// _integerEquals: integer-pyretnum integer-pyretnum -> boolean\\nvar _integerEquals = makeIntegerBinop(\\n  /* @stopify flat */ function(m, n) {\\n    return m === n;\\n  },\\n  /* @stopify flat */ function(m, n) {\\n    return bnEquals.call(m, n);\\n  },\\n  {doNotCoerceToFloating: true});\\n\\n// _integerGreaterThan: integer-pyretnum integer-pyretnum -> boolean\\nvar _integerGreaterThan = makeIntegerBinop(\\n  /* @stopify flat */ function(m, n) {\\n    return m > n;\\n  },\\n  /* @stopify flat */ function(m, n) {\\n    return bnCompareTo.call(m, n) > 0;\\n  },\\n  {doNotCoerceToFloating: true});\\n\\n// _integerLessThan: integer-pyretnum integer-pyretnum -> boolean\\nvar _integerLessThan = makeIntegerBinop(\\n  /* @stopify flat */ function(m, n) {\\n    return m < n;\\n  },\\n  /* @stopify flat */ function(m, n) {\\n    return bnCompareTo.call(m, n) < 0;\\n  },\\n  {doNotCoerceToFloating: true});\\n\\n// _integerGreaterThanOrEqual: integer-pyretnum integer-pyretnum -> boolean\\nvar _integerGreaterThanOrEqual = makeIntegerBinop(\\n  /* @stopify flat */ function(m, n) {\\n    return m >= n;\\n  },\\n  /* @stopify flat */ function(m, n) {\\n    return bnCompareTo.call(m, n) >= 0;\\n  },\\n  {doNotCoerceToFloating: true});\\n\\n// _integerLessThanOrEqual: integer-pyretnum integer-pyretnum -> boolean\\nvar _integerLessThanOrEqual = makeIntegerBinop(\\n  /* @stopify flat */ function(m, n) {\\n    return m <= n;\\n  },\\n  /* @stopify flat */ function(m, n) {\\n    return bnCompareTo.call(m, n) <= 0;\\n  },\\n  {doNotCoerceToFloating: true});\\n\\n//////////////////////////////////////////////////////////////////////\\n// The boxed number types are expected to implement the following\\n// interface.\\n//\\n// toString: -> string\\n\\n// isFinite: -> boolean\\n\\n// isInteger: -> boolean\\n// Produce true if this number can be coerced into an integer.\\n\\n// isRational: -> boolean\\n// Produce true if the number is rational.\\n\\n// isExact === isRational\\n\\n// isReal: -> boolean\\n// Produce true if the number is real.\\n\\n// toRational: -> pyretnum\\n// Produce an exact number.\\n\\n// toExact === toRational\\n\\n// toRoughnum: -> pyretnum\\n// Produce a roughnum.\\n\\n// toFixnum: -> fixnum\\n// Produce a javascript number.\\n\\n// greaterThan: pyretnum -> boolean\\n// Compare against instance of the same type.\\n\\n// greaterThanOrEqual: pyretnum -> boolean\\n// Compare against instance of the same type.\\n\\n// lessThan: pyretnum -> boolean\\n// Compare against instance of the same type.\\n\\n// lessThanOrEqual: pyretnum -> boolean\\n// Compare against instance of the same type.\\n\\n// add: pyretnum -> pyretnum\\n// Add with an instance of the same type.\\n\\n// subtract: pyretnum -> pyretnum\\n// Subtract with an instance of the same type.\\n\\n// multiply: pyretnum -> pyretnum\\n// Multiply with an instance of the same type.\\n\\n// divide: pyretnum -> pyretnum\\n// Divide with an instance of the same type.\\n\\n// numerator: -> pyretnum\\n// Return the numerator.\\n\\n// denominator: -> pyretnum\\n// Return the denominator.\\n\\n// integerSqrt: -> pyretnum\\n// Produce the integer square root.\\n\\n// sqrt: -> pyretnum\\n// Produce the square root.\\n\\n// abs: -> pyretnum\\n// Produce the absolute value.\\n\\n// floor: -> pyretnum\\n// Produce the floor.\\n\\n// ceiling: -> pyretnum\\n// Produce the ceiling.\\n\\n// log: -> pyretnum\\n// Produce the log.\\n\\n// atan: -> pyretnum\\n// Produce the arc tangent.\\n\\n// cos: -> pyretnum\\n// Produce the cosine.\\n\\n// sin: -> pyretnum\\n// Produce the sine.\\n\\n// expt: pyretnum -> pyretnum\\n// Produce the power to the input.\\n\\n// exp: -> pyretnum\\n// Produce e raised to the given power.\\n\\n// acos: -> pyretnum\\n// Produce the arc cosine.\\n\\n// asin: -> pyretnum\\n// Produce the arc sine.\\n\\n// round: -> pyretnum\\n// Round to the nearest integer.\\n\\n// equals: pyretnum -> boolean\\n// Produce true if the given number of the same type is equal.\\n\\n//////////////////////////////////////////////////////////////////////\\n\\n// Rationals\\n\\n/* @stopify flat */ \\nfunction Rational(n, d) {\\n  this.n = n;\\n  this.d = d;\\n};\\n\\nRational.makeInstance = /* @stopify flat */ function(n, d, errbacks) {\\n  if (n === undefined)\\n    errbacks.throwUndefinedValue(\\\"n undefined\\\", n, d);\\n\\n  if (d === undefined) { d = 1; }\\n\\n  if (_integerLessThan(d, 0)) {\\n    n = negate(n, errbacks);\\n    d = negate(d, errbacks);\\n  }\\n\\n  var divisor = _integerGcd(abs(n, errbacks), abs(d, errbacks));\\n  n = _integerQuotient(n, divisor);\\n  d = _integerQuotient(d, divisor);\\n\\n  // Optimization: if we can get around construction the rational\\n  // in favor of just returning n, do it:\\n  if (_integerIsOne(d) || _integerIsZero(n)) {\\n    return n;\\n  }\\n\\n  return new Rational(n, d);\\n};\\n\\nRational.prototype.toString = /* @stopify flat */ function() {\\n  if (_integerIsOne(this.d)) {\\n    return this.n.toString() + \\\"\\\";\\n  } else {\\n    return this.n.toString() + \\\"/\\\" + this.d.toString();\\n  }\\n};\\n\\nRational.prototype.isFinite = /* @stopify flat */ function() {\\n  return true;\\n};\\n\\nRational.prototype.equals = /* @stopify flat */ function(other, errbacks) {\\n  return (other instanceof Rational &&\\n          _integerEquals(this.n, other.n) &&\\n          _integerEquals(this.d, other.d));\\n};\\n\\nRational.prototype.isInteger = /* @stopify flat */ function() {\\n  return _integerIsOne(this.d);\\n};\\n\\nRational.prototype.isRational = /* @stopify flat */ function() {\\n  return true;\\n};\\n\\nRational.prototype.isExact = Rational.prototype.isRational;\\n\\nRational.prototype.isReal = /* @stopify flat */ function() {\\n  return true;\\n};\\n\\nRational.prototype.isRoughnum = /* @stopify flat */ function() {\\n  return false;\\n};\\n\\nRational.prototype.isPositive = /* @stopify flat */ function() {\\n  // don't care about this.d\\n  return this.n > 0;\\n};\\n\\nRational.prototype.isNonNegative = /* @stopify flat */ function() {\\n  return this.n >= 0;\\n};\\n\\nRational.prototype.isNegative = /* @stopify flat */ function() {\\n  return this.n < 0;\\n};\\n\\nRational.prototype.isNonPositive = /* @stopify flat */ function() {\\n  return this.n <= 0;\\n};\\n\\nRational.prototype.add = /* @stopify flat */ function(other, errbacks) {\\n  return Rational.makeInstance(_integerAdd(_integerMultiply(this.n, other.d),\\n                                           _integerMultiply(this.d, other.n)),\\n                               _integerMultiply(this.d, other.d), errbacks);\\n};\\n\\nRational.prototype.subtract = /* @stopify flat */ function(other, errbacks) {\\n  return Rational.makeInstance(_integerSubtract(_integerMultiply(this.n, other.d),\\n                                                _integerMultiply(this.d, other.n)),\\n                               _integerMultiply(this.d, other.d), errbacks);\\n};\\n\\nRational.prototype.negate = /* @stopify flat */ function(errbacks) {\\n  return Rational.makeInstance(negate(this.n, errbacks), this.d, errbacks)\\n};\\n\\nRational.prototype.multiply = /* @stopify flat */ function(other, errbacks) {\\n  return Rational.makeInstance(_integerMultiply(this.n, other.n),\\n                               _integerMultiply(this.d, other.d), errbacks);\\n};\\n\\nRational.prototype.divide = /* @stopify flat */ function(other, errbacks) {\\n  if (_integerIsZero(this.d) || _integerIsZero(other.n)) {  // dead code!\\n    errbacks.throwDivByZero(\\\"/: division by zero\\\", this, other);\\n  }\\n  return Rational.makeInstance(_integerMultiply(this.n, other.d),\\n                               _integerMultiply(this.d, other.n), errbacks);\\n};\\n\\nRational.prototype.toRational = /* @stopify flat */ function() {\\n  return this;\\n};\\n\\nRational.prototype.toExact = Rational.prototype.toRational;\\n\\n\\nRational.prototype.toFixnum = /* @stopify flat */ function() {\\n  return _integerDivideToFixnum(this.n, this.d);\\n};\\n\\nRational.prototype.toRoughnum = /* @stopify flat */ function(errbacks) {\\n  return Roughnum.makeInstance(this.toFixnum(), errbacks);\\n};\\n\\nRational.prototype.numerator = /* @stopify flat */ function() {\\n  return this.n;\\n};\\n\\nRational.prototype.denominator = /* @stopify flat */ function() {\\n  return this.d;\\n};\\n\\nRational.prototype.greaterThan = /* @stopify flat */ function(other, errbacks) {\\n  return _integerGreaterThan(_integerMultiply(this.n, other.d),\\n                             _integerMultiply(this.d, other.n));\\n};\\n\\nRational.prototype.greaterThanOrEqual = /* @stopify flat */ function(other, errbacks) {\\n  return _integerGreaterThanOrEqual(_integerMultiply(this.n, other.d),\\n                                    _integerMultiply(this.d, other.n));\\n};\\n\\nRational.prototype.lessThan = /* @stopify flat */ function(other, errbacks) {\\n  return _integerLessThan(_integerMultiply(this.n, other.d),\\n                          _integerMultiply(this.d, other.n));\\n};\\n\\nRational.prototype.lessThanOrEqual = /* @stopify flat */ function(other, errbacks) {\\n  return _integerLessThanOrEqual(_integerMultiply(this.n, other.d),\\n                                 _integerMultiply(this.d, other.n));\\n};\\n\\nRational.prototype.integerSqrt = /* @stopify flat */ function(errbacks) {\\n  var result = sqrt(this);\\n  return toRational(floor(result, errbacks), errbacks);\\n};\\n\\nRational.prototype.sqrt = /* @stopify flat */ function(errbacks) {\\n  var newN = sqrt(this.n);\\n  var newD = sqrt(this.d);\\n  if (isRational(newN) && isRational(newD) &&\\n      equals(floor(newN), newN) &&\\n      equals(floor(newD), newD)) {\\n    return Rational.makeInstance(newN, newD, errbacks);\\n  } else {\\n    return divide(newN, newD, errbacks);\\n  }\\n};\\n\\nRational.prototype.abs = /* @stopify flat */ function(errbacks) {\\n  return Rational.makeInstance(abs(this.n, errbacks),\\n                               this.d, errbacks);\\n};\\n\\nRational.prototype.floor = /* @stopify flat */ function(errbacks) {\\n  var quotient = _integerQuotient(this.n, this.d);\\n  if (_integerLessThan(this.n, 0)) {\\n    return subtract(quotient, 1, errbacks);\\n  } else {\\n    return quotient;\\n  }\\n};\\n\\nRational.prototype.ceiling = /* @stopify flat */ function(errbacks) {\\n  var quotient = _integerQuotient(this.n, this.d);\\n  if (_integerLessThan(this.n, 0)) {\\n    return quotient;\\n  } else {\\n    return add(quotient, 1, errbacks);\\n  }\\n};\\n\\nRational.prototype.round = /* @stopify flat */ function(errbacks) {\\n  var halfintp = equals(this.d, 2);\\n  var negativep = _integerLessThan(this.n, 0);\\n  var n = this.n;\\n  if (negativep) {\\n    n = negate(n, errbacks);\\n  }\\n  var quo = _integerQuotient(n, this.d);\\n  if (halfintp) {\\n    // rounding half to away from 0\\n    // uncomment following if rounding half to even\\n    // if (_integerIsOne(_integerModulo(quo, 2)))\\n    quo = add(quo, 1, errbacks);\\n  } else {\\n    var rem = _integerRemainder(n, this.d);\\n    if (greaterThan(multiply(rem, 2, errbacks), this.d, errbacks)) {\\n      quo = add(quo, 1, errbacks);\\n    }\\n  }\\n  if (negativep) {\\n    quo = negate(quo, errbacks);\\n  }\\n  return quo;\\n};\\n\\nRational.prototype.roundEven = /* @stopify flat */ function(errbacks) {\\n  // rounds half-integers to even\\n  var halfintp = equals(this.d, 2, errbacks);\\n  var negativep = _integerLessThan(this.n, 0);\\n  var n = this.n;\\n  if (negativep) n = negate(n, errbacks);\\n  var quo = _integerQuotient(n, this.d);\\n  if (halfintp) {\\n    if (_integerIsOne(_integerModulo(quo, 2)))\\n      quo = add(quo, 1, errbacks);\\n  } else {\\n    var rem = _integerRemainder(n, this.d);\\n    if (greaterThan(multiply(rem, 2, errbacks), this.d, errbacks))\\n      quo = add(quo, 1, errbacks);\\n  }\\n  if (negativep) quo = negate(quo, errbacks);\\n  return quo;\\n};\\n\\nRational.prototype.log = /* @stopify flat */ function(errbacks){\\n  return Roughnum.makeInstance(Math.log(this.toFixnum()), errbacks);\\n};\\n\\nRational.prototype.tan = /* @stopify flat */ function(errbacks){\\n  return Roughnum.makeInstance(Math.tan(this.toFixnum()), errbacks);\\n};\\n\\nRational.prototype.atan = /* @stopify flat */ function(errbacks){\\n  return Roughnum.makeInstance(Math.atan(this.toFixnum()), errbacks);\\n};\\n\\nRational.prototype.cos = /* @stopify flat */ function(errbacks){\\n  return Roughnum.makeInstance(Math.cos(this.toFixnum()), errbacks);\\n};\\n\\nRational.prototype.sin = /* @stopify flat */ function(errbacks){\\n  return Roughnum.makeInstance(Math.sin(this.toFixnum()), errbacks);\\n};\\n\\nvar integerNthRoot = /* @stopify flat */ function(n, m, errbacks) {\\n  var guessPrev, guessToTheN;\\n  var guess = m;\\n\\n  // find closest integral zero of x^n - m = 0 using Newton-Raphson.\\n  // if k'th guess is x_k, then\\n  // x_{k+1} = floor( x_k - [(x_k)^n - m]/[n (x_k)^(n-1)] ).\\n  // Stop iteration if (x_k)^n is close enough to m, or\\n  // if x_k stops evolving\\n\\n  while(true) {\\n    guessToTheN = expt(guess, n, errbacks);\\n    if (lessThanOrEqual(guessToTheN, m, errbacks) &&\\n        lessThan(m, expt(add(guess, 1, errbacks), n, errbacks), errbacks)) break;\\n    guessPrev = guess;\\n    guess = floor(subtract(guess, divide(subtract(guessToTheN, m, errbacks),\\n          multiply(n, divide(guessToTheN, guess, errbacks), errbacks), errbacks), errbacks), errbacks);\\n    if (equals(guess, guessPrev, errbacks)) break;\\n  }\\n\\n  return guess;\\n};\\n\\nvar nthRoot = /* @stopify flat */ function(n, m, errbacks) {\\n  var mNeg = (sign(m) < 0);\\n  var mAbs = (mNeg ? abs(m, errbacks) : m);\\n  var approx;\\n\\n  if (mNeg && _integerModulo(n, 2) === 0)\\n    errbacks.throwDomainError('expt: taking even (' + n + ') root of negative integer ' + m);\\n\\n  approx = integerNthRoot(n, mAbs, errbacks);\\n  if (mNeg) approx = negate(approx, errbacks);\\n  if (eqv(expt(approx, n, errbacks), m, errbacks)) return approx;\\n\\n  approx = Roughnum.makeInstance(Math.pow(toFixnum(mAbs),\\n                                          toFixnum(divide(1,n, errbacks))), errbacks);\\n  return (mNeg ? negate(approx, errbacks) : approx);\\n};\\n\\nRational.prototype.expt = /* @stopify flat */ function(a, errbacks) {\\n  if (isInteger(a) && greaterThanOrEqual(a, 0, errbacks)) {\\n    return fastExpt(this, a, errbacks);\\n  } else if (_integerLessThanOrEqual(a.d, 8)) {\\n    var nRaisedToAn = expt(this.n, a.n, errbacks);\\n    var dRaisedToAn = expt(this.d, a.n, errbacks);\\n    var newN = nthRoot(a.d, nRaisedToAn, errbacks);\\n    var newD = nthRoot(a.d, dRaisedToAn, errbacks);\\n    if (isRational(newN) && isRational(newD) &&\\n        equals(floor(newN), newN, errbacks) &&\\n        equals(floor(newD), newD, errbacks)) {\\n      return Rational.makeInstance(newN, newD, errbacks);\\n    } else {\\n      return divide(newN, newD, errbacks);\\n   }\\n  } else {\\n    if (this.isNegative() && !a.isInteger())\\n      errbacks.throwDomainError('expt: raising negative number ' + this + ' to nonintegral power ' + a);\\n    return Roughnum.makeInstance(Math.pow(this.toFixnum(), a.toFixnum()), errbacks);\\n  }\\n};\\n\\nRational.prototype.exp = /* @stopify flat */ function(errbacks){\\n  var res = Math.exp(this.toFixnum());\\n  if (!isFinite(res))\\n    errbacks.throwDomainError('exp: argument too large: ' + this);\\n  return Roughnum.makeInstance(res, errbacks);\\n};\\n\\nRational.prototype.acos = /* @stopify flat */ function(errbacks){\\n  return acos(this.toFixnum(), errbacks);\\n};\\n\\nRational.prototype.asin = /* @stopify flat */ function(errbacks){\\n  return asin(this.toFixnum(), errbacks);\\n};\\n\\n// sign: Number -> {-1, 0, 1}\\nvar sign = /* @stopify flat */ function(n, errbacks) {\\n  if (lessThan(n, 0, errbacks)) {\\n    return -1;\\n  } else if (greaterThan(n, 0, errbacks)) {\\n    return 1;\\n  } else {\\n    return 0;\\n  }\\n};\\n\\n// Roughnums\\n\\nvar Roughnum = /* @stopify flat */ function(n, errbacks) {\\n  if (!(typeof(n) === 'number'))\\n    errbacks.throwGeneralError('roughnum constructor got unsuitable arg ' + n);\\n  this.n = n;\\n};\\n\\nRoughnum.makeInstance = /* @stopify flat */ function(n, errbacks) {\\n  if (typeof(n) === 'number' && !isFinite(n)) {\\n    errbacks.throwDomainError('roughnum overflow error');\\n  }\\n  return new Roughnum(n, errbacks);\\n};\\n\\nRoughnum.prototype.isFinite = /* @stopify flat */ function() {\\n  //actually always true, as we don't store overflows\\n  return (isFinite(this.n));\\n};\\n\\nRoughnum.prototype.toRational = /* @stopify flat */ function(errbacks) {\\n  if (!isFinite(this.n)) {\\n    // this _should_ be dead, as we don't store overflows\\n    errbacks.throwInternalError(\\\"toRational: no exact representation for \\\" + this);\\n  }\\n\\n  return fromString(this.n.toString(), errbacks);\\n};\\n\\nRoughnum.prototype.toExact = Roughnum.prototype.toRational;\\n\\nRoughnum.prototype.toString = /* @stopify flat */ function() {\\n  return '~' + this.n.toString();\\n};\\n\\nRoughnum.prototype.equals = /* @stopify flat */ function(other, errbacks) {\\n  errbacks.throwIncomparableValues(\\\"roughnums cannot be compared for equality\\\");\\n};\\n\\nRoughnum.prototype.isRational = /* @stopify flat */ function() {\\n  return false;\\n};\\n\\nRoughnum.prototype.isExact = Roughnum.prototype.isRational;\\n\\nRoughnum.prototype.isInteger = /* @stopify flat */ function() {\\n  return false;\\n};\\n\\nRoughnum.prototype.isReal = /* @stopify flat */ function() {\\n  return true;\\n};\\n\\nRoughnum.prototype.isRoughnum = /* @stopify flat */ function() {\\n  return true;\\n};\\n\\nRoughnum.prototype.isPositive = /* @stopify flat */ function() {\\n  return this.n > 0;\\n};\\n\\nRoughnum.prototype.isNonNegative = /* @stopify flat */ function() {\\n  return this.n >= 0;\\n};\\n\\nRoughnum.prototype.isNegative = /* @stopify flat */ function() {\\n  return this.n < 0;\\n};\\n\\nRoughnum.prototype.isNonPositive = /* @stopify flat */ function() {\\n  return this.n <= 0;\\n};\\n\\nRoughnum.prototype.add = /* @stopify flat */ function(other, errbacks) {\\n  return Roughnum.makeInstance(this.n + other.n, errbacks);\\n};\\n\\nRoughnum.prototype.subtract = /* @stopify flat */ function(other, errbacks) {\\n  return Roughnum.makeInstance(this.n - other.n, errbacks);\\n};\\n\\nRoughnum.prototype.negate = /* @stopify flat */ function(errbacks) {\\n  return Roughnum.makeInstance(-this.n, errbacks);\\n};\\n\\nRoughnum.prototype.multiply = /* @stopify flat */ function(other, errbacks) {\\n  return Roughnum.makeInstance(this.n * other.n, errbacks);\\n};\\n\\nRoughnum.prototype.divide = /* @stopify flat */ function(other, errbacks) {\\n  return Roughnum.makeInstance(this.n / other.n, errbacks);\\n};\\n\\nRoughnum.prototype.toFixnum = /* @stopify flat */ function() {\\n  return this.n;\\n};\\n\\nRoughnum.prototype.toRoughnum = /* @stopify flat */ function(errbacks) {\\n  return this;\\n};\\n\\nRoughnum.prototype.numerator = /* @stopify flat */ function() {\\n  var stringRep = this.n.toString();\\n  var match = stringRep.match(/^(.*)\\\\.(.*)$/);\\n  if (match) {\\n    var afterDecimal = parseInt(match[2]);\\n    var factorToInt = Math.pow(10, match[2].length);\\n    var extraFactor = _integerGcd(factorToInt, afterDecimal);\\n    var multFactor = factorToInt / extraFactor;\\n    return Roughnum.makeInstance( Math.round(this.n * multFactor) );\\n  } else {\\n    return this;\\n  }\\n};\\n\\nRoughnum.prototype.denominator = /* @stopify flat */ function() {\\n  var stringRep = this.n.toString();\\n  var match = stringRep.match(/^(.*)\\\\.(.*)$/);\\n  if (match) {\\n    var afterDecimal = parseInt(match[2]);\\n    var factorToInt = Math.pow(10, match[2].length);\\n    var extraFactor = _integerGcd(factorToInt, afterDecimal);\\n    return Roughnum.makeInstance( Math.round(factorToInt/extraFactor) );\\n  } else {\\n    return Roughnum.makeInstance(1);\\n  }\\n};\\n\\nRoughnum.prototype.floor = /* @stopify flat */ function(errbacks) {\\n  return Math.floor(this.n);\\n};\\n\\nRoughnum.prototype.ceiling = /* @stopify flat */ function(errbacks) {\\n  return Math.ceil(this.n);\\n};\\n\\nRoughnum.prototype.round = /* @stopify flat */ function(errbacks){\\n  var negativep = (this.n < 0);\\n  var n = this.n;\\n  if (negativep) n = -n;\\n  var res = Math.round(n);\\n  if (negativep) res = -res;\\n  return res;\\n};\\n\\nRoughnum.prototype.roundEven = /* @stopify flat */ function(errbacks) {\\n  var negativep = (this.n < 0);\\n  var n = this.n;\\n  if (negativep) n = -n;\\n  var res = Math.round(n);\\n  if ((Math.abs(n - res) === 0.5) && (res % 2 === 1))\\n    res -= 1;\\n  return res;\\n};\\n\\nRoughnum.prototype.greaterThan = /* @stopify flat */ function(other, errbacks) {\\n  return this.n > other.n;\\n};\\n\\nRoughnum.prototype.greaterThanOrEqual = /* @stopify flat */ function(other, errbacks) {\\n  return this.n >= other.n;\\n};\\n\\nRoughnum.prototype.lessThan = /* @stopify flat */ function(other, errbacks) {\\n  return this.n < other.n;\\n};\\n\\nRoughnum.prototype.lessThanOrEqual = /* @stopify flat */ function(other, errbacks) {\\n  return this.n <= other.n;\\n};\\n\\nRoughnum.prototype.integerSqrt = /* @stopify flat */ function(errbacks) {\\n  if (isInteger(this)) {\\n    if(this.n >= 0) {\\n      return Roughnum.makeInstance(Math.floor(Math.sqrt(this.n)), errbacks);\\n    } else {\\n      errbacks.throwDomainError('integerSqrt of negative roughnum', this.n);\\n    }\\n  } else {\\n    errbacks.throwDomainError(\\\"integerSqrt: can only be applied to an integer\\\", this);\\n  }\\n};\\n\\nRoughnum.prototype.sqrt = /* @stopify flat */ function(errbacks) {\\n  return Roughnum.makeInstance(Math.sqrt(this.n), errbacks);\\n};\\n\\nRoughnum.prototype.abs = /* @stopify flat */ function(errbacks) {\\n  return Roughnum.makeInstance(Math.abs(this.n), errbacks);\\n};\\n\\nRoughnum.prototype.log = /* @stopify flat */ function(errbacks){\\n  if (this.n < 0)\\n    errbacks.throwDomainError('log of negative roughnum', this.n);\\n  else\\n    return Roughnum.makeInstance(Math.log(this.n), errbacks);\\n};\\n\\nRoughnum.prototype.tan = /* @stopify flat */ function(errbacks){\\n  return Roughnum.makeInstance(Math.tan(this.n), errbacks);\\n};\\n\\nRoughnum.prototype.atan = /* @stopify flat */ function(errbacks){\\n  return Roughnum.makeInstance(Math.atan(this.n), errbacks);\\n};\\n\\nRoughnum.prototype.cos = /* @stopify flat */ function(errbacks){\\n  return Roughnum.makeInstance(Math.cos(this.n), errbacks);\\n};\\n\\nRoughnum.prototype.sin = /* @stopify flat */ function(errbacks){\\n  return Roughnum.makeInstance(Math.sin(this.n), errbacks);\\n};\\n\\nRoughnum.prototype.expt = /* @stopify flat */ function(a, errbacks){\\n  if (this.n === 1) {\\n    return this;\\n  } else {\\n    return Roughnum.makeInstance(Math.pow(this.n, a.n), errbacks);\\n  }\\n};\\n\\nRoughnum.prototype.exp = /* @stopify flat */ function(errbacks){\\n  var res = Math.exp(this.n);\\n  if (!isFinite(res))\\n    errbacks.throwDomainError('exp: argument too large: ' + this);\\n  return Roughnum.makeInstance(res);\\n};\\n\\nRoughnum.prototype.acos = /* @stopify flat */ function(errbacks){\\n  return acos(this.n, errbacks);\\n};\\n\\nRoughnum.prototype.asin = /* @stopify flat */ function(errbacks){\\n  return asin(this.n, errbacks);\\n};\\n\\nvar rationalRegexp = new RegExp(\\\"^([+-]?\\\\\\\\d+)/(\\\\\\\\d+)$\\\");\\nvar digitRegexp = new RegExp(\\\"^[+-]?\\\\\\\\d+$\\\");\\nvar flonumRegexp = new RegExp(\\\"^([-+]?)(\\\\\\\\d+\\\\)((?:\\\\\\\\.\\\\\\\\d+)?)((?:[Ee][-+]?\\\\\\\\d+)?)$\\\");\\n\\n\\nvar roughnumDecRegexp = new RegExp(\\\"^~([-+]?\\\\\\\\d+(?:\\\\\\\\.\\\\\\\\d+)?(?:[Ee][-+]?\\\\\\\\d+)?)$\\\");\\n\\nvar roughnumRatRegexp = new RegExp(\\\"^~([+-]?\\\\\\\\d+)/(\\\\\\\\d+)$\\\");\\n\\n\\nvar scientificPattern = new RegExp(\\\"^([+-]?\\\\\\\\d*\\\\\\\\.?\\\\\\\\d*)[Ee]([+]?\\\\\\\\d+)$\\\");\\n\\n// fromString: string -> (pyretnum | false)\\nvar fromString = /* @stopify flat */ function(x, errbacks) {\\n  if (x.match(digitRegexp)) {\\n    var n = Number(x);\\n    if (isOverflow(n)) {\\n      return makeBignum(x);\\n    } else {\\n      return n;\\n    }\\n  }\\n\\n  var aMatch = x.match(rationalRegexp);\\n  if (aMatch) {\\n    return Rational.makeInstance(fromString(aMatch[1]),\\n                                 fromString(aMatch[2]), errbacks);\\n  }\\n\\n  aMatch = x.match(flonumRegexp);\\n  if (aMatch) {\\n    var negativeP = (aMatch[1] === \\\"-\\\");\\n    //\\n    var beforeDecimalString = aMatch[2];\\n    var beforeDecimal = 0;\\n    if (beforeDecimalString !== '') {\\n      beforeDecimal = makeBignum(beforeDecimalString);\\n    }\\n    //\\n    var afterDecimalString = aMatch[3];\\n    var denominatorTen = 1;\\n    var afterDecimal = 0;\\n    if (afterDecimalString !== '') {\\n      afterDecimalString = afterDecimalString.substring(1);\\n      denominatorTen = makeBignum('1' + new Array(afterDecimalString.length + 1).join('0'));\\n      if (afterDecimalString !== '') {\\n        afterDecimal = makeBignum(afterDecimalString);\\n      }\\n    }\\n    //\\n    var exponentString = aMatch[4];\\n    var exponentNegativeP = false;\\n    var exponent = 1;\\n    if (exponentString !== '') {\\n      exponentString = exponentString.substring(1);\\n      var exponentSign = exponentString.charAt(0);\\n      exponentNegativeP = (exponentSign === '-');\\n      if (exponentSign === '-' || exponentSign === '+') {\\n        exponentString = exponentString.substring(1);\\n      }\\n      exponent = makeBignum('1' + new Array(Number(exponentString) + 1).join('0'));\\n    }\\n\\n    var finalDen = denominatorTen;\\n    var finalNum = _integerAdd(_integerMultiply(beforeDecimal, denominatorTen), afterDecimal);\\n    if (negativeP) {\\n      finalNum = negate(finalNum, errbacks);\\n    }\\n    //\\n    if (!equals(exponent, 1)) {\\n      if (exponentNegativeP) {\\n        finalDen = _integerMultiply(finalDen, exponent);\\n      } else {\\n        finalNum = _integerMultiply(finalNum, exponent);\\n      }\\n    }\\n    return Rational.makeInstance(finalNum, finalDen, errbacks);\\n  }\\n\\n  aMatch = x.match(roughnumRatRegexp);\\n  if (aMatch) {\\n    return toRoughnum(Rational.makeInstance(fromString(aMatch[1]), fromString(aMatch[2])),\\n      errbacks);\\n  }\\n\\n  aMatch = x.match(roughnumDecRegexp);\\n  if (aMatch) {\\n    return Roughnum.makeInstance(Number(aMatch[1]), errbacks);\\n  }\\n\\n  return false; // if all else fails\\n\\n};\\n\\n///////////////////////////////////////////////////////////\\n\\n// recognizing numbers in (We)Scheme syntax:\\n\\n  var hashModifiersRegexp = new RegExp(\\\"^(#[ei]#[bodx]|#[bodx]#[ei]|#[bodxei])(.*)$\\\")\\n\\n  /* @stopify flat */ function schemeRationalRegexp(digits) { return new RegExp(\\\"^([+-]?[\\\"+digits+\\\"]+)/([\\\"+digits+\\\"]+)$\\\"); }\\n\\n  /* @stopify flat */ function matchComplexRegexp(radix, x, errbacks) {\\nvar sign = \\\"[+-]\\\";\\nvar maybeSign = \\\"[+-]?\\\";\\nvar digits = digitsForRadix(radix, errbacks)\\nvar expmark = \\\"[\\\"+expMarkForRadix(radix, errbacks)+\\\"]\\\"\\nvar digitSequence = \\\"[\\\"+digits+\\\"]+\\\"\\n\\nvar unsignedRational = digitSequence+\\\"/\\\"+digitSequence\\nvar rational = maybeSign + unsignedRational\\n\\nvar noDecimal = digitSequence\\nvar decimalNumOnRight = \\\"[\\\"+digits+\\\"]*\\\\\\\\.[\\\"+digits+\\\"]+\\\"\\nvar decimalNumOnLeft = \\\"[\\\"+digits+\\\"]+\\\\\\\\.[\\\"+digits+\\\"]*\\\"\\n\\nvar unsignedDecimal = \\\"(?:\\\" + noDecimal + \\\"|\\\" + decimalNumOnRight + \\\"|\\\" + decimalNumOnLeft + \\\")\\\"\\n\\nvar special = \\\"(?:inf\\\\.0|nan\\\\.0|inf\\\\.f|nan\\\\.f)\\\"\\n\\nvar unsignedRealNoExp = \\\"(?:\\\" + unsignedDecimal + \\\"|\\\" + unsignedRational + \\\")\\\"\\nvar unsignedReal = unsignedRealNoExp + \\\"(?:\\\" + expmark + maybeSign + digitSequence + \\\")?\\\"\\nvar unsignedRealOrSpecial = \\\"(?:\\\" + unsignedReal + \\\"|\\\" + special + \\\")\\\"\\nvar real = \\\"(?:\\\" + maybeSign + unsignedReal + \\\"|\\\" + sign + special + \\\")\\\"\\n\\nvar alt1 = new RegExp(\\\"^(\\\" + rational + \\\")\\\"\\n                           + \\\"(\\\" + sign + unsignedRational + \\\"?)\\\"\\n                           + \\\"i$\\\");\\nvar alt2 = new RegExp(\\\"^(\\\" + real + \\\")?\\\"\\n                           + \\\"(\\\" + sign + unsignedRealOrSpecial + \\\"?)\\\"\\n                           + \\\"i$\\\");\\nvar alt3 = new RegExp(\\\"^(\\\" + real + \\\")@(\\\" + real + \\\")$\\\");\\n\\nvar match1 = x.match(alt1)\\nvar match2 = x.match(alt2)\\nvar match3 = x.match(alt3)\\n\\nreturn match1 ? match1 :\\n       match2 ? match2 :\\n       match3 ? match3 :\\n     /* else */ false\\n  }\\n\\n  /* @stopify flat */ function schemeDigitRegexp(digits) { return new RegExp(\\\"^[+-]?[\\\"+digits+\\\"]+$\\\"); }\\n  /**\\n  /* NB: !!!! flonum regexp only matches \\\"X.\\\", \\\".X\\\", or \\\"X.X\\\", NOT \\\"X\\\", this\\n  /* must be separately checked with schemeDigitRegexp.\\n  /* I know this seems dumb, but the alternative would be that this regexp\\n  /* returns six matches, which also seems dumb.\\n  /***/\\n  /* @stopify flat */ function schemeFlonumRegexp(digits) {\\nvar decimalNumOnRight = \\\"([\\\"+digits+\\\"]*)\\\\\\\\.([\\\"+digits+\\\"]+)\\\"\\nvar decimalNumOnLeft = \\\"([\\\"+digits+\\\"]+)\\\\\\\\.([\\\"+digits+\\\"]*)\\\"\\nreturn new RegExp(\\\"^(?:([+-]?)(\\\" +\\n                        decimalNumOnRight+\\\"|\\\"+decimalNumOnLeft +\\n                        \\\"))$\\\");\\n  }\\n  /* @stopify flat */ function schemeScientificPattern(digits, exp_mark) {\\nvar noDecimal = \\\"[\\\"+digits+\\\"]+\\\"\\nvar decimalNumOnRight = \\\"[\\\"+digits+\\\"]*\\\\\\\\.[\\\"+digits+\\\"]+\\\"\\nvar decimalNumOnLeft = \\\"[\\\"+digits+\\\"]+\\\\\\\\.[\\\"+digits+\\\"]*\\\"\\nreturn new RegExp(\\\"^(?:([+-]?\\\" +\\n      \\\"(?:\\\"+noDecimal+\\\"|\\\"+decimalNumOnRight+\\\"|\\\"+decimalNumOnLeft+\\\")\\\" +\\n      \\\")[\\\"+exp_mark+\\\"]([+-]?[\\\"+digits+\\\"]+))$\\\");\\n  }\\n\\n  /* @stopify flat */ function digitsForRadix(radix, errbacks) {\\nreturn radix === 2  ? \\\"01\\\" :\\n       radix === 8  ? \\\"0-7\\\" :\\n       radix === 10 ? \\\"0-9\\\" :\\n       radix === 16 ? \\\"0-9a-fA-F\\\" :\\n       errbacks.throwInternalError(\\\"digitsForRadix: invalid radix\\\", this, radix)\\n  }\\n  /* @stopify flat */ function expMarkForRadix(radix, errbacks) {\\nreturn (radix === 2 || radix === 8 || radix === 10) ? \\\"defsl\\\" :\\n       (radix === 16)                               ? \\\"sl\\\" :\\n       errbacks.throwInternalError(\\\"expMarkForRadix: invalid radix\\\", this, radix)\\n  }\\n\\n  /* @stopify flat */ function Exactness(i) {\\n    this.defaultp = /* @stopify flat */ function () { return i == 0; }\\n    this.exactp = /* @stopify flat */ function () { return i == 1; }\\n    this.inexactp = /* @stopify flat */ function () { return i == 2; }\\n  }\\n\\n  Exactness.def = new Exactness(0);\\n  Exactness.on = new Exactness(1);\\n  Exactness.off = new Exactness(2);\\n\\n  Exactness.prototype.intAsExactp = /* @stopify flat */ function () { return this.defaultp() || this.exactp(); };\\n  Exactness.prototype.floatAsInexactp = /* @stopify flat */ function () { return this.defaultp() || this.inexactp(); };\\n\\n  // fromSchemeString: string boolean -> (scheme-number | false)\\n  var fromSchemeString = /* @stopify flat */ function(x, exactness, errbacks) {\\n\\nvar radix = 10\\nvar exactness = typeof exactness === 'undefined' ? Exactness.def :\\n    exactness === true               ? Exactness.on :\\n    exactness === false              ? Exactness.off :\\n   /* else */  errbacks.throwInternalError( \\\"exactness must be true or false\\\"\\n                                                 , this\\n                                                 , r) ;\\n\\nvar hMatch = x.toLowerCase().match(hashModifiersRegexp)\\nif (hMatch) {\\n    var modifierString = hMatch[1].toLowerCase();\\n\\n    var exactFlag = modifierString.match(new RegExp(\\\"(#[ei])\\\"))\\n    var radixFlag = modifierString.match(new RegExp(\\\"(#[bodx])\\\"))\\n\\n    if (exactFlag) {\\n  var f = exactFlag[1].charAt(1)\\n  exactness = f === 'e' ? Exactness.on :\\n        f === 'i' ? Exactness.off :\\n     // this case is unreachable\\n     errbacks.throwInternalError(\\\"invalid exactness flag\\\", this, r)\\n    }\\n    if (radixFlag) {\\n  var f = radixFlag[1].charAt(1)\\n  radix = f === 'b' ? 2 :\\n          f === 'o' ? 8 :\\n          f === 'd' ? 10 :\\n          f === 'x' ? 16 :\\n     // this case is unreachable\\n    errbacks.throwInternalError(\\\"invalid radix flag\\\", this, r)\\n    }\\n}\\n\\nvar numberString = hMatch ? hMatch[2] : x\\n// if the string begins with a hash modifier, then it must parse as a\\n// number, an invalid parse is an error, not false. False is returned\\n// when the item could potentially have been read as a symbol.\\nvar mustBeANumberp = hMatch ? true : false\\n\\nreturn fromSchemeStringRaw(numberString, radix, exactness, mustBeANumberp, errbacks)\\n  };\\n\\n  /* @stopify flat */ function fromSchemeStringRaw(x, radix, exactness, mustBeANumberp, errbacks) {\\nvar cMatch = matchComplexRegexp(radix, x, errbacks);\\nif (cMatch) {\\n        throw \\\"Complex Numbers are not supported in Pyret\\\";\\n}\\n\\n      return fromSchemeStringRawNoComplex(x, radix, exactness, mustBeANumberp, errbacks)\\n  }\\n\\n  /* @stopify flat */ function fromSchemeStringRawNoComplex(x, radix, exactness, mustBeANumberp, errbacks) {\\nvar aMatch = x.match(schemeRationalRegexp(digitsForRadix(radix, errbacks)));\\nif (aMatch) {\\n  return Rational.makeInstance( fromSchemeStringRawNoComplex( aMatch[1]\\n                                                                    , radix\\n                                                                    , exactness\\n                                                                    , errbacks\\n                                                                  )\\n                                      , fromSchemeStringRawNoComplex( aMatch[2]\\n                                                                      , radix\\n                                                                      , exactness\\n                                                                      , errbacks\\n                                                                    )\\n                                      , errbacks);\\n}\\n\\n      if (x === '+nan.0' ||\\n          x === '-nan.0' ||\\n          x === '+inf.0' ||\\n          x === '-inf.0' ||\\n          x === '-0.0') {\\n        return Roughnum.makeInstance(Infinity);\\n      }\\n\\nvar fMatch = x.match(schemeFlonumRegexp(digitsForRadix(radix, errbacks)))\\nif (fMatch) {\\n    var integralPart = fMatch[3] !== undefined ? fMatch[3] : fMatch[5];\\n    var fractionalPart = fMatch[4] !== undefined ? fMatch[4] : fMatch[6];\\n    return parseFloat( fMatch[1]\\n                             , integralPart\\n                             , fractionalPart\\n                             , radix\\n                             , exactness\\n                             , errbacks\\n                           )\\n}\\n\\nvar sMatch = x.match(schemeScientificPattern( digitsForRadix(radix, errbacks)\\n              , expMarkForRadix(radix, errbacks)\\n              ))\\nif (sMatch) {\\n    var coefficient = fromSchemeStringRawNoComplex(sMatch[1], radix, exactness, errbacks)\\n    var exponent = fromSchemeStringRawNoComplex(sMatch[2], radix, exactness, errbacks)\\n    return multiply(coefficient, expt(radix, exponent, errbacks), errbacks);\\n}\\n\\n// Finally, integer tests.\\nif (x.match(schemeDigitRegexp(digitsForRadix(radix, errbacks)))) {\\n    var n = parseInt(x, radix);\\n    if (isOverflow(n)) {\\n  return makeBignum(x);\\n    } else if (exactness.intAsExactp()) {\\n  return n;\\n    } else {\\n  return Roughnum.makeInstance(n)\\n    }\\n} else if (mustBeANumberp) {\\n    if(x.length===0) errbacks.throwGeneralError(\\\"no digits\\\");\\n    errbacks.throwGeneralError(\\\"bad number: \\\" + x, this);\\n} else {\\n    return false;\\n}\\n  };\\n\\n  /* @stopify flat */ function parseFloat(sign, integralPart, fractionalPart, radix, exactness, errbacks) {\\nvar sign = (sign == \\\"-\\\" ? -1 : 1);\\nvar integralPartValue = integralPart === \\\"\\\"  ? 0  :\\n      exactness.intAsExactp() ? parseExactInt(integralPart, radix, errbacks) :\\n              parseInt(integralPart, radix)\\n\\nvar fractionalNumerator = fractionalPart === \\\"\\\" ? 0 :\\n        exactness.intAsExactp() ? parseExactInt(fractionalPart, radix, errbacks) :\\n                parseInt(fractionalPart, radix)\\n/* unfortunately, for these next two calculations, `expt` and `divide` */\\n/* will promote to Bignum and Rational, respectively, but we only want */\\n/* these if we're parsing in exact mode */\\nvar fractionalDenominator = exactness.intAsExactp() ? expt(radix, fractionalPart.length, errbacks) :\\n                  Math.pow(radix, fractionalPart.length)\\nvar fractionalPartValue = fractionalPart === \\\"\\\" ? 0 :\\n        exactness.intAsExactp() ? divide(fractionalNumerator, fractionalDenominator, errbacks) :\\n                fractionalNumerator / fractionalDenominator\\n\\nvar forceInexact = /* @stopify flat */ function(o) {\\n    return typeof o === \\\"number\\\" ? Roughnum.makeInstance(o, errbacks) :\\n           o.toRoughnum(errbacks);\\n}\\n\\nreturn exactness.floatAsInexactp() ? forceInexact(multiply(sign, add( integralPartValue, fractionalPartValue))) :\\n             multiply(sign, add(integralPartValue, fractionalPartValue));\\n  }\\n\\n  /* @stopify flat */ function parseExactInt(str, radix, errbacks) {\\nreturn fromSchemeStringRawNoComplex(str, radix, Exactness.on, true, errbacks);\\n  }\\n\\n//////////////////////////////////////////////////////////////////////\\n//////////////////////////////////////////////////////////////////////\\n//////////////////////////////////////////////////////////////////////\\n//////////////////////////////////////////////////////////////////////\\n//////////////////////////////////////////////////////////////////////\\n\\n// The code below comes from Tom Wu's BigInteger implementation:\\n\\n// Copyright (c) 2005  Tom Wu\\n// All Rights Reserved.\\n// See \\\"LICENSE\\\" for details.\\n\\n// Basic JavaScript BN library - subset useful for RSA encryption.\\n\\n// Bits per digit\\nvar dbits;\\n\\n// JavaScript engine analysis\\nvar canary = 0xdeadbeefcafe;\\nvar j_lm = ((canary&0xffffff)==0xefcafe);\\n\\n// (public) Constructor\\n/* @stopify flat */ function BigInteger(a,b,c) {\\n  if(a != null)\\n    if(\\\"number\\\" == typeof a) this.fromNumber(a,b,c);\\n  else if(b == null && \\\"string\\\" != typeof a) this.fromString(a,256);\\n  else this.fromString(a,b);\\n}\\n\\n// return new, unset BigInteger\\n/* @stopify flat */ function nbi() { return new BigInteger(null); }\\n\\n// am: Compute w_j += (x*this_i), propagate carries,\\n// c is initial carry, returns final carry.\\n// c < 3*dvalue, x < 2*dvalue, this_i < dvalue\\n// We need to select the fastest one that works in this environment.\\n\\n// am1: use a single mult and divide to get the high bits,\\n// max digit bits should be 26 because\\n// max internal value = 2*dvalue^2-2*dvalue (< 2^53)\\n/* @stopify flat */ function am1(i,x,w,j,c,n) {\\n  while(--n >= 0) {\\n    var v = x*this[i++]+w[j]+c;\\n    c = Math.floor(v/0x4000000);\\n    w[j++] = v&0x3ffffff;\\n  }\\n  return c;\\n}\\n// am2 avoids a big mult-and-extract completely.\\n// Max digit bits should be <= 30 because we do bitwise ops\\n// on values up to 2*hdvalue^2-hdvalue-1 (< 2^31)\\n/* @stopify flat */ function am2(i,x,w,j,c,n) {\\n  var xl = x&0x7fff, xh = x>>15;\\n  while(--n >= 0) {\\n    var l = this[i]&0x7fff;\\n    var h = this[i++]>>15;\\n    var m = xh*l+h*xl;\\n    l = xl*l+((m&0x7fff)<<15)+w[j]+(c&0x3fffffff);\\n    c = (l>>>30)+(m>>>15)+xh*h+(c>>>30);\\n    w[j++] = l&0x3fffffff;\\n  }\\n  return c;\\n}\\n// Alternately, set max digit bits to 28 since some\\n// browsers slow down when dealing with 32-bit numbers.\\n/* @stopify flat */ function am3(i,x,w,j,c,n) {\\n  var xl = x&0x3fff, xh = x>>14;\\n  while(--n >= 0) {\\n    var l = this[i]&0x3fff;\\n    var h = this[i++]>>14;\\n    var m = xh*l+h*xl;\\n    l = xl*l+((m&0x3fff)<<14)+w[j]+c;\\n    c = (l>>28)+(m>>14)+xh*h;\\n    w[j++] = l&0xfffffff;\\n  }\\n  return c;\\n}\\nif(j_lm && (typeof(navigator) !== 'undefined' && navigator.appName == \\\"Microsoft Internet Explorer\\\")) {\\n  BigInteger.prototype.am = am2;\\n  dbits = 30;\\n}\\nelse if(j_lm && (typeof(navigator) !== 'undefined' && navigator.appName != \\\"Netscape\\\")) {\\n  BigInteger.prototype.am = am1;\\n  dbits = 26;\\n}\\nelse { // Mozilla/Netscape seems to prefer am3\\n  BigInteger.prototype.am = am3;\\n  dbits = 28;\\n}\\n\\nBigInteger.prototype.DB = dbits;\\nBigInteger.prototype.DM = ((1<<dbits)-1);\\nBigInteger.prototype.DV = (1<<dbits);\\n\\nvar BI_FP = 52;\\nBigInteger.prototype.FV = Math.pow(2,BI_FP);\\nBigInteger.prototype.F1 = BI_FP-dbits;\\nBigInteger.prototype.F2 = 2*dbits-BI_FP;\\n\\n// Digit conversions\\nvar BI_RM = \\\"0123456789abcdefghijklmnopqrstuvwxyz\\\";\\nvar BI_RC = [];\\nvar rr,vv;\\nrr = \\\"0\\\".charCodeAt(0);\\nfor(vv = 0; vv <= 9; ++vv) BI_RC[rr++] = vv;\\nrr = \\\"a\\\".charCodeAt(0);\\nfor(vv = 10; vv < 36; ++vv) BI_RC[rr++] = vv;\\nrr = \\\"A\\\".charCodeAt(0);\\nfor(vv = 10; vv < 36; ++vv) BI_RC[rr++] = vv;\\n\\n/* @stopify flat */ function int2char(n) { return BI_RM.charAt(n); }\\n/* @stopify flat */ function intAt(s,i) {\\n  var c = BI_RC[s.charCodeAt(i)];\\n  return (c==null)?-1:c;\\n}\\n\\n// (protected) copy this to r\\n/* @stopify flat */ function bnpCopyTo(r) {\\n  for(var i = this.t-1; i >= 0; --i) r[i] = this[i];\\n  r.t = this.t;\\n  r.s = this.s;\\n}\\n\\n// (protected) set from integer value x, -DV <= x < DV\\n/* @stopify flat */ function bnpFromInt(x) {\\n  this.t = 1;\\n  this.s = (x<0)?-1:0;\\n  if(x > 0) this[0] = x;\\n  else if(x < -1) this[0] = x+DV;\\n  else this.t = 0;\\n}\\n\\n// return bigint initialized to value\\n/* @stopify flat */ function nbv(i) { var r = nbi(); r.fromInt(i); return r; }\\n\\n// (protected) set from string and radix\\n/* @stopify flat */ function bnpFromString(s,b) {\\n  var k;\\n  if(b == 16) k = 4;\\n  else if(b == 8) k = 3;\\n  else if(b == 256) k = 8; // byte array\\n  else if(b == 2) k = 1;\\n  else if(b == 32) k = 5;\\n  else if(b == 4) k = 2;\\n  else { this.fromRadix(s,b); return; }\\n  this.t = 0;\\n  this.s = 0;\\n  var i = s.length, mi = false, sh = 0;\\n  while(--i >= 0) {\\n    var x = (k==8)?s[i]&0xff:intAt(s,i);\\n    if(x < 0) {\\n      if(s.charAt(i) == \\\"-\\\") mi = true;\\n      continue;\\n    }\\n    mi = false;\\n    if(sh == 0)\\n      this[this.t++] = x;\\n    else if(sh+k > this.DB) {\\n      this[this.t-1] |= (x&((1<<(this.DB-sh))-1))<<sh;\\n      this[this.t++] = (x>>(this.DB-sh));\\n    }\\n    else\\n      this[this.t-1] |= x<<sh;\\n    sh += k;\\n    if(sh >= this.DB) sh -= this.DB;\\n  }\\n  if(k == 8 && (s[0]&0x80) != 0) {\\n    this.s = -1;\\n    if(sh > 0) this[this.t-1] |= ((1<<(this.DB-sh))-1)<<sh;\\n  }\\n  this.clamp();\\n  if(mi) BigInteger.ZERO.subTo(this,this);\\n}\\n\\n// (protected) clamp off excess high words\\n/* @stopify flat */ function bnpClamp() {\\n  var c = this.s&this.DM;\\n  while(this.t > 0 && this[this.t-1] == c) --this.t;\\n}\\n\\n// (public) return string representation in given radix\\n/* @stopify flat */ function bnToString(b) {\\n  if(this.s < 0) return \\\"-\\\"+this.negate().toString(b);\\n  var k;\\n  if(b == 16) k = 4;\\n  else if(b == 8) k = 3;\\n  else if(b == 2) k = 1;\\n  else if(b == 32) k = 5;\\n  else if(b == 4) k = 2;\\n  else return this.toRadix(b);\\n  var km = (1<<k)-1, d, m = false, r = [], i = this.t;\\n  var p = this.DB-(i*this.DB)%k;\\n  if(i-- > 0) {\\n    if(p < this.DB && (d = this[i]>>p) > 0) { m = true; r.push(int2char(d)); }\\n    while(i >= 0) {\\n      if(p < k) {\\n        d = (this[i]&((1<<p)-1))<<(k-p);\\n        d |= this[--i]>>(p+=this.DB-k);\\n      }\\n      else {\\n        d = (this[i]>>(p-=k))&km;\\n        if(p <= 0) { p += this.DB; --i; }\\n      }\\n      if(d > 0) m = true;\\n      if(m) r.push(int2char(d));\\n    }\\n  }\\n  return m?r.join(\\\"\\\"):\\\"0\\\";\\n}\\n\\n// (public) -this\\n/* @stopify flat */ function bnNegate() { var r = nbi(); BigInteger.ZERO.subTo(this,r); return r; }\\n\\n// (public) |this|\\n/* @stopify flat */ function bnAbs() { return (this.s<0)?this.negate():this; }\\n\\n// (public) return + if this > a, - if this < a, 0 if equal\\n/* @stopify flat */ function bnCompareTo(a) {\\n  var r = this.s-a.s;\\n  if(r != 0) return r;\\n  var i = this.t;\\n  if ( this.s < 0 ) {\\n    r = a.t - i;\\n  }\\n  else {\\n    r = i - a.t;\\n  }\\n  if(r != 0) return r;\\n  while(--i >= 0) if((r=this[i]-a[i]) != 0) return r;\\n  return 0;\\n}\\n\\n// returns bit length of the integer x\\n/* @stopify flat */ function nbits(x) {\\n  var r = 1, t;\\n  if((t=x>>>16) != 0) { x = t; r += 16; }\\n  if((t=x>>8) != 0) { x = t; r += 8; }\\n  if((t=x>>4) != 0) { x = t; r += 4; }\\n  if((t=x>>2) != 0) { x = t; r += 2; }\\n  if((t=x>>1) != 0) { x = t; r += 1; }\\n  return r;\\n}\\n\\n// (public) return the number of bits in \\\"this\\\"\\n/* @stopify flat */ function bnBitLength() {\\n  if(this.t <= 0) return 0;\\n  return this.DB*(this.t-1)+nbits(this[this.t-1]^(this.s&this.DM));\\n}\\n\\n// (protected) r = this << n*DB\\n/* @stopify flat */ function bnpDLShiftTo(n,r) {\\n  var i;\\n  for(i = this.t-1; i >= 0; --i) r[i+n] = this[i];\\n  for(i = n-1; i >= 0; --i) r[i] = 0;\\n  r.t = this.t+n;\\n  r.s = this.s;\\n}\\n\\n// (protected) r = this >> n*DB\\n/* @stopify flat */ function bnpDRShiftTo(n,r) {\\n  for(var i = n; i < this.t; ++i) r[i-n] = this[i];\\n  r.t = Math.max(this.t-n,0);\\n  r.s = this.s;\\n}\\n\\n// (protected) r = this << n\\n/* @stopify flat */ function bnpLShiftTo(n,r) {\\n  var bs = n%this.DB;\\n  var cbs = this.DB-bs;\\n  var bm = (1<<cbs)-1;\\n  var ds = Math.floor(n/this.DB), c = (this.s<<bs)&this.DM, i;\\n  for(i = this.t-1; i >= 0; --i) {\\n    r[i+ds+1] = (this[i]>>cbs)|c;\\n    c = (this[i]&bm)<<bs;\\n  }\\n  for(i = ds-1; i >= 0; --i) r[i] = 0;\\n  r[ds] = c;\\n  r.t = this.t+ds+1;\\n  r.s = this.s;\\n  r.clamp();\\n}\\n\\n// (protected) r = this >> n\\n/* @stopify flat */ function bnpRShiftTo(n,r) {\\n  r.s = this.s;\\n  var ds = Math.floor(n/this.DB);\\n  if(ds >= this.t) { r.t = 0; return; }\\n  var bs = n%this.DB;\\n  var cbs = this.DB-bs;\\n  var bm = (1<<bs)-1;\\n  r[0] = this[ds]>>bs;\\n  for(var i = ds+1; i < this.t; ++i) {\\n    r[i-ds-1] |= (this[i]&bm)<<cbs;\\n    r[i-ds] = this[i]>>bs;\\n  }\\n  if(bs > 0) r[this.t-ds-1] |= (this.s&bm)<<cbs;\\n  r.t = this.t-ds;\\n  r.clamp();\\n}\\n\\n// (protected) r = this - a\\n/* @stopify flat */ function bnpSubTo(a,r) {\\n  var i = 0, c = 0, m = Math.min(a.t,this.t);\\n  while(i < m) {\\n    c += this[i]-a[i];\\n    r[i++] = c&this.DM;\\n    c >>= this.DB;\\n  }\\n  if(a.t < this.t) {\\n    c -= a.s;\\n    while(i < this.t) {\\n      c += this[i];\\n      r[i++] = c&this.DM;\\n      c >>= this.DB;\\n    }\\n    c += this.s;\\n  }\\n  else {\\n    c += this.s;\\n    while(i < a.t) {\\n      c -= a[i];\\n      r[i++] = c&this.DM;\\n      c >>= this.DB;\\n    }\\n    c -= a.s;\\n  }\\n  r.s = (c<0)?-1:0;\\n  if(c < -1) r[i++] = this.DV+c;\\n  else if(c > 0) r[i++] = c;\\n  r.t = i;\\n  r.clamp();\\n}\\n\\n// (protected) r = this * a, r != this,a (HAC 14.12)\\n// \\\"this\\\" should be the larger one if appropriate.\\n/* @stopify flat */ function bnpMultiplyTo(a,r) {\\n  var x = this.abs(), y = a.abs();\\n  var i = x.t;\\n  r.t = i+y.t;\\n  while(--i >= 0) r[i] = 0;\\n  for(i = 0; i < y.t; ++i) r[i+x.t] = x.am(0,y[i],r,i,0,x.t);\\n  r.s = 0;\\n  r.clamp();\\n  if(this.s != a.s) BigInteger.ZERO.subTo(r,r);\\n}\\n\\n// (protected) r = this^2, r != this (HAC 14.16)\\n/* @stopify flat */ function bnpSquareTo(r) {\\n  var x = this.abs();\\n  var i = r.t = 2*x.t;\\n  while(--i >= 0) r[i] = 0;\\n  for(i = 0; i < x.t-1; ++i) {\\n    var c = x.am(i,x[i],r,2*i,0,1);\\n    if((r[i+x.t]+=x.am(i+1,2*x[i],r,2*i+1,c,x.t-i-1)) >= x.DV) {\\n      r[i+x.t] -= x.DV;\\n      r[i+x.t+1] = 1;\\n    }\\n  }\\n  if(r.t > 0) r[r.t-1] += x.am(i,x[i],r,2*i,0,1);\\n  r.s = 0;\\n  r.clamp();\\n}\\n\\n// (protected) divide this by m, quotient and remainder to q, r (HAC 14.20)\\n// r != q, this != m.  q or r may be null.\\n/* @stopify flat */ function bnpDivRemTo(m,q,r) {\\n  var pm = m.abs();\\n  if(pm.t <= 0) return;\\n  var pt = this.abs();\\n  if(pt.t < pm.t) {\\n    if(q != null) q.fromInt(0);\\n    if(r != null) this.copyTo(r);\\n    return;\\n  }\\n  if(r == null) r = nbi();\\n  var y = nbi(), ts = this.s, ms = m.s;\\n  var nsh = this.DB-nbits(pm[pm.t-1]);    // normalize modulus\\n  if(nsh > 0) { pm.lShiftTo(nsh,y); pt.lShiftTo(nsh,r); }\\n  else { pm.copyTo(y); pt.copyTo(r); }\\n  var ys = y.t;\\n  var y0 = y[ys-1];\\n  if(y0 == 0) return;\\n  var yt = y0*(1<<this.F1)+((ys>1)?y[ys-2]>>this.F2:0);\\n  var d1 = this.FV/yt, d2 = (1<<this.F1)/yt, e = 1<<this.F2;\\n  var i = r.t, j = i-ys, t = (q==null)?nbi():q;\\n  y.dlShiftTo(j,t);\\n  if(r.compareTo(t) >= 0) {\\n    r[r.t++] = 1;\\n    r.subTo(t,r);\\n  }\\n  BigInteger.ONE.dlShiftTo(ys,t);\\n  t.subTo(y,y);   // \\\"negative\\\" y so we can replace sub with am later\\n  while(y.t < ys) y[y.t++] = 0;\\n  while(--j >= 0) {\\n    // Estimate quotient digit\\n    var qd = (r[--i]==y0)?this.DM:Math.floor(r[i]*d1+(r[i-1]+e)*d2);\\n    if((r[i]+=y.am(0,qd,r,j,0,ys)) < qd) {    // Try it out\\n      y.dlShiftTo(j,t);\\n      r.subTo(t,r);\\n      while(r[i] < --qd) r.subTo(t,r);\\n    }\\n  }\\n  if(q != null) {\\n    r.drShiftTo(ys,q);\\n    if(ts != ms) BigInteger.ZERO.subTo(q,q);\\n  }\\n  r.t = ys;\\n  r.clamp();\\n  if(nsh > 0) r.rShiftTo(nsh,r);  // Denormalize remainder\\n  if(ts < 0) BigInteger.ZERO.subTo(r,r);\\n}\\n\\n// (public) this mod a\\n/* @stopify flat */ function bnMod(a) {\\n  var r = nbi();\\n  this.abs().divRemTo(a,null,r);\\n  if(this.s < 0 && r.compareTo(BigInteger.ZERO) > 0) a.subTo(r,r);\\n  return r;\\n}\\n\\n// Modular reduction using \\\"classic\\\" algorithm\\n/* @stopify flat */ function Classic(m) { this.m = m; }\\n/* @stopify flat */ function cConvert(x) {\\n  if(x.s < 0 || x.compareTo(this.m) >= 0) return x.mod(this.m);\\n  else return x;\\n}\\n/* @stopify flat */ function cRevert(x) { return x; }\\n/* @stopify flat */ function cReduce(x) { x.divRemTo(this.m,null,x); }\\n/* @stopify flat */ function cMulTo(x,y,r) { x.multiplyTo(y,r); this.reduce(r); }\\n/* @stopify flat */ function cSqrTo(x,r) { x.squareTo(r); this.reduce(r); }\\n\\nClassic.prototype.convert = cConvert;\\nClassic.prototype.revert = cRevert;\\nClassic.prototype.reduce = cReduce;\\nClassic.prototype.mulTo = cMulTo;\\nClassic.prototype.sqrTo = cSqrTo;\\n\\n// (protected) return \\\"-1/this % 2^DB\\\"; useful for Mont. reduction\\n// justification:\\n//         xy == 1 (mod m)\\n//         xy =  1+km\\n//   xy(2-xy) = (1+km)(1-km)\\n// x[y(2-xy)] = 1-k^2m^2\\n// x[y(2-xy)] == 1 (mod m^2)\\n// if y is 1/x mod m, then y(2-xy) is 1/x mod m^2\\n// should reduce x and y(2-xy) by m^2 at each step to keep size bounded.\\n// JS multiply \\\"overflows\\\" differently from C/C++, so care is needed here.\\n/* @stopify flat */ function bnpInvDigit() {\\n  if(this.t < 1) return 0;\\n  var x = this[0];\\n  if((x&1) == 0) return 0;\\n  var y = x&3;        // y == 1/x mod 2^2\\n  y = (y*(2-(x&0xf)*y))&0xf;  // y == 1/x mod 2^4\\n  y = (y*(2-(x&0xff)*y))&0xff;    // y == 1/x mod 2^8\\n  y = (y*(2-(((x&0xffff)*y)&0xffff)))&0xffff; // y == 1/x mod 2^16\\n  // last step - calculate inverse mod DV directly;\\n  // assumes 16 < DB <= 32 and assumes ability to handle 48-bit ints\\n  y = (y*(2-x*y%this.DV))%this.DV;        // y == 1/x mod 2^dbits\\n  // we really want the negative inverse, and -DV < y < DV\\n  return (y>0)?this.DV-y:-y;\\n}\\n\\n// Montgomery reduction\\n/* @stopify flat */ function Montgomery(m) {\\n  this.m = m;\\n  this.mp = m.invDigit();\\n  this.mpl = this.mp&0x7fff;\\n  this.mph = this.mp>>15;\\n  this.um = (1<<(m.DB-15))-1;\\n  this.mt2 = 2*m.t;\\n}\\n\\n// xR mod m\\n/* @stopify flat */ function montConvert(x) {\\n  var r = nbi();\\n  x.abs().dlShiftTo(this.m.t,r);\\n  r.divRemTo(this.m,null,r);\\n  if(x.s < 0 && r.compareTo(BigInteger.ZERO) > 0) this.m.subTo(r,r);\\n  return r;\\n}\\n\\n// x/R mod m\\n/* @stopify flat */ function montRevert(x) {\\n  var r = nbi();\\n  x.copyTo(r);\\n  this.reduce(r);\\n  return r;\\n}\\n\\n// x = x/R mod m (HAC 14.32)\\n/* @stopify flat */ function montReduce(x) {\\n  while(x.t <= this.mt2)  // pad x so am has enough room later\\n    x[x.t++] = 0;\\n  for(var i = 0; i < this.m.t; ++i) {\\n    // faster way of calculating u0 = x[i]*mp mod DV\\n    var j = x[i]&0x7fff;\\n    var u0 = (j*this.mpl+(((j*this.mph+(x[i]>>15)*this.mpl)&this.um)<<15))&x.DM;\\n    // use am to combine the multiply-shift-add into one call\\n    j = i+this.m.t;\\n    x[j] += this.m.am(0,u0,x,i,0,this.m.t);\\n    // propagate carry\\n    while(x[j] >= x.DV) { x[j] -= x.DV; x[++j]++; }\\n  }\\n  x.clamp();\\n  x.drShiftTo(this.m.t,x);\\n  if(x.compareTo(this.m) >= 0) x.subTo(this.m,x);\\n}\\n\\n// r = \\\"x^2/R mod m\\\"; x != r\\n/* @stopify flat */ function montSqrTo(x,r) { x.squareTo(r); this.reduce(r); }\\n\\n// r = \\\"xy/R mod m\\\"; x,y != r\\n/* @stopify flat */ function montMulTo(x,y,r) { x.multiplyTo(y,r); this.reduce(r); }\\n\\nMontgomery.prototype.convert = montConvert;\\nMontgomery.prototype.revert = montRevert;\\nMontgomery.prototype.reduce = montReduce;\\nMontgomery.prototype.mulTo = montMulTo;\\nMontgomery.prototype.sqrTo = montSqrTo;\\n\\n// (protected) true iff this is even\\n/* @stopify flat */ function bnpIsEven() { return ((this.t>0)?(this[0]&1):this.s) == 0; }\\n\\n// (protected) this^e, e < 2^32, doing sqr and mul with \\\"r\\\" (HAC 14.79)\\n/* @stopify flat */ function bnpExp(e,z) {\\n  if(e > 0xffffffff || e < 1) return BigInteger.ONE;\\n  var r = nbi(), r2 = nbi(), g = z.convert(this), i = nbits(e)-1;\\n  g.copyTo(r);\\n  while(--i >= 0) {\\n    z.sqrTo(r,r2);\\n    if((e&(1<<i)) > 0) z.mulTo(r2,g,r);\\n    else { var t = r; r = r2; r2 = t; }\\n  }\\n  return z.revert(r);\\n}\\n\\n// (public) this^e % m, 0 <= e < 2^32\\n/* @stopify flat */ function bnModPowInt(e,m) {\\n  var z;\\n  if(e < 256 || m.isEven()) z = new Classic(m); else z = new Montgomery(m);\\n  return this.bnpExp(e,z);\\n}\\n\\n// protected\\nBigInteger.prototype.copyTo = bnpCopyTo;\\nBigInteger.prototype.fromInt = bnpFromInt;\\nBigInteger.prototype.fromString = bnpFromString;\\nBigInteger.prototype.clamp = bnpClamp;\\nBigInteger.prototype.dlShiftTo = bnpDLShiftTo;\\nBigInteger.prototype.drShiftTo = bnpDRShiftTo;\\nBigInteger.prototype.lShiftTo = bnpLShiftTo;\\nBigInteger.prototype.rShiftTo = bnpRShiftTo;\\nBigInteger.prototype.subTo = bnpSubTo;\\nBigInteger.prototype.multiplyTo = bnpMultiplyTo;\\nBigInteger.prototype.squareTo = bnpSquareTo;\\nBigInteger.prototype.divRemTo = bnpDivRemTo;\\nBigInteger.prototype.invDigit = bnpInvDigit;\\nBigInteger.prototype.isEven = bnpIsEven;\\nBigInteger.prototype.bnpExp = bnpExp; // renamed from exp, because we need the latter for Pyret\\n\\n// public\\nBigInteger.prototype.toString = bnToString;\\nBigInteger.prototype.negate = bnNegate;\\nBigInteger.prototype.abs = bnAbs;\\nBigInteger.prototype.compareTo = bnCompareTo;\\nBigInteger.prototype.bitLength = bnBitLength;\\nBigInteger.prototype.mod = bnMod;\\nBigInteger.prototype.modPowInt = bnModPowInt;\\n\\n// \\\"constants\\\"\\nBigInteger.ZERO = nbv(0);\\nBigInteger.ONE = nbv(1);\\n\\n// Copyright (c) 2005-2009  Tom Wu\\n// All Rights Reserved.\\n// See \\\"LICENSE\\\" for details.\\n\\n// Extended JavaScript BN /* @stopify flat */ functions, required for RSA private ops.\\n\\n// Version 1.1: new BigInteger(\\\"0\\\", 10) returns \\\"proper\\\" zero\\n\\n// (public)\\n/* @stopify flat */ function bnClone() { var r = nbi(); this.copyTo(r); return r; }\\n\\n// (public) return value as integer\\n/* @stopify flat */ function bnIntValue() {\\n  if(this.s < 0) {\\n    if(this.t == 1) return this[0]-this.DV;\\n    else if(this.t == 0) return -1;\\n  }\\n  else if(this.t == 1) return this[0];\\n  else if(this.t == 0) return 0;\\n  // assumes 16 < DB < 32\\n  return ((this[1]&((1<<(32-this.DB))-1))<<this.DB)|this[0];\\n}\\n\\n// (public) return value as byte\\n/* @stopify flat */ function bnByteValue() { return (this.t==0)?this.s:(this[0]<<24)>>24; }\\n\\n// (public) return value as short (assumes DB>=16)\\n/* @stopify flat */ function bnShortValue() { return (this.t==0)?this.s:(this[0]<<16)>>16; }\\n\\n// (protected) return x s.t. r^x < DV\\n/* @stopify flat */ function bnpChunkSize(r) { return Math.floor(Math.LN2*this.DB/Math.log(r)); }\\n\\n// (public) 0 if this == 0, 1 if this > 0\\n/* @stopify flat */ function bnSigNum() {\\n  if(this.s < 0) return -1;\\n  else if(this.t <= 0 || (this.t == 1 && this[0] <= 0)) return 0;\\n  else return 1;\\n}\\n\\n// (protected) convert to radix string\\n/* @stopify flat */ function bnpToRadix(b) {\\n  if(b == null) b = 10;\\n  if(this.signum() == 0 || b < 2 || b > 36) return \\\"0\\\";\\n  var cs = this.chunkSize(b);\\n  var a = Math.pow(b,cs);\\n  var d = nbv(a), y = nbi(), z = nbi(), r = \\\"\\\";\\n  this.divRemTo(d,y,z);\\n  while(y.signum() > 0) {\\n    r = (a+z.intValue()).toString(b).substr(1) + r;\\n    y.divRemTo(d,y,z);\\n  }\\n  return z.intValue().toString(b) + r;\\n}\\n\\n// (protected) convert from radix string\\n/* @stopify flat */ function bnpFromRadix(s,b) {\\n  this.fromInt(0);\\n  if(b == null) b = 10;\\n  var cs = this.chunkSize(b);\\n  var d = Math.pow(b,cs), mi = false, j = 0, w = 0;\\n  for(var i = 0; i < s.length; ++i) {\\n    var x = intAt(s,i);\\n    if(x < 0) {\\n      if(s.charAt(i) == \\\"-\\\" && this.signum() == 0) mi = true;\\n      continue;\\n    }\\n    w = b*w+x;\\n    if(++j >= cs) {\\n      this.dMultiply(d);\\n      this.dAddOffset(w,0);\\n      j = 0;\\n      w = 0;\\n    }\\n  }\\n  if(j > 0) {\\n    this.dMultiply(Math.pow(b,j));\\n    this.dAddOffset(w,0);\\n  }\\n  if(mi) BigInteger.ZERO.subTo(this,this);\\n}\\n\\n// (protected) alternate constructor\\n/* @stopify flat */ function bnpFromNumber(a,b,c) {\\n  if(\\\"number\\\" == typeof b) {\\n    // new BigInteger(int,int,RNG)\\n    if(a < 2) this.fromInt(1);\\n    else {\\n      this.fromNumber(a,c);\\n      if(!this.testBit(a-1))  // force MSB set\\n        this.bitwiseTo(BigInteger.ONE.shiftLeft(a-1),op_or,this);\\n      if(this.isEven()) this.dAddOffset(1,0); // force odd\\n      while(!this.isProbablePrime(b)) {\\n        this.dAddOffset(2,0);\\n        if(this.bitLength() > a) this.subTo(BigInteger.ONE.shiftLeft(a-1),this);\\n      }\\n    }\\n  }\\n  else {\\n    // new BigInteger(int,RNG)\\n    var x = [], t = a&7;\\n    x.length = (a>>3)+1;\\n    b.nextBytes(x);\\n    if(t > 0) x[0] &= ((1<<t)-1); else x[0] = 0;\\n    this.fromString(x,256);\\n  }\\n}\\n\\n// (public) convert to bigendian byte array\\n/* @stopify flat */ function bnToByteArray() {\\n  var i = this.t, r = [];\\n  r[0] = this.s;\\n  var p = this.DB-(i*this.DB)%8, d, k = 0;\\n  if(i-- > 0) {\\n    if(p < this.DB && (d = this[i]>>p) != (this.s&this.DM)>>p)\\n      r[k++] = d|(this.s<<(this.DB-p));\\n    while(i >= 0) {\\n      if(p < 8) {\\n        d = (this[i]&((1<<p)-1))<<(8-p);\\n        d |= this[--i]>>(p+=this.DB-8);\\n      }\\n      else {\\n        d = (this[i]>>(p-=8))&0xff;\\n        if(p <= 0) { p += this.DB; --i; }\\n      }\\n      if((d&0x80) != 0) d |= -256;\\n      if(k == 0 && (this.s&0x80) != (d&0x80)) ++k;\\n      if(k > 0 || d != this.s) r[k++] = d;\\n    }\\n  }\\n  return r;\\n}\\n\\n/* @stopify flat */ function bnEquals(a) { return(this.compareTo(a)==0); }\\n/* @stopify flat */ function bnMin(a) { return(this.compareTo(a)<0)?this:a; }\\n/* @stopify flat */ function bnMax(a) { return(this.compareTo(a)>0)?this:a; }\\n\\n// (protected) r = this op a (bitwise)\\n/* @stopify flat */ function bnpBitwiseTo(a,op,r) {\\n  var i, f, m = Math.min(a.t,this.t);\\n  for(i = 0; i < m; ++i) r[i] = op(this[i],a[i]);\\n  if(a.t < this.t) {\\n    f = a.s&this.DM;\\n    for(i = m; i < this.t; ++i) r[i] = op(this[i],f);\\n    r.t = this.t;\\n  }\\n  else {\\n    f = this.s&this.DM;\\n    for(i = m; i < a.t; ++i) r[i] = op(f,a[i]);\\n    r.t = a.t;\\n  }\\n  r.s = op(this.s,a.s);\\n  r.clamp();\\n}\\n\\n// (public) this & a\\n/* @stopify flat */ function op_and(x,y) { return x&y; }\\n/* @stopify flat */ function bnAnd(a) { var r = nbi(); this.bitwiseTo(a,op_and,r); return r; }\\n\\n// (public) this | a\\n/* @stopify flat */ function op_or(x,y) { return x|y; }\\n/* @stopify flat */ function bnOr(a) { var r = nbi(); this.bitwiseTo(a,op_or,r); return r; }\\n\\n// (public) this ^ a\\n/* @stopify flat */ function op_xor(x,y) { return x^y; }\\n/* @stopify flat */ function bnXor(a) { var r = nbi(); this.bitwiseTo(a,op_xor,r); return r; }\\n\\n// (public) this & ~a\\n/* @stopify flat */ function op_andnot(x,y) { return x&~y; }\\n/* @stopify flat */ function bnAndNot(a) { var r = nbi(); this.bitwiseTo(a,op_andnot,r); return r; }\\n\\n// (public) ~this\\n/* @stopify flat */ function bnNot() {\\n  var r = nbi();\\n  for(var i = 0; i < this.t; ++i) r[i] = this.DM&~this[i];\\n  r.t = this.t;\\n  r.s = ~this.s;\\n  return r;\\n}\\n\\n// (public) this << n\\n/* @stopify flat */ function bnShiftLeft(n) {\\n  var r = nbi();\\n  if(n < 0) this.rShiftTo(-n,r); else this.lShiftTo(n,r);\\n  return r;\\n}\\n\\n// (public) this >> n\\n/* @stopify flat */ function bnShiftRight(n) {\\n  var r = nbi();\\n  if(n < 0) this.lShiftTo(-n,r); else this.rShiftTo(n,r);\\n  return r;\\n}\\n\\n// return index of lowest 1-bit in x, x < 2^31\\n/* @stopify flat */ function lbit(x) {\\n  if(x == 0) return -1;\\n  var r = 0;\\n  if((x&0xffff) == 0) { x >>= 16; r += 16; }\\n  if((x&0xff) == 0) { x >>= 8; r += 8; }\\n  if((x&0xf) == 0) { x >>= 4; r += 4; }\\n  if((x&3) == 0) { x >>= 2; r += 2; }\\n  if((x&1) == 0) ++r;\\n  return r;\\n}\\n\\n// (public) returns index of lowest 1-bit (or -1 if none)\\n/* @stopify flat */ function bnGetLowestSetBit() {\\n  for(var i = 0; i < this.t; ++i)\\n    if(this[i] != 0) return i*this.DB+lbit(this[i]);\\n  if(this.s < 0) return this.t*this.DB;\\n  return -1;\\n}\\n\\n// return number of 1 bits in x\\n/* @stopify flat */ function cbit(x) {\\n  var r = 0;\\n  while(x != 0) { x &= x-1; ++r; }\\n  return r;\\n}\\n\\n// (public) return number of set bits\\n/* @stopify flat */ function bnBitCount() {\\n  var r = 0, x = this.s&this.DM;\\n  for(var i = 0; i < this.t; ++i) r += cbit(this[i]^x);\\n  return r;\\n}\\n\\n// (public) true iff nth bit is set\\n/* @stopify flat */ function bnTestBit(n) {\\n  var j = Math.floor(n/this.DB);\\n  if(j >= this.t) return(this.s!=0);\\n  return((this[j]&(1<<(n%this.DB)))!=0);\\n}\\n\\n// (protected) this op (1<<n)\\n/* @stopify flat */ function bnpChangeBit(n,op) {\\n  var r = BigInteger.ONE.shiftLeft(n);\\n  this.bitwiseTo(r,op,r);\\n  return r;\\n}\\n\\n// (public) this | (1<<n)\\n/* @stopify flat */ function bnSetBit(n) { return this.changeBit(n,op_or); }\\n\\n// (public) this & ~(1<<n)\\n/* @stopify flat */ function bnClearBit(n) { return this.changeBit(n,op_andnot); }\\n\\n// (public) this ^ (1<<n)\\n/* @stopify flat */ function bnFlipBit(n) { return this.changeBit(n,op_xor); }\\n\\n// (protected) r = this + a\\n/* @stopify flat */ function bnpAddTo(a,r) {\\n  var i = 0, c = 0, m = Math.min(a.t,this.t);\\n  while(i < m) {\\n    c += this[i]+a[i];\\n    r[i++] = c&this.DM;\\n    c >>= this.DB;\\n  }\\n  if(a.t < this.t) {\\n    c += a.s;\\n    while(i < this.t) {\\n      c += this[i];\\n      r[i++] = c&this.DM;\\n      c >>= this.DB;\\n    }\\n    c += this.s;\\n  }\\n  else {\\n    c += this.s;\\n    while(i < a.t) {\\n      c += a[i];\\n      r[i++] = c&this.DM;\\n      c >>= this.DB;\\n    }\\n    c += a.s;\\n  }\\n  r.s = (c<0)?-1:0;\\n  if(c > 0) r[i++] = c;\\n  else if(c < -1) r[i++] = this.DV+c;\\n  r.t = i;\\n  r.clamp();\\n}\\n\\n// (public) this + a\\n/* @stopify flat */ function bnAdd(a) { var r = nbi(); this.addTo(a,r); return r; }\\n\\n// (public) this - a\\n/* @stopify flat */ function bnSubtract(a) { var r = nbi(); this.subTo(a,r); return r; }\\n\\n// (public) this * a\\n/* @stopify flat */ function bnMultiply(a) { var r = nbi(); this.multiplyTo(a,r); return r; }\\n\\n// (public) this / a\\n/* @stopify flat */ function bnDivide(a) { var r = nbi(); this.divRemTo(a,r,null); return r; }\\n\\n// (public) this % a\\n/* @stopify flat */ function bnRemainder(a) { var r = nbi(); this.divRemTo(a,null,r); return r; }\\n\\n// (public) [this/a,this%a]\\n/* @stopify flat */ function bnDivideAndRemainder(a) {\\n  var q = nbi(), r = nbi();\\n  this.divRemTo(a,q,r);\\n  return [q,r];\\n}\\n\\n// (protected) this *= n, this >= 0, 1 < n < DV\\n/* @stopify flat */ function bnpDMultiply(n) {\\n  this[this.t] = this.am(0,n-1,this,0,0,this.t);\\n  ++this.t;\\n  this.clamp();\\n}\\n\\n// (protected) this += n << w words, this >= 0\\n/* @stopify flat */ function bnpDAddOffset(n,w) {\\n  if(n == 0) return;\\n  while(this.t <= w) this[this.t++] = 0;\\n  this[w] += n;\\n  while(this[w] >= this.DV) {\\n    this[w] -= this.DV;\\n    if(++w >= this.t) this[this.t++] = 0;\\n    ++this[w];\\n  }\\n}\\n\\n// A \\\"null\\\" reducer\\n/* @stopify flat */ function NullExp() {}\\n/* @stopify flat */ function nNop(x) { return x; }\\n/* @stopify flat */ function nMulTo(x,y,r) { x.multiplyTo(y,r); }\\n/* @stopify flat */ function nSqrTo(x,r) { x.squareTo(r); }\\n\\nNullExp.prototype.convert = nNop;\\nNullExp.prototype.revert = nNop;\\nNullExp.prototype.mulTo = nMulTo;\\nNullExp.prototype.sqrTo = nSqrTo;\\n\\n// (public) this^e\\n/* @stopify flat */ function bnPow(e) { return this.bnpExp(e,new NullExp()); }\\n\\n// (protected) r = lower n words of \\\"this * a\\\", a.t <= n\\n// \\\"this\\\" should be the larger one if appropriate.\\n/* @stopify flat */ function bnpMultiplyLowerTo(a,n,r) {\\n  var i = Math.min(this.t+a.t,n);\\n  r.s = 0; // assumes a,this >= 0\\n  r.t = i;\\n  while(i > 0) r[--i] = 0;\\n  var j;\\n  for(j = r.t-this.t; i < j; ++i) r[i+this.t] = this.am(0,a[i],r,i,0,this.t);\\n  for(j = Math.min(a.t,n); i < j; ++i) this.am(0,a[i],r,i,0,n-i);\\n  r.clamp();\\n}\\n\\n// (protected) r = \\\"this * a\\\" without lower n words, n > 0\\n// \\\"this\\\" should be the larger one if appropriate.\\n/* @stopify flat */ function bnpMultiplyUpperTo(a,n,r) {\\n  --n;\\n  var i = r.t = this.t+a.t-n;\\n  r.s = 0; // assumes a,this >= 0\\n  while(--i >= 0) r[i] = 0;\\n  for(i = Math.max(n-this.t,0); i < a.t; ++i)\\n    r[this.t+i-n] = this.am(n-i,a[i],r,0,0,this.t+i-n);\\n  r.clamp();\\n  r.drShiftTo(1,r);\\n}\\n\\n// Barrett modular reduction\\n/* @stopify flat */ function Barrett(m) {\\n  // setup Barrett\\n  this.r2 = nbi();\\n  this.q3 = nbi();\\n  BigInteger.ONE.dlShiftTo(2*m.t,this.r2);\\n  this.mu = this.r2.divide(m);\\n  this.m = m;\\n}\\n\\n/* @stopify flat */ function barrettConvert(x) {\\n  if(x.s < 0 || x.t > 2*this.m.t) return x.mod(this.m);\\n  else if(x.compareTo(this.m) < 0) return x;\\n  else { var r = nbi(); x.copyTo(r); this.reduce(r); return r; }\\n}\\n\\n/* @stopify flat */ function barrettRevert(x) { return x; }\\n\\n// x = x mod m (HAC 14.42)\\n/* @stopify flat */ function barrettReduce(x) {\\n  x.drShiftTo(this.m.t-1,this.r2);\\n  if(x.t > this.m.t+1) { x.t = this.m.t+1; x.clamp(); }\\n  this.mu.multiplyUpperTo(this.r2,this.m.t+1,this.q3);\\n  this.m.multiplyLowerTo(this.q3,this.m.t+1,this.r2);\\n  while(x.compareTo(this.r2) < 0) x.dAddOffset(1,this.m.t+1);\\n  x.subTo(this.r2,x);\\n  while(x.compareTo(this.m) >= 0) x.subTo(this.m,x);\\n}\\n\\n// r = x^2 mod m; x != r\\n/* @stopify flat */ function barrettSqrTo(x,r) { x.squareTo(r); this.reduce(r); }\\n\\n// r = x*y mod m; x,y != r\\n/* @stopify flat */ function barrettMulTo(x,y,r) { x.multiplyTo(y,r); this.reduce(r); }\\n\\nBarrett.prototype.convert = barrettConvert;\\nBarrett.prototype.revert = barrettRevert;\\nBarrett.prototype.reduce = barrettReduce;\\nBarrett.prototype.mulTo = barrettMulTo;\\nBarrett.prototype.sqrTo = barrettSqrTo;\\n\\n// (public) this^e % m (HAC 14.85)\\n/* @stopify flat */ function bnModPow(e,m) {\\n  var i = e.bitLength(), k, r = nbv(1), z;\\n  if(i <= 0) return r;\\n  else if(i < 18) k = 1;\\n  else if(i < 48) k = 3;\\n  else if(i < 144) k = 4;\\n  else if(i < 768) k = 5;\\n  else k = 6;\\n  if(i < 8)\\n    z = new Classic(m);\\n  else if(m.isEven())\\n    z = new Barrett(m);\\n  else\\n    z = new Montgomery(m);\\n\\n  // precomputation\\n  var g = [], n = 3, k1 = k-1, km = (1<<k)-1;\\n  g[1] = z.convert(this);\\n  if(k > 1) {\\n    var g2 = nbi();\\n    z.sqrTo(g[1],g2);\\n    while(n <= km) {\\n      g[n] = nbi();\\n      z.mulTo(g2,g[n-2],g[n]);\\n      n += 2;\\n    }\\n  }\\n\\n  var j = e.t-1, w, is1 = true, r2 = nbi(), t;\\n  i = nbits(e[j])-1;\\n  while(j >= 0) {\\n    if(i >= k1) w = (e[j]>>(i-k1))&km;\\n    else {\\n      w = (e[j]&((1<<(i+1))-1))<<(k1-i);\\n      if(j > 0) w |= e[j-1]>>(this.DB+i-k1);\\n    }\\n\\n    n = k;\\n    while((w&1) == 0) { w >>= 1; --n; }\\n    if((i -= n) < 0) { i += this.DB; --j; }\\n    if(is1) { // ret == 1, don't bother squaring or multiplying it\\n      g[w].copyTo(r);\\n      is1 = false;\\n    }\\n    else {\\n      while(n > 1) { z.sqrTo(r,r2); z.sqrTo(r2,r); n -= 2; }\\n      if(n > 0) z.sqrTo(r,r2); else { t = r; r = r2; r2 = t; }\\n      z.mulTo(r2,g[w],r);\\n    }\\n\\n    while(j >= 0 && (e[j]&(1<<i)) == 0) {\\n      z.sqrTo(r,r2); t = r; r = r2; r2 = t;\\n      if(--i < 0) { i = this.DB-1; --j; }\\n    }\\n  }\\n  return z.revert(r);\\n}\\n\\n// (public) gcd(this,a) (HAC 14.54)\\n/* @stopify flat */ function bnGCD(a) {\\n  var x = (this.s<0)?this.negate():this.clone();\\n  var y = (a.s<0)?a.negate():a.clone();\\n  if(x.compareTo(y) < 0) { var t = x; x = y; y = t; }\\n  var i = x.getLowestSetBit(), g = y.getLowestSetBit();\\n  if(g < 0) return x;\\n  if(i < g) g = i;\\n  if(g > 0) {\\n    x.rShiftTo(g,x);\\n    y.rShiftTo(g,y);\\n  }\\n  while(x.signum() > 0) {\\n    if((i = x.getLowestSetBit()) > 0) x.rShiftTo(i,x);\\n    if((i = y.getLowestSetBit()) > 0) y.rShiftTo(i,y);\\n    if(x.compareTo(y) >= 0) {\\n      x.subTo(y,x);\\n      x.rShiftTo(1,x);\\n    }\\n    else {\\n      y.subTo(x,y);\\n      y.rShiftTo(1,y);\\n    }\\n  }\\n  if(g > 0) y.lShiftTo(g,y);\\n  return y;\\n}\\n\\n// (protected) this % n, n < 2^26\\n/* @stopify flat */ function bnpModInt(n) {\\n  if(n <= 0) return 0;\\n  var d = this.DV%n, r = (this.s<0)?n-1:0;\\n  if(this.t > 0)\\n    if(d == 0) r = this[0]%n;\\n  else for(var i = this.t-1; i >= 0; --i) r = (d*r+this[i])%n;\\n  return r;\\n}\\n\\n// (public) 1/this % m (HAC 14.61)\\n/* @stopify flat */ function bnModInverse(m) {\\n  var ac = m.isEven();\\n  if((this.isEven() && ac) || m.signum() == 0) return BigInteger.ZERO;\\n  var u = m.clone(), v = this.clone();\\n  var a = nbv(1), b = nbv(0), c = nbv(0), d = nbv(1);\\n  while(u.signum() != 0) {\\n    while(u.isEven()) {\\n      u.rShiftTo(1,u);\\n      if(ac) {\\n        if(!a.isEven() || !b.isEven()) { a.addTo(this,a); b.subTo(m,b); }\\n        a.rShiftTo(1,a);\\n      }\\n      else if(!b.isEven()) b.subTo(m,b);\\n      b.rShiftTo(1,b);\\n    }\\n    while(v.isEven()) {\\n      v.rShiftTo(1,v);\\n      if(ac) {\\n        if(!c.isEven() || !d.isEven()) { c.addTo(this,c); d.subTo(m,d); }\\n        c.rShiftTo(1,c);\\n      }\\n      else if(!d.isEven()) d.subTo(m,d);\\n      d.rShiftTo(1,d);\\n    }\\n    if(u.compareTo(v) >= 0) {\\n      u.subTo(v,u);\\n      if(ac) a.subTo(c,a);\\n      b.subTo(d,b);\\n    }\\n    else {\\n      v.subTo(u,v);\\n      if(ac) c.subTo(a,c);\\n      d.subTo(b,d);\\n    }\\n  }\\n  if(v.compareTo(BigInteger.ONE) != 0) return BigInteger.ZERO;\\n  if(d.compareTo(m) >= 0) return d.subtract(m);\\n  if(d.signum() < 0) d.addTo(m,d); else return d;\\n  if(d.signum() < 0) return d.add(m); else return d;\\n}\\n\\nvar lowprimes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509];\\nvar lplim = (1<<26)/lowprimes[lowprimes.length-1];\\n\\n// (public) test primality with certainty >= 1-.5^t\\n/* @stopify flat */ function bnIsProbablePrime(t) {\\n  var i, x = this.abs();\\n  if(x.t == 1 && x[0] <= lowprimes[lowprimes.length-1]) {\\n    for(i = 0; i < lowprimes.length; ++i)\\n      if(x[0] == lowprimes[i]) return true;\\n    return false;\\n  }\\n  if(x.isEven()) return false;\\n  i = 1;\\n  while(i < lowprimes.length) {\\n    var m = lowprimes[i], j = i+1;\\n    while(j < lowprimes.length && m < lplim) m *= lowprimes[j++];\\n    m = x.modInt(m);\\n    while(i < j) if(m%lowprimes[i++] == 0) return false;\\n  }\\n  return x.millerRabin(t);\\n}\\n\\n// (protected) true if probably prime (HAC 4.24, Miller-Rabin)\\n/* @stopify flat */ function bnpMillerRabin(t) {\\n  var n1 = this.subtract(BigInteger.ONE);\\n  var k = n1.getLowestSetBit();\\n  if(k <= 0) return false;\\n  var r = n1.shiftRight(k);\\n  t = (t+1)>>1;\\n  if(t > lowprimes.length) t = lowprimes.length;\\n  var a = nbi();\\n  for(var i = 0; i < t; ++i) {\\n    a.fromInt(lowprimes[i]);\\n    var y = a.modPow(r,this);\\n    if(y.compareTo(BigInteger.ONE) != 0 && y.compareTo(n1) != 0) {\\n      var j = 1;\\n      while(j++ < k && y.compareTo(n1) != 0) {\\n        y = y.modPowInt(2,this);\\n        if(y.compareTo(BigInteger.ONE) == 0) return false;\\n      }\\n      if(y.compareTo(n1) != 0) return false;\\n    }\\n  }\\n  return true;\\n}\\n\\n// protected\\nBigInteger.prototype.chunkSize = bnpChunkSize;\\nBigInteger.prototype.toRadix = bnpToRadix;\\nBigInteger.prototype.fromRadix = bnpFromRadix;\\nBigInteger.prototype.fromNumber = bnpFromNumber;\\nBigInteger.prototype.bitwiseTo = bnpBitwiseTo;\\nBigInteger.prototype.changeBit = bnpChangeBit;\\nBigInteger.prototype.addTo = bnpAddTo;\\nBigInteger.prototype.dMultiply = bnpDMultiply;\\nBigInteger.prototype.dAddOffset = bnpDAddOffset;\\nBigInteger.prototype.multiplyLowerTo = bnpMultiplyLowerTo;\\nBigInteger.prototype.multiplyUpperTo = bnpMultiplyUpperTo;\\nBigInteger.prototype.modInt = bnpModInt;\\nBigInteger.prototype.millerRabin = bnpMillerRabin;\\n\\n// public\\nBigInteger.prototype.clone = bnClone;\\nBigInteger.prototype.intValue = bnIntValue;\\nBigInteger.prototype.byteValue = bnByteValue;\\nBigInteger.prototype.shortValue = bnShortValue;\\nBigInteger.prototype.signum = bnSigNum;\\nBigInteger.prototype.toByteArray = bnToByteArray;\\nBigInteger.prototype.equals = bnEquals;\\nBigInteger.prototype.min = bnMin;\\nBigInteger.prototype.max = bnMax;\\nBigInteger.prototype.and = bnAnd;\\nBigInteger.prototype.or = bnOr;\\nBigInteger.prototype.xor = bnXor;\\nBigInteger.prototype.andNot = bnAndNot;\\nBigInteger.prototype.not = bnNot;\\nBigInteger.prototype.shiftLeft = bnShiftLeft;\\nBigInteger.prototype.shiftRight = bnShiftRight;\\nBigInteger.prototype.getLowestSetBit = bnGetLowestSetBit;\\nBigInteger.prototype.bitCount = bnBitCount;\\nBigInteger.prototype.testBit = bnTestBit;\\nBigInteger.prototype.setBit = bnSetBit;\\nBigInteger.prototype.clearBit = bnClearBit;\\nBigInteger.prototype.flipBit = bnFlipBit;\\nBigInteger.prototype.add = bnAdd;\\nBigInteger.prototype.subtract = bnSubtract;\\nBigInteger.prototype.multiply = bnMultiply;\\nBigInteger.prototype.divide = bnDivide;\\nBigInteger.prototype.remainder = bnRemainder;\\nBigInteger.prototype.divideAndRemainder = bnDivideAndRemainder;\\nBigInteger.prototype.modPow = bnModPow;\\nBigInteger.prototype.modInverse = bnModInverse;\\nBigInteger.prototype.pow = bnPow;\\nBigInteger.prototype.gcd = bnGCD;\\nBigInteger.prototype.isProbablePrime = bnIsProbablePrime;\\n\\n// BigInteger interfaces not implemented in jsbn:\\n\\n// BigInteger(int signum, byte[] magnitude)\\n// double doubleValue()\\n// float floatValue()\\n// int hashCode()\\n// long longValue()\\n// static BigInteger valueOf(long val)\\n\\n//////////////////////////////////////////////////////////////////////\\n//////////////////////////////////////////////////////////////////////\\n//////////////////////////////////////////////////////////////////////\\n//////////////////////////////////////////////////////////////////////\\n//////////////////////////////////////////////////////////////////////\\n// END OF copy-and-paste of jsbn.\\n\\nBigInteger.NEGATIVE_ONE = BigInteger.ONE.negate();\\n\\n// Other methods we need to add for compatibilty with js-numbers numeric tower.\\n\\n// add is implemented above.\\n// subtract is implemented above.\\n// multiply is implemented above.\\n// equals is implemented above.\\n// abs is implemented above.\\n// negate is defined above.\\n\\n// makeBignum: string -> BigInteger\\nvar makeBignum = /* @stopify flat */ function(s) {\\n  if (typeof(s) === 'number') { s = s + ''; }\\n  s = expandExponent(s);\\n  return new BigInteger(s, 10);\\n};\\n\\nvar zerostring = /* @stopify flat */ function(n) {\\n  var buf = [];\\n  for (var i = 0; i < n; i++) {\\n    buf.push('0');\\n  }\\n  return buf.join('');\\n};\\n\\nBigInteger.prototype.isFinite = /* @stopify flat */ function() {\\n  return true;\\n};\\n\\nBigInteger.prototype.isInteger = /* @stopify flat */ function() {\\n  return true;\\n};\\n\\nBigInteger.prototype.isRational = /* @stopify flat */ function() {\\n  return true;\\n};\\n\\nBigInteger.prototype.isExact = BigInteger.prototype.isRational;\\n\\nBigInteger.prototype.isReal = /* @stopify flat */ function() {\\n  return true;\\n};\\n\\nBigInteger.prototype.isRoughnum = /* @stopify flat */ function() {\\n  return false;\\n};\\n\\nBigInteger.prototype.isPositive = /* @stopify flat */ function() {\\n  return this.compareTo(BigInteger.ZERO) > 0;\\n};\\n\\nBigInteger.prototype.isNonNegative = /* @stopify flat */ function() {\\n  return this.compareTo(BigInteger.ZERO) >= 0;\\n};\\n\\nBigInteger.prototype.isNegative = /* @stopify flat */ function() {\\n  return this.compareTo(BigInteger.ZERO) < 0;\\n};\\n\\nBigInteger.prototype.isNonPositive = /* @stopify flat */ function() {\\n  return this.compareTo(BigInteger.ZERO) <= 0;\\n};\\n\\nBigInteger.prototype.toRational = /* @stopify flat */ function() {\\n  return this;\\n};\\n\\nBigInteger.prototype.toExact = BigInteger.prototype.toRational;\\n\\nBigInteger.prototype.toFixnum = /* @stopify flat */ function() {\\n  var a = splitIntIntoMantissaExpt(this);\\n  //console.log('bigint.tofixnum of', this);\\n  //console.log('split = ', a);\\n  var r = Number(String(a[0]) + 'e' + String(a[1]));\\n  //console.log('returning', r);\\n  return r;\\n}\\n\\nBigInteger.prototype.toRoughnum = /* @stopify flat */ function(errbacks) {\\n  return Roughnum.makeInstance(this.toFixnum(), errbacks);\\n};\\n\\nBigInteger.prototype.greaterThan = /* @stopify flat */ function(other, errbacks) {\\n  return this.compareTo(other, errbacks) > 0;\\n};\\n\\nBigInteger.prototype.greaterThanOrEqual = /* @stopify flat */ function(other, errbacks) {\\n  return this.compareTo(other, errbacks) >= 0;\\n};\\n\\nBigInteger.prototype.lessThan = /* @stopify flat */ function(other, errbacks) {\\n  return this.compareTo(other, errbacks) < 0;\\n};\\n\\nBigInteger.prototype.lessThanOrEqual = /* @stopify flat */ function(other, errbacks) {\\n  return this.compareTo(other, errbacks) <= 0;\\n};\\n\\n// divide: pyretnum -> pyretnum\\n// WARNING NOTE: we override the old version of divide.\\nBigInteger.prototype.divide = /* @stopify flat */ function(other, errbacks) {\\n  var quotientAndRemainder = bnDivideAndRemainder.call(this, other);\\n  if (quotientAndRemainder[1].compareTo(BigInteger.ZERO) === 0) {\\n    return quotientAndRemainder[0];\\n  } else {\\n    var result = add(quotientAndRemainder[0],\\n                     Rational.makeInstance(quotientAndRemainder[1], other, errbacks), errbacks);\\n    return result;\\n  }\\n};\\n\\nBigInteger.prototype.numerator = /* @stopify flat */ function() {\\n  return this;\\n};\\n\\nBigInteger.prototype.denominator = /* @stopify flat */ function() {\\n  return 1;\\n};\\n\\n(/* @stopify flat */ function() {\\n  // Classic implementation of Newton-Raphson square-root search,\\n  // adapted for integer-sqrt.\\n  // http://en.wikipedia.org/wiki/Newton's_method#Square_root_of_a_number\\n  var searchIter = /* @stopify flat */ function(n, guess, errbacks) {\\n    while(!(lessThanOrEqual(sqr(guess),n, errbacks) &&\\n            lessThan(n,sqr(add(guess, 1, errbacks), errbacks), errbacks))) {\\n      guess = floor(divide(add(guess,\\n                               floor(divide(n, guess, errbacks), errbacks), errbacks),\\n                           2, errbacks), errbacks);\\n    }\\n    return guess;\\n  };\\n\\n  // integerSqrt: -> pyretnum\\n  BigInteger.prototype.integerSqrt = /* @stopify flat */ function(errbacks) {\\n    var n;\\n    if(sign(this) >= 0) {\\n      return searchIter(this, this, errbacks);\\n    } else {\\n      errbacks.throwDomainError('integerSqrt of negative bignum ' + this);\\n    }\\n  };\\n})();\\n\\n(/* @stopify flat */ function() {\\n  // Get an approximation using integerSqrt, and then start another\\n  // Newton-Raphson search if necessary.\\n  BigInteger.prototype.sqrt = /* @stopify flat */ function(errbacks) {\\n    var approx = this.integerSqrt(errbacks), fix;\\n    if (eqv(sqr(approx, errbacks), this, errbacks)) {\\n      return approx;\\n    }\\n    fix = toFixnum(this);\\n    if (isFinite(fix)) {\\n      return Roughnum.makeInstance(Math.sqrt(fix), errbacks);\\n    } else {\\n      return approx;\\n    }\\n  };\\n})();\\n\\n// sqrt: -> pyretnum\\n// http://en.wikipedia.org/wiki/Newton's_method#Square_root_of_a_number\\n// Produce the square root.\\n\\n// floor: -> pyretnum\\n// Produce the floor.\\nBigInteger.prototype.floor = /* @stopify flat */ function(errbacks) {\\n  return this;\\n}\\n\\n// ceiling: -> pyretnum\\n// Produce the ceiling.\\nBigInteger.prototype.ceiling = /* @stopify flat */ function(errbacks) {\\n  return this;\\n}\\n\\n// round: -> pyretnum\\n// Round to the nearest integer.\\nBigInteger.prototype.round = /* @stopify flat */ function(n, errbacks) {\\n  return this;\\n};\\n\\nBigInteger.prototype.roundEven = /* @stopify flat */ function(n, errbacks) {\\n  return this;\\n};\\n\\n// log: -> pyretnum\\n// Produce the log.\\nBigInteger.prototype.log = /* @stopify flat */ function(n, errbacks) {\\n  return log(this.toFixnum(), errbacks);\\n};\\n\\n// tan: -> pyretnum\\n// Produce the tan.\\nBigInteger.prototype.tan = /* @stopify flat */ function(n, errbacks) {\\n  return tan(this.toFixnum(), errbacks);\\n};\\n\\n// atan: -> pyretnum\\n// Produce the arc tangent.\\nBigInteger.prototype.atan = /* @stopify flat */ function(n, errbacks) {\\n  return atan(this.toFixnum(), errbacks);\\n};\\n\\n// cos: -> pyretnum\\n// Produce the cosine.\\nBigInteger.prototype.cos = /* @stopify flat */ function(n, errbacks) {\\n  return cos(this.toFixnum(), errbacks);\\n};\\n\\n// sin: -> pyretnum\\n// Produce the sine.\\nBigInteger.prototype.sin = /* @stopify flat */ function(n, errbacks) {\\n  return sin(this.toFixnum(), errbacks);\\n};\\n\\n// expt: pyretnum -> pyretnum\\n// Produce the power to the input.\\nBigInteger.prototype.expt = /* @stopify flat */ function(n, errbacks) {\\n  return bnPow.call(this, n);\\n};\\n\\n// exp: -> pyretnum\\n// Produce e raised to the given power.\\nBigInteger.prototype.exp = /* @stopify flat */ function(errbacks) {\\n  var res = Math.exp(this.toFixnum());\\n  if (!isFinite(res))\\n    errbacks.throwDomainError('exp: argument too large: ' + this);\\n  return Roughnum.makeInstance(res, errbacks);\\n};\\n\\n// acos: -> pyretnum\\n// Produce the arc cosine.\\nBigInteger.prototype.acos = /* @stopify flat */ function(n, errbacks) {\\n  return acos(this.toFixnum(), errbacks);\\n};\\n\\n// asin: -> pyretnum\\n// Produce the arc sine.\\nBigInteger.prototype.asin = /* @stopify flat */ function(n, errbacks) {\\n  return asin(this.toFixnum(), errbacks);\\n};\\n\\n//////////////////////////////////////////////////////////////////////\\n// toRepeatingDecimal: jsnum jsnum {limit: number}? -> [string, string, string]\\n//\\n// Given the numerator and denominator parts of a rational,\\n// produces the repeating-decimal representation, where the first\\n// part are the digits before the decimal, the second are the\\n// non-repeating digits after the decimal, and the third are the\\n// remaining repeating decimals.\\n//\\n// An optional limit on the decimal expansion can be provided, in which\\n// case the search cuts off if we go past the limit.\\n// If this happens, the third argument returned becomes '...' to indicate\\n// that the search was prematurely cut off.\\nvar toRepeatingDecimal = (/* @stopify flat */ function() {\\n  var getResidue = /* @stopify flat */ function(r, d, limit, errbacks) {\\n    var digits = [];\\n    var seenRemainders = {};\\n    seenRemainders[r] = true;\\n    while(true) {\\n      if (limit-- <= 0) {\\n        return [digits.join(''), '...']\\n      }\\n\\n      var nextDigit = quotient(\\n        multiply(r, 10, errbacks), d, errbacks);\\n      var nextRemainder = remainder(\\n        multiply(r, 10, errbacks),\\n        d, errbacks);\\n      digits.push(nextDigit.toString());\\n      if (seenRemainders[nextRemainder]) {\\n        r = nextRemainder;\\n        break;\\n      } else {\\n        seenRemainders[nextRemainder] = true;\\n        r = nextRemainder;\\n      }\\n    }\\n\\n    var firstRepeatingRemainder = r;\\n    var repeatingDigits = [];\\n    while (true) {\\n      var nextDigit = quotient(multiply(r, 10, errbacks), d, errbacks);\\n      var nextRemainder = remainder(\\n        multiply(r, 10, errbacks),\\n        d, errbacks);\\n      repeatingDigits.push(nextDigit.toString());\\n      if (equals(nextRemainder, firstRepeatingRemainder)) {\\n        break;\\n      } else {\\n        r = nextRemainder;\\n      }\\n    };\\n\\n    var digitString = digits.join('');\\n    var repeatingDigitString = repeatingDigits.join('');\\n\\n    while (digitString.length >= repeatingDigitString.length &&\\n           (digitString.substring(\\n             digitString.length - repeatingDigitString.length)\\n            === repeatingDigitString)) {\\n      digitString = digitString.substring(\\n        0, digitString.length - repeatingDigitString.length);\\n    }\\n\\n    return [digitString, repeatingDigitString];\\n\\n  };\\n\\n  return /* @stopify flat */ function(n, d, options, errbacks) {\\n    // default limit on decimal expansion; can be overridden\\n    var limit = 512;\\n    if (options && typeof(options.limit) !== 'undefined') {\\n      limit = options.limit;\\n    }\\n    if (! isInteger(n)) {\\n      errbacks.throwDomainError('toRepeatingDecimal: n ' + n.toString() +\\n                                \\\" is not an integer.\\\");\\n    }\\n    if (! isInteger(d)) {\\n      errbacks.throwDomainError('toRepeatingDecimal: d ' + d.toString() +\\n                                \\\" is not an integer.\\\");\\n    }\\n    if (equals(d, 0, errbacks)) {\\n      errbacks.throwDomainError('toRepeatingDecimal: d equals 0');\\n    }\\n    if (lessThan(d, 0, errbacks)) {\\n      errbacks.throwDomainError('toRepeatingDecimal: d < 0');\\n    }\\n    var sign = (lessThan(n, 0) ? \\\"-\\\" : \\\"\\\");\\n    n = abs(n, errbacks);\\n    var beforeDecimalPoint = sign + quotient(n, d, errbacks);\\n    var afterDecimals = getResidue(remainder(n, d, errbacks), d, limit, errbacks);\\n    return [beforeDecimalPoint].concat(afterDecimals);\\n  };\\n})();\\n//////////////////////////////////////////////////////////////////////\\n// toStringDigits: jsnum jsnum -> string\\n// Converts the number to a string, providing digits precision in the\\n// output.  If digits is positive, provides that many digits to the right\\n// of the decimal point (including adding zeroes beyond the actual precision of\\n// the number).  If digits is negative, rounds that many positions to the\\n// left of the decimal, replacing them with zeroes.\\n//\\n// Note that num-to-string-digits is only for formatting, and its\\n// output's apparent precision may be unrelated to the actual precision of the\\n// input number, which may have been an approximation, or unrepresentable in\\n// decimal.\\n/* @stopify flat */ \\nfunction toStringDigits(n, digits, errbacks) {\\n  if (!isInteger(digits)) {\\n    errbacks.throwDomainError('num-to-string-digits: digits should be an integer');\\n  }\\n  var tenDigits = expt(10, digits, errbacks);\\n  var d = toFixnum(digits);\\n  n = divide(round(multiply(n, tenDigits, errbacks), errbacks), tenDigits, errbacks);\\n  if (isInteger(n)) {\\n    var ans = n.toString();\\n    if (d >= 1) {\\n      ans += '.';\\n      for (var i = 0; i < d; i++) {\\n        ans += '0';\\n      }\\n    }\\n    return ans;\\n  }\\n  // n is not an integer implies that d >= 1\\n  var decimal = toRepeatingDecimal(n.numerator(), n.denominator(), undefined, errbacks);\\n  var ans = decimal[1].toString();\\n  while (ans.length < d) {\\n    ans += decimal[2];\\n  }\\n  return decimal[0] + '.' + ans.substring(0, d);\\n}\\n//////////////////////////////////////////////////////////////////////\\n\\n// External interface of js-numbers:\\n\\nNumbers['fromFixnum'] = fromFixnum;\\nNumbers['fromString'] = fromString;\\nNumbers['fromSchemeString'] = fromSchemeString;\\nNumbers['makeBignum'] = makeBignum;\\nNumbers['makeRational'] = Rational.makeInstance;\\nNumbers['makeRoughnum'] = Roughnum.makeInstance;\\n\\nNumbers['isPyretNumber'] = isPyretNumber;\\nNumbers['isRational'] = isRational;\\nNumbers['isReal'] = isReal;\\nNumbers['isExact'] = isExact;\\nNumbers['isInteger'] = isInteger;\\nNumbers['isRoughnum'] = isRoughnum;\\nNumbers['isPositive'] = isPositive;\\nNumbers['isNegative'] = isNegative;\\nNumbers['isNonPositive'] = isNonPositive;\\nNumbers['isNonNegative'] = isNonNegative;\\n\\nNumbers['toFixnum'] = toFixnum;\\nNumbers['toExact'] = toExact;\\nNumbers['toRational'] = toRational;\\nNumbers['toRoughnum'] = toRoughnum;\\n\\nNumbers['add'] = add;\\nNumbers['subtract'] = subtract;\\nNumbers['multiply'] = multiply;\\nNumbers['divide'] = divide;\\nNumbers['equals'] = equals;\\nNumbers['equalsAnyZero'] = equalsAnyZero;\\nNumbers['eqv'] = eqv; // why is this being exported?\\nNumbers['roughlyEquals'] = roughlyEquals;\\nNumbers['roughlyEqualsRel'] = roughlyEqualsRel;\\nNumbers['greaterThanOrEqual'] = greaterThanOrEqual;\\nNumbers['lessThanOrEqual'] = lessThanOrEqual;\\nNumbers['greaterThan'] = greaterThan;\\nNumbers['lessThan'] = lessThan;\\nNumbers['expt'] = expt;\\nNumbers['exp'] = exp;\\nNumbers['modulo'] = modulo;\\nNumbers['numerator'] = numerator;\\nNumbers['denominator'] = denominator;\\nNumbers['integerSqrt'] = integerSqrt;\\nNumbers['sqrt'] = sqrt;\\nNumbers['abs'] = abs;\\nNumbers['quotient'] = quotient;\\nNumbers['remainder'] = remainder;\\nNumbers['floor'] = floor;\\nNumbers['ceiling'] = ceiling;\\nNumbers['round'] = round;\\nNumbers['roundEven'] = roundEven;\\nNumbers['log'] = log;\\nNumbers['tan'] = tan;\\nNumbers['atan'] = atan;\\nNumbers['atan2'] = atan2;\\nNumbers['cos'] = cos;\\nNumbers['sin'] = sin;\\nNumbers['tan'] = tan;\\nNumbers['acos'] = acos;\\nNumbers['asin'] = asin;\\nNumbers['sqr'] = sqr;\\nNumbers['gcd'] = gcd;\\nNumbers['lcm'] = lcm;\\n\\nNumbers['toRepeatingDecimal'] = toRepeatingDecimal;\\nNumbers['toStringDigits'] = toStringDigits;\\n\\n// The following exposes the class representations for easier\\n// integration with other projects.\\nNumbers['BigInteger'] = BigInteger;\\nNumbers['Rational'] = Rational;\\nNumbers['Roughnum'] = Roughnum;\\nNumbers['FloatPoint'] = Roughnum; //FIXME\\nNumbers['Complex'] = Roughnum; //FIXME\\n\\nNumbers['MIN_FIXNUM'] = MIN_FIXNUM;\\nNumbers['MAX_FIXNUM'] = MAX_FIXNUM;\\n\\nmodule.exports = Numbers;\\n\",\"timestamp\":1568484216651},{\"key\":\"prewritten/list-immutable.arr.js\",\"content\":\"// NOTE(alex): Cannot use object binding pattern b/c Babel limitations (through Stopify)\\r\\nconst immutable = require( 'immutable' );\\r\\nconst List = immutable.List;\\r\\n\\r\\nmodule.exports = {\\r\\n  'list': {\\r\\n    'make': function( list ) {\\r\\n      return List( list );\\r\\n    }\\r\\n  },\\r\\n  'to-list': function( list ) {\\r\\n    return List( list );\\r\\n  },\\r\\n  'at': function( list, index ) {\\r\\n    return list.get( index );\\r\\n  },\\r\\n  'length': function( list ) {\\r\\n    return list.size;\\r\\n  },\\r\\n  'contains': function( list, elm ) {\\r\\n    return list.includes( elm );\\r\\n  },\\r\\n  'map': function( list, fun ) {\\r\\n    return list.map( fun );\\r\\n  },\\r\\n  'flat-map': function( fun, list ) {\\r\\n    return List( list ).flatMap( fun );\\r\\n  },\\r\\n  'flatten': function( list ) {\\r\\n    return list.flatten();\\r\\n  },\\r\\n  'slice': function( list, start, end ) {\\r\\n    if (end === undefined) {\\r\\n      end = list.length;\\r\\n    }\\r\\n    return list.slice( start, end );\\r\\n  },\\r\\n  'push': function( list, elm ) {\\r\\n    return list.push( elm );\\r\\n  },\\r\\n  'filter': function( fun, list ) {\\r\\n    return list.filter( fun );\\r\\n  },\\r\\n  'reduce': function( fun, list, val ) {\\r\\n    return list.reduce( fun, val );\\r\\n  },\\r\\n  'sum': function( list ) {\\r\\n    return list.reduce( ( x, y ) => x + y, 0 );\\r\\n  },\\r\\n  'min': function( list ) {\\r\\n    return list.min();\\r\\n  },\\r\\n  'max': function( list ) {\\r\\n    return list.max();\\r\\n  },\\r\\n  'range': function( start, end ) {\\r\\n    let list = List( [] );\\r\\n\\r\\n    for ( var i = start; i < end; i++ ) {\\r\\n      list = list.push( i );\\r\\n    }\\r\\n\\r\\n    return list;\\r\\n  },\\r\\n  'empty-list': function() {\\r\\n    return List( [] );\\r\\n  },\\r\\n  'concat': function( listA, listB ) {\\r\\n\\t\\treturn listA.concat( listB );\\r\\n  },\\r\\n  'is-list': function( list ) {\\r\\n    return List.isList( list );\\r\\n  }\\r\\n};\\r\\n\",\"timestamp\":1568484216649},{\"key\":\"prewritten/list-immutable.arr.json\",\"content\":\"{\\r\\n  \\\"requires\\\": [],\\r\\n  \\\"provides\\\": {\\r\\n    \\\"shorthands\\\": {\\r\\n      \\\"lOfA\\\": [\\\"tyapp\\\", [\\\"local\\\", \\\"List\\\"], [[\\\"tid\\\", \\\"a\\\"]]],\\r\\n      \\\"lOfB\\\": [\\\"tyapp\\\", [\\\"local\\\", \\\"List\\\"], [[\\\"tid\\\", \\\"b\\\"]]],\\r\\n      \\\"lOfInt\\\": [\\\"tyapp\\\", [\\\"local\\\", \\\"List\\\"], [\\\"Number\\\"]],\\r\\n      \\\"tva\\\": [\\\"tid\\\", \\\"a\\\"],\\r\\n      \\\"tvb\\\": [\\\"tid\\\", \\\"b\\\"]\\r\\n    },\\r\\n    \\\"values\\\": {\\r\\n      \\\"list\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"Maker\\\", [\\\"tid\\\", \\\"a\\\"], \\\"lOfA\\\"]],\\r\\n      \\\"to-list\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfA\\\"], \\\"lOfA\\\"]],\\r\\n      \\\"at\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfA\\\", \\\"Number\\\"], \\\"tva\\\"]],\\r\\n      \\\"length\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfA\\\"], \\\"Number\\\"]],\\r\\n      \\\"contains\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfA\\\", \\\"tva\\\"], \\\"Boolean\\\"]],\\r\\n      \\\"map\\\": [\\\"forall\\\", [\\\"a\\\", \\\"b\\\"], [\\\"arrow\\\", [\\\"lOfA\\\", [\\\"arrow\\\", [\\\"tva\\\"], \\\"tvb\\\"]], \\\"lOfB\\\"]],\\r\\n      \\\"flat-map\\\": [\\\"forall\\\", [\\\"a\\\", \\\"b\\\"], [\\\"arrow\\\", [[\\\"arrow\\\", [\\\"tva\\\"], \\\"tvb\\\"], \\\"lOfA\\\"], \\\"lOfB\\\"]],\\r\\n      \\\"flatten\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfA\\\"], \\\"lOfA\\\"]],\\r\\n      \\\"slice\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfA\\\", \\\"Number\\\", \\\"Number\\\"], \\\"lOfA\\\"]],\\r\\n      \\\"push\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfA\\\", \\\"tva\\\"], \\\"lOfA\\\"]],\\r\\n      \\\"filter\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [[\\\"arrow\\\", [\\\"tva\\\"], \\\"Boolean\\\"], \\\"lOfA\\\"], \\\"lOfA\\\"]],\\r\\n      \\\"reduce\\\": [\\\"forall\\\", [\\\"a\\\", \\\"b\\\"], [\\\"arrow\\\", [[\\\"arrow\\\", [\\\"tvb\\\", \\\"tva\\\"], \\\"tvb\\\"], \\\"lOfA\\\", \\\"tvb\\\"], \\\"tvb\\\"]],\\r\\n      \\\"sum\\\": [\\\"arrow\\\", [\\\"lOfInt\\\"], \\\"Number\\\"],\\r\\n      \\\"min\\\": [\\\"arrow\\\", [\\\"lOfInt\\\"], \\\"Number\\\"],\\r\\n      \\\"max\\\": [\\\"arrow\\\", [\\\"lOfInt\\\"], \\\"Number\\\"],\\r\\n      \\\"range\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\"], \\\"lOfInt\\\"],\\r\\n      \\\"empty-list\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [], \\\"lOfA\\\"]],\\r\\n      \\\"concat\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfA\\\", \\\"lOfA\\\"], \\\"lOfA\\\"]],\\r\\n      \\\"is-list\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfA\\\"], \\\"Boolean\\\"]]\\r\\n    },\\r\\n    \\\"aliases\\\": {\\r\\n      \\\"List\\\": [\\\"local\\\", \\\"List\\\"]\\r\\n    },\\r\\n    \\\"datatypes\\\": {\\r\\n      \\\"List\\\": [\\\"data\\\", \\\"List\\\", [\\\"a\\\"], [], {}]\\r\\n    }\\r\\n  }\\r\\n}\\r\\n\",\"timestamp\":1568484216697},{\"key\":\"prewritten/list.arr.js\",\"content\":\"module.exports = {\\r\\n  'list': {\\r\\n    'make': function( list ) {\\r\\n      return list;\\r\\n    }\\r\\n  },\\r\\n  'at': function( list, index ) {\\r\\n    return list[index];\\r\\n  },\\r\\n  'length': function( list ) {\\r\\n    return list.length;\\r\\n  },\\r\\n  'contains': function( list, elm ) {\\r\\n    return list.some( function(cur) { return cur === elm; } );\\r\\n  },\\r\\n  'map': function( fun, list ) {\\r\\n    return list.map( fun );\\r\\n  },\\r\\n  'slice': function( list, start, end) {\\r\\n    if(end === undefined) { end = list.length; }\\r\\n    return list.slice( start, end );\\r\\n  },\\r\\n  'push': function( list, elm ) {\\r\\n    list.push( elm );\\r\\n    return list;\\r\\n  },\\r\\n  'filter': function( fun, list ) {\\r\\n    return list.filter( fun );\\r\\n  },\\r\\n  'reduce': function( fun, val, list) {\\r\\n    return list.reduce( fun, val );\\r\\n  },\\r\\n  'sum': function( list ) {\\r\\n    return list.reduce( function( x, y ) { return x + y; }, 0 );\\r\\n  },\\r\\n  'min': function( list ) {\\r\\n    return list.reduce( function( x, y ) { return Math.min( x, y ); }, list[0] );\\r\\n  },\\r\\n  'max': function( list ) {\\r\\n    return list.reduce( function( x, y ) { return Math.max( x, y ); }, list[0] );\\r\\n  },\\r\\n  'range': function( start, end ) {\\r\\n    var list = [];\\r\\n\\r\\n    for ( var i = start; i < end; i++ ) {\\r\\n      list.push( i );\\r\\n    }\\r\\n\\r\\n    return list;\\r\\n  },\\r\\n  'empty-list': function() {\\r\\n    return [];\\r\\n  },\\r\\n  'concat': function( listA, listB ) {\\r\\n    return listA.concat( listB );\\r\\n  },\\r\\n  'concat-push': function( listA, listB ) {\\r\\n    for ( var index = 0; index < listB.length; index++ ) {\\r\\n      listA.push( listB[index] );\\r\\n    }\\r\\n    \\r\\n    return listA;\\r\\n  }\\r\\n};\\r\\n\",\"timestamp\":1568484216646},{\"key\":\"prewritten/list.arr.json\",\"content\":\"{\\r\\n  \\\"requires\\\": [],\\r\\n  \\\"provides\\\": {\\r\\n    \\\"shorthands\\\": {\\r\\n      \\\"lOfA\\\": [\\\"tyapp\\\", [\\\"local\\\", \\\"List\\\"], [[\\\"tid\\\", \\\"a\\\"]]],\\r\\n      \\\"lOfB\\\": [\\\"tyapp\\\", [\\\"local\\\", \\\"List\\\"], [[\\\"tid\\\", \\\"b\\\"]]],\\r\\n      \\\"lOfInt\\\": [\\\"tyapp\\\", [\\\"local\\\", \\\"List\\\"], [\\\"Number\\\"]],\\r\\n      \\\"tva\\\": [\\\"tid\\\", \\\"a\\\"],\\r\\n      \\\"tvb\\\": [\\\"tid\\\", \\\"b\\\"]\\r\\n    },\\r\\n    \\\"values\\\": {\\r\\n      \\\"list\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"Maker\\\", [\\\"tid\\\", \\\"a\\\"], \\\"lOfA\\\"]],\\r\\n      \\\"at\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfA\\\", \\\"Number\\\"], \\\"tva\\\"]],\\r\\n      \\\"length\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfA\\\"], \\\"Number\\\"]],\\r\\n      \\\"contains\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfA\\\", \\\"tva\\\"], \\\"Boolean\\\"]],\\r\\n      \\\"map\\\": [\\\"forall\\\", [\\\"a\\\", \\\"b\\\"], [\\\"arrow\\\", [[\\\"arrow\\\", [\\\"tva\\\"], \\\"tvb\\\"], \\\"lOfA\\\"], \\\"lOfB\\\"]],\\r\\n      \\\"slice\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfA\\\", \\\"Number\\\", \\\"Number\\\"], \\\"lOfA\\\"]],\\r\\n      \\\"push\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfA\\\", \\\"tva\\\"], \\\"lOfA\\\"]],\\r\\n      \\\"filter\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [[\\\"arrow\\\", [\\\"tva\\\"], \\\"Boolean\\\"], \\\"lOfA\\\"], \\\"lOfA\\\"]],\\r\\n      \\\"reduce\\\": [\\\"forall\\\", [\\\"a\\\", \\\"b\\\"], [\\\"arrow\\\", [[\\\"arrow\\\", [\\\"tvb\\\", \\\"tva\\\"], \\\"tvb\\\"], \\\"tvb\\\", \\\"lOfA\\\"], \\\"tvb\\\"]],\\r\\n      \\\"sum\\\": [\\\"arrow\\\", [\\\"lOfInt\\\"], \\\"Number\\\"],\\r\\n      \\\"min\\\": [\\\"arrow\\\", [\\\"lOfInt\\\"], \\\"Number\\\"],\\r\\n      \\\"max\\\": [\\\"arrow\\\", [\\\"lOfInt\\\"], \\\"Number\\\"],\\r\\n      \\\"range\\\": [\\\"arrow\\\", [\\\"Number\\\", \\\"Number\\\"], \\\"lOfInt\\\"],\\r\\n      \\\"empty-list\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [], \\\"lOfA\\\"]],\\r\\n      \\\"concat\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfA\\\", \\\"lOfA\\\"], \\\"lOfA\\\"]],\\r\\n      \\\"concat-push\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfA\\\", \\\"lOfA\\\"], \\\"lOfA\\\"]]\\r\\n    },\\r\\n    \\\"aliases\\\": {\\r\\n      \\\"List\\\": [\\\"local\\\", \\\"List\\\"]\\r\\n    },\\r\\n    \\\"datatypes\\\": {\\r\\n      \\\"List\\\": [\\\"data\\\", \\\"List\\\", [\\\"a\\\"], [], {}]\\r\\n    }\\r\\n  }\\r\\n}\\r\\n\",\"timestamp\":1568484216691},{\"key\":\"prewritten/option.arr.js\",\"content\":\"// provide *\\n// provide-types *\\n//\\n// import global as g\\n//\\n// data Option<a>:\\n//   | some(elt :: a)\\n//   | none\\n// end\\nvar g15 = require(\\\"././global.arr.js\\\");\\nvar _global = require(\\\"././global.arr.js\\\");\\nvar _nothing = undefined;\\nvar nothing14 = g15.nothing;\\nvar $some9 = {\\\"names\\\":[\\\"elt\\\"]};\\nvar $none10 = {\\\"names\\\":false};\\nvar Option1 = {\\\"some\\\":function some(elt11) {\\n  return {\\\"$brand\\\":$some9,\\n          \\\"$tag\\\":0,\\n          \\\"elt\\\":elt11};\\n},\\n               \\\"none\\\":{\\\"$brand\\\":$none10,\\n                       \\\"$tag\\\":1},\\n               \\\"is-some\\\":function some(val) {\\n                 return val.$brand === $some9;\\n               },\\n               \\\"is-none\\\":function none(val) {\\n                 return val.$brand === $none10;\\n               }};\\nvar is$Option6 = Option1[\\\"Option\\\"];\\nvar is$some5 = Option1[\\\"is-some\\\"];\\nvar some4 = Option1[\\\"some\\\"];\\nvar is$none3 = Option1[\\\"is-none\\\"];\\nvar none2 = Option1[\\\"none\\\"];\\nreturn module[\\\"exports\\\"] = {\\\"is-none\\\":is$none3,\\n                            \\\"none\\\":none2,\\n                            \\\"is-Option\\\":is$Option6,\\n                            \\\"is-some\\\":is$some5,\\n                            \\\"some\\\":some4,\\n                            \\\"$answer\\\":_global[\\\"trace-value\\\"](\\\"srcloc\\\",nothing14),\\n                            \\\"$checks\\\":undefined};\\n\",\"timestamp\":1568484216648},{\"key\":\"prewritten/option.arr.json\",\"content\":\"{\\\"theMap\\\":\\\"{\\\\\\\"version\\\\\\\":3,\\\\\\\"sources\\\\\\\":[\\\\\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\\\\\"],\\\\\\\"names\\\\\\\":[\\\\\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\\\\\"],\\\\\\\"mappings\\\\\\\":\\\\\\\"AAACA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA\\\\\\\",\\\\\\\"file\\\\\\\":\\\\\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\\\\\"}\\\",\\n\\\"nativeRequires\\\":[],\\n\\\"provides\\\":{\\\"modules\\\":{},\\n\\\"values\\\":{\\\"is-none\\\":{\\\"bind\\\":\\\"let\\\",\\n\\\"origin\\\":{\\\"local-bind-site\\\":[\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\",\\n8,\\n2,\\n84,\\n8,\\n8,\\n90],\\n\\\"definition-bind-site\\\":[\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\",\\n8,\\n2,\\n84,\\n8,\\n8,\\n90],\\n\\\"new-definition\\\":true,\\n\\\"uri-of-definition\\\":\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\"},\\n\\\"typ\\\":[\\\"forall\\\",\\n[\\\"atom#a#10\\\"],\\n[\\\"arrow\\\",\\n[[\\\"tyapp\\\",{\\\"tag\\\":\\\"name\\\",\\n\\\"origin\\\":{\\\"import-type\\\":\\\"$ELF\\\"},\\n\\\"name\\\":\\\"Option\\\"},[[\\\"tid\\\",\\n\\\"atom#a#10\\\"]]]],\\n{\\\"tag\\\":\\\"name\\\",\\n\\\"origin\\\":{\\\"import-type\\\":\\\"uri\\\",\\n\\\"uri\\\":\\\"builtin://global\\\"},\\n\\\"name\\\":\\\"Boolean\\\"}]]},\\n\\\"none\\\":{\\\"bind\\\":\\\"let\\\",\\n\\\"origin\\\":{\\\"local-bind-site\\\":[\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\",\\n8,\\n2,\\n84,\\n8,\\n8,\\n90],\\n\\\"definition-bind-site\\\":[\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\",\\n8,\\n2,\\n84,\\n8,\\n8,\\n90],\\n\\\"new-definition\\\":true,\\n\\\"uri-of-definition\\\":\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\"},\\n\\\"typ\\\":[\\\"forall\\\",\\n[\\\"atom#a#10\\\"],\\n[\\\"data%\\\",\\n[\\\"tyapp\\\",{\\\"tag\\\":\\\"name\\\",\\n\\\"origin\\\":{\\\"import-type\\\":\\\"$ELF\\\"},\\n\\\"name\\\":\\\"Option\\\"},[[\\\"tid\\\",\\n\\\"atom#a#10\\\"]]],\\n\\\"none\\\"]]},\\n\\\"is-Option\\\":{\\\"bind\\\":\\\"let\\\",\\n\\\"origin\\\":{\\\"local-bind-site\\\":[\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\",\\n6,\\n0,\\n47,\\n9,\\n3,\\n94],\\n\\\"definition-bind-site\\\":[\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\",\\n6,\\n0,\\n47,\\n9,\\n3,\\n94],\\n\\\"new-definition\\\":true,\\n\\\"uri-of-definition\\\":\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\"},\\n\\\"typ\\\":[\\\"forall\\\",\\n[\\\"atom#a#10\\\"],\\n[\\\"arrow\\\",\\n[[\\\"tyapp\\\",{\\\"tag\\\":\\\"name\\\",\\n\\\"origin\\\":{\\\"import-type\\\":\\\"$ELF\\\"},\\n\\\"name\\\":\\\"Option\\\"},[[\\\"tid\\\",\\n\\\"atom#a#10\\\"]]]],\\n{\\\"tag\\\":\\\"name\\\",\\n\\\"origin\\\":{\\\"import-type\\\":\\\"uri\\\",\\n\\\"uri\\\":\\\"builtin://global\\\"},\\n\\\"name\\\":\\\"Boolean\\\"}]]},\\n\\\"is-some\\\":{\\\"bind\\\":\\\"let\\\",\\n\\\"origin\\\":{\\\"local-bind-site\\\":[\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\",\\n7,\\n2,\\n65,\\n7,\\n18,\\n81],\\n\\\"definition-bind-site\\\":[\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\",\\n7,\\n2,\\n65,\\n7,\\n18,\\n81],\\n\\\"new-definition\\\":true,\\n\\\"uri-of-definition\\\":\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\"},\\n\\\"typ\\\":[\\\"forall\\\",\\n[\\\"atom#a#10\\\"],\\n[\\\"arrow\\\",\\n[[\\\"tyapp\\\",{\\\"tag\\\":\\\"name\\\",\\n\\\"origin\\\":{\\\"import-type\\\":\\\"$ELF\\\"},\\n\\\"name\\\":\\\"Option\\\"},[[\\\"tid\\\",\\n\\\"atom#a#10\\\"]]]],\\n{\\\"tag\\\":\\\"name\\\",\\n\\\"origin\\\":{\\\"import-type\\\":\\\"uri\\\",\\n\\\"uri\\\":\\\"builtin://global\\\"},\\n\\\"name\\\":\\\"Boolean\\\"}]]},\\n\\\"some\\\":{\\\"bind\\\":\\\"let\\\",\\n\\\"origin\\\":{\\\"local-bind-site\\\":[\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\",\\n7,\\n2,\\n65,\\n7,\\n18,\\n81],\\n\\\"definition-bind-site\\\":[\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\",\\n7,\\n2,\\n65,\\n7,\\n18,\\n81],\\n\\\"new-definition\\\":true,\\n\\\"uri-of-definition\\\":\\\"file:///home/michael/projects/pyret-lang/src/runtime-arr/option.arr\\\"},\\n\\\"typ\\\":[\\\"forall\\\",\\n[\\\"atom#a#10\\\"],\\n[\\\"arrow\\\",\\n[[\\\"tid\\\",\\n\\\"atom#a#10\\\"]],\\n[\\\"data%\\\",\\n[\\\"tyapp\\\",{\\\"tag\\\":\\\"name\\\",\\n\\\"origin\\\":{\\\"import-type\\\":\\\"$ELF\\\"},\\n\\\"name\\\":\\\"Option\\\"},[[\\\"tid\\\",\\n\\\"atom#a#10\\\"]]],\\n\\\"some\\\"]]]}},\\n\\\"datatypes\\\":{\\\"Option\\\":[\\\"data\\\",\\\"Option\\\",[\\\"atom#a#10\\\"],[[\\\"some\\\",\\n[[\\\"elt\\\",\\n[\\\"tid\\\",\\n\\\"atom#a#10\\\"]]],\\n{}],[\\\"none\\\",\\n{}]],{}]},\\n\\\"aliases\\\":{\\\"Option\\\":{\\\"tag\\\":\\\"name\\\",\\n\\\"origin\\\":{\\\"import-type\\\":\\\"$ELF\\\"},\\n\\\"name\\\":\\\"Option\\\"}}},\\n\\\"requires\\\":[]}\\n\",\"timestamp\":1568484216693},{\"key\":\"prewritten/runtime.js\",\"content\":\"\\\"use strict\\\";\\nexports.__esModule = true;\\n/*\\n * 'export named-js-value' desugars into 'exports.name = js-value'\\n *\\n * https://stackoverflow.com/questions/16383795/difference-between-module-exports-and-exports-in-the-commonjs-module-system\\n *\\n */\\nvar _NUMBER = require(\\\"./js-numbers.js\\\");\\nvar $EqualBrand = { \\\"names\\\": false };\\nvar $NotEqualBrand = { \\\"names\\\": [\\\"reason\\\", \\\"value1\\\", \\\"value2\\\"] };\\nvar $UnknownBrand = { \\\"names\\\": [\\\"reason\\\", \\\"value1\\\", \\\"value2\\\"] };\\nvar $EqualTag = 0;\\nvar $NotEqualTag = 1;\\nvar $UnknownTag = 2;\\nvar $PTupleBrand = \\\"tuple\\\";\\nvar $PRefBrand = \\\"ref\\\";\\nfunction PTuple(values) {\\n    values[\\\"$brand\\\"] = $PTupleBrand;\\n    return values;\\n}\\nexports.PTuple = PTuple;\\nfunction Equal() {\\n    return {\\n        \\\"$brand\\\": $EqualBrand,\\n        \\\"$tag\\\": $EqualTag\\n    };\\n}\\nfunction NotEqual(reason, value1, value2) {\\n    return {\\n        \\\"$brand\\\": $NotEqualBrand,\\n        \\\"$tag\\\": $NotEqualTag,\\n        \\\"reason\\\": reason,\\n        \\\"value1\\\": value1,\\n        \\\"value2\\\": value2\\n    };\\n}\\nexports.NotEqual = NotEqual;\\nfunction Unknown(reason, value1, value2) {\\n    return {\\n        \\\"$brand\\\": $UnknownBrand,\\n        \\\"$tag\\\": $UnknownTag,\\n        \\\"reason\\\": reason,\\n        \\\"value1\\\": value1,\\n        \\\"value2\\\": value2\\n    };\\n}\\nexports.Unknown = Unknown;\\nfunction isEqual(val) {\\n    return val.$brand === $EqualBrand;\\n}\\nexports.isEqual = isEqual;\\nfunction isNotEqual(val) {\\n    return val.$brand === $NotEqualBrand;\\n}\\nexports.isNotEqual = isNotEqual;\\nfunction isUnknown(val) {\\n    return val.$brand === $UnknownBrand;\\n}\\nexports.isUnknown = isUnknown;\\n// ********* Helpers *********\\nfunction equalityResultToBool(ans) {\\n    if (isEqual(ans)) {\\n        return true;\\n    }\\n    else if (isNotEqual(ans)) {\\n        return false;\\n    }\\n    else if (isUnknown(ans)) {\\n        var unknownVariant = ans;\\n        throw {\\n            reason: unknownVariant.reason,\\n            value1: unknownVariant.value1,\\n            value2: unknownVariant.value2\\n        };\\n    }\\n}\\nfunction isFunction(obj) { return typeof obj === \\\"function\\\"; }\\nfunction isMethod(obj) {\\n    return typeof obj === \\\"function\\\" && \\\"$brand\\\" in obj && obj[\\\"$brand\\\"] === \\\"METHOD\\\";\\n}\\n// TODO(alex): Will nothing always be value 'undefined'?\\nfunction isNothing(obj) { return obj === undefined; }\\n;\\n// TODO(alex): Identify opaque types\\nfunction isOpaque(val) { return false; }\\nvar isNumber = _NUMBER[\\\"isPyretNumber\\\"];\\nvar isRoughNumber = _NUMBER[\\\"isRoughnum\\\"];\\nvar numericEquals = _NUMBER[\\\"equals\\\"];\\nfunction isBoolean(val) {\\n    return typeof val === \\\"boolean\\\";\\n}\\nfunction isString(val) {\\n    return typeof val === \\\"string\\\";\\n}\\nfunction isDataVariant(val) {\\n    return (typeof val === \\\"object\\\") && (\\\"$brand\\\" in val) && !(isPTuple(val));\\n}\\nfunction isRawObject(val) {\\n    return (typeof val === \\\"object\\\") && !(\\\"$brand\\\" in val);\\n}\\nfunction isPTuple(val) {\\n    return (Array.isArray(val)) && (\\\"$brand\\\" in val) && (val[\\\"$brand\\\"] === $PTupleBrand);\\n}\\nfunction isArray(val) {\\n    return (Array.isArray(val)) && !(\\\"$brand\\\" in val);\\n}\\nfunction isPRef(val) {\\n    return (typeof val === \\\"object\\\") && (\\\"$brand\\\" in val) && (val[\\\"$brand\\\"] === $PRefBrand);\\n}\\nvar NumberErrbacks = {\\n    throwDivByZero: function (msg) { throw msg; },\\n    throwToleranceError: function (msg) { throw msg; },\\n    throwRelToleranceError: function (msg) { throw msg; },\\n    throwGeneralError: function (msg) { throw msg; },\\n    throwDomainError: function (msg) { throw msg; },\\n    throwSqrtNegative: function (msg) { throw msg; },\\n    throwLogNonPositive: function (msg) { throw msg; },\\n    throwIncomparableValues: function (msg) { throw msg; },\\n    throwInternalError: function (msg) { throw msg; }\\n};\\n// ********* Equality Functions *********\\nfunction identical3(v1, v2) {\\n    if (isFunction(v1) && isFunction(v2)) {\\n        return Unknown(\\\"Function\\\", v1, v2);\\n        // TODO(alex): Handle/detect methods\\n        // } else if (isMethod(v1) && isMethod(v2)) {\\n        //  return thisRuntime.ffi.unknown.app('Methods', v1,  v2);\\n        //  TODO(alex): Handle/detect rough numbers\\n    }\\n    else if (isRoughNumber(v1) && isRoughNumber(v2)) {\\n        return Unknown('Roughnums', v1, v2);\\n    }\\n    else if (v1 === v2) {\\n        return Equal();\\n    }\\n    else {\\n        return NotEqual(\\\"\\\", v1, v2);\\n    }\\n}\\nexports.identical3 = identical3;\\nfunction identical(v1, v2) {\\n    var ans = identical3(v1, v2);\\n    return equalityResultToBool(ans);\\n}\\nexports.identical = identical;\\n/*\\n * Structural equality. Stops at mutable data (refs) and only checks that\\n * mutable data are identical.\\n *\\n * Data variants and raw (unbranded) objects are NEVER equal.\\n *\\n */\\nfunction equalAlways3(e1, e2) {\\n    if (isEqual(identical3(e1, e2))) {\\n        // Identical so must always be equal\\n        return Equal();\\n    }\\n    var worklist = [[e1, e2]];\\n    while (worklist.length > 0) {\\n        var curr = worklist.pop();\\n        var v1 = curr[0];\\n        var v2 = curr[1];\\n        if (isEqual(identical3(v1, v2))) {\\n            // Identical so must always be equal\\n            continue;\\n        }\\n        if (isNumber(v1) && isNumber(v2)) {\\n            if (isRoughNumber(v1) || isRoughNumber(v2)) {\\n                return Unknown(\\\"Rough Number equal-always\\\", v1, v2);\\n            }\\n            else if (numericEquals(v1, v2, NumberErrbacks)) {\\n                continue;\\n            }\\n            else {\\n                return NotEqual(\\\"Numers\\\", v1, v2);\\n            }\\n        }\\n        else if (isBoolean(v1) && isBoolean(v2)) {\\n            if (v1 !== v2) {\\n                return NotEqual(\\\"Booleans\\\", v1, v2);\\n            }\\n            continue;\\n        }\\n        else if (isString(v1) && isString(v2)) {\\n            if (v1 !== v2) {\\n                return NotEqual(\\\"Strings\\\", v1, v2);\\n            }\\n            continue;\\n        }\\n        else if (isFunction(v1) && isFunction(v2)) {\\n            // Cannot compare functions for equality\\n            return Unknown(\\\"Functions\\\", v1, v2);\\n            // TODO(alex): Handle methods\\n        }\\n        else if (isPTuple(v1) && isPTuple(v2)) {\\n            if (v1.length !== v2.length) {\\n                return NotEqual(\\\"PTuple Length\\\", v1, v2);\\n            }\\n            for (var i = 0; i < v1.length; i++) {\\n                worklist.push([v1[i], v2[i]]);\\n            }\\n            continue;\\n        }\\n        else if (isArray(v1) && isArray(v2)) {\\n            if (v1.length !== v2.length) {\\n                return NotEqual(\\\"Array Length\\\", v1, v2);\\n            }\\n            for (var i = 0; i < v1.length; i++) {\\n                worklist.push([v1[i], v2[i]]);\\n            }\\n            continue;\\n        }\\n        else if (isNothing(v1) && isNothing(v2)) {\\n            // TODO(alex): Is equality defined for Pyret Nothing?\\n            continue;\\n        }\\n        else if (isPRef(v1) && isPRef(v2)) {\\n            // In equal-always, non-identical refs are not equal\\n            if (v1.ref !== v2.ref) {\\n                return NotEqual(\\\"PRef'd Objects\\\", v1, v2);\\n            }\\n            continue;\\n        }\\n        else if (isDataVariant(v1) && isDataVariant(v2)) {\\n            if (v1.$brand && v1.$brand === v2.$brand) {\\n                if (\\\"_equals\\\" in v1) {\\n                    // TODO(alex): Recursive callback\\n                    var ans = v1[\\\"_equals\\\"](v2, undefined);\\n                    if (!isEqual(ans)) {\\n                        return ans;\\n                    }\\n                    else {\\n                        continue;\\n                    }\\n                }\\n                var fields1 = v1.$brand.names;\\n                var fields2 = v2.$brand.names;\\n                if (fields1.length !== fields2.length) {\\n                    // Not the same brand\\n                    return NotEqual(\\\"Object Brands\\\", v1, v2);\\n                }\\n                for (var i = 0; i < fields1.length; i += 1) {\\n                    if (fields1[i] != fields2[i]) {\\n                        // Not the same brand\\n                        return NotEqual(\\\"Field Brands\\\", fields1[i], fields2[i]);\\n                    }\\n                    worklist.push([v1[fields1[i]], v2[fields2[i]]]);\\n                }\\n                continue;\\n            }\\n            else {\\n                return NotEqual(\\\"Variant Brands\\\", v1, v2);\\n            }\\n        }\\n        else if (isRawObject(v1) && isRawObject(v2)) {\\n            var keys1 = Object.keys(v1);\\n            var keys2 = Object.keys(v2);\\n            if (keys1.length !== keys2.length) {\\n                return NotEqual(\\\"Raw Object Field Count\\\", v1, v2);\\n            }\\n            // Check for matching keys and push field to worklist\\n            for (var i = 0; i < keys1.length; i++) {\\n                var key2Index = keys2.indexOf(keys1[i]);\\n                if (key2Index === -1) {\\n                    // Key in v1 not found in v2\\n                    return NotEqual(\\\"Raw Object Missing Field '\\\" + keys1[i] + \\\"'\\\", v1, v2);\\n                }\\n                else {\\n                    // Push common field to worklist\\n                    worklist.push([v1[keys1[i]], v2[keys2[key2Index]]]);\\n                }\\n            }\\n            continue;\\n        }\\n        else {\\n            return NotEqual(\\\"\\\", e1, e2);\\n        }\\n    }\\n    return Equal();\\n}\\nexports.equalAlways3 = equalAlways3;\\nfunction equalAlways(v1, v2) {\\n    var ans = equalAlways3(v1, v2);\\n    return equalityResultToBool(ans);\\n}\\nexports.equalAlways = equalAlways;\\nfunction _spy(spyObject) {\\n    var message = spyObject.message();\\n    var spyLoc = spyObject.loc;\\n    if (message != null) {\\n        console.log(\\\"Spying \\\\\\\"\\\" + message + \\\"\\\\\\\" (at \\\" + spyLoc + \\\")\\\");\\n    }\\n    else {\\n        console.log(\\\"Spying (at \\\" + spyLoc + \\\")\\\");\\n    }\\n    var exprs = spyObject.exprs;\\n    for (var i = 0; i < exprs.length; i++) {\\n        var key = exprs[i].key;\\n        var loc = exprs[i].loc;\\n        var value = exprs[i].expr();\\n        console.log(\\\"    \\\" + key + \\\": \\\" + value + \\\" (at \\\" + loc + \\\")\\\");\\n    }\\n}\\nvar _globalCheckContext = [];\\nvar _globalCheckResults = [];\\nfunction getCheckResults() {\\n    return _globalCheckResults.slice();\\n}\\nfunction checkResults() {\\n    var errorCount = 0;\\n    _globalCheckResults.forEach(function (result) {\\n        if (!result.success) {\\n            errorCount += 1;\\n        }\\n    });\\n    if (errorCount === 0) {\\n        console.log(\\\"All tests pass\\\");\\n    }\\n    else {\\n        console.log(\\\"Some tests failed\\\");\\n    }\\n    _globalCheckResults.forEach(function (result) {\\n        if (result.success) {\\n            console.log(\\\"[PASS] Found <\\\" + result.lhs + \\\">. Expected <\\\" + result.rhs + \\\"> ([\\\" + result.path + \\\"], at \\\" + result.loc + \\\")\\\");\\n        }\\n        else {\\n            console.log(\\\"[FAIL] Found <\\\" + result.lhs + \\\">. Expected <\\\" + result.rhs + \\\"> ([\\\" + result.path + \\\"], at \\\" + result.loc + \\\")\\\");\\n        }\\n    });\\n    return getCheckResults();\\n}\\nfunction eagerCheckTest(test, loc) {\\n    try {\\n        var result = test();\\n        _globalCheckResults.push({\\n            success: result.success,\\n            path: _globalCheckContext.join(),\\n            loc: loc,\\n            lhs: result.lhs,\\n            rhs: result.rhs\\n        });\\n    }\\n    catch (e) {\\n        _globalCheckResults.push({\\n            success: false,\\n            path: _globalCheckContext.join(),\\n            loc: loc,\\n            lhs: undefined,\\n            rhs: undefined\\n        });\\n    }\\n}\\nfunction eagerCheckBlockRunner(name, checkBlock) {\\n    _globalCheckContext.push(name);\\n    try {\\n        checkBlock();\\n    }\\n    catch (e) {\\n        throw e;\\n    }\\n    finally {\\n        _globalCheckContext.pop();\\n    }\\n}\\n// ********* Other Functions *********\\nfunction traceValue(loc, value) {\\n    // NOTE(alex): stubbed out until we decide what to actually do with it\\n    return value;\\n}\\nexports.traceValue = traceValue;\\n// Allow '+' for string concat. \\n// Otherwise, defer to the number library.\\nfunction customAdd(lhs, rhs, errbacks) {\\n    if (typeof (lhs) === \\\"string\\\" && typeof (rhs) === \\\"string\\\") {\\n        return lhs + rhs;\\n    }\\n    else {\\n        return _NUMBER[\\\"add\\\"](lhs, rhs, errbacks);\\n    }\\n}\\n// MUTATES an object to rebind any methods to it\\nfunction _rebind(toRebind) {\\n    if (typeof toRebind === \\\"object\\\") {\\n        Object.keys(toRebind).forEach(function (key) {\\n            if (key === \\\"$brand\\\" || key === \\\"$tag\\\") {\\n                return;\\n            }\\n            var value = toRebind[key];\\n            if (isMethod(value)) {\\n                toRebind[key] = value[\\\"$binder\\\"](toRebind);\\n            }\\n        });\\n    }\\n    return toRebind;\\n}\\nfunction pauseStack(callback) {\\n    // @ts-ignore\\n    return $STOPIFY.pauseK(function (kontinue) {\\n        return callback({\\n            resume: function (val) { return kontinue({ type: \\\"normal\\\", value: val }); },\\n            error: function (err) { return kontinue({ type: \\\"error\\\", error: err }); }\\n        });\\n    });\\n}\\nexports.pauseStack = pauseStack;\\n// Hack needed b/c of interactions with the 'export' keyword\\n// Pyret instantiates singleton data varaints by taking a reference to the value\\n// TODO(alex): Should Pyret perform a function call to create a singleton data variant\\nmodule.exports[\\\"Equal\\\"] = Equal();\\n// Hack needed to match generate Pyret-code\\nmodule.exports[\\\"is-Equal\\\"] = isEqual;\\nmodule.exports[\\\"is-NotEqual\\\"] = isNotEqual;\\nmodule.exports[\\\"is-Unknown\\\"] = isUnknown;\\n// Expected runtime functions\\nmodule.exports[\\\"$spy\\\"] = _spy;\\nmodule.exports[\\\"$rebind\\\"] = _rebind;\\nmodule.exports[\\\"$checkTest\\\"] = eagerCheckTest;\\nmodule.exports[\\\"$checkBlock\\\"] = eagerCheckBlockRunner;\\nmodule.exports[\\\"$checkResults\\\"] = checkResults;\\nmodule.exports[\\\"$getCheckResults\\\"] = getCheckResults;\\nmodule.exports[\\\"$makeRational\\\"] = _NUMBER[\\\"makeRational\\\"];\\nmodule.exports[\\\"$makeRoughnum\\\"] = _NUMBER[\\\"makeRoughnum\\\"];\\nmodule.exports[\\\"$errCallbacks\\\"] = NumberErrbacks;\\nmodule.exports[\\\"_add\\\"] = customAdd;\\nmodule.exports[\\\"_subtract\\\"] = _NUMBER[\\\"subtract\\\"];\\nmodule.exports[\\\"_multiply\\\"] = _NUMBER[\\\"multiply\\\"];\\nmodule.exports[\\\"_divide\\\"] = _NUMBER[\\\"divide\\\"];\\nmodule.exports[\\\"_lessThan\\\"] = _NUMBER[\\\"lessThan\\\"];\\nmodule.exports[\\\"_greaterThan\\\"] = _NUMBER[\\\"greaterThan\\\"];\\nmodule.exports[\\\"_lessThanOrEqual\\\"] = _NUMBER[\\\"lessThanOrEqual\\\"];\\nmodule.exports[\\\"_greaterThanOrEqual\\\"] = _NUMBER[\\\"greaterThanOrEqual\\\"];\\nmodule.exports[\\\"_makeNumberFromString\\\"] = _NUMBER['fromString'];\\n\",\"timestamp\":1568484161605},{\"key\":\"prewritten/string-dict-immutable.arr.js\",\"content\":\"// NOTE(alex): Cannot use object binding pattern b/c Babel limitations (through Stopify)\\r\\nconst immutable = require( 'immutable' );\\r\\nconst Map = immutable.Map;\\r\\nconst List = immutable.List;\\r\\n\\r\\nmodule.exports = {\\r\\n  'string-dict': {\\r\\n    'make': function( list ) {\\r\\n      return Map( list[0] );\\r\\n    }\\r\\n  },\\r\\n  'make-string-dict': function() {\\r\\n    return Map();\\r\\n  },\\r\\n  'count': function( list ) {\\r\\n    var dict = Map();\\r\\n\\r\\n    for ( var index = 0; index < list.size; index++ ) {\\r\\n      var elm = list.get( index );\\r\\n\\r\\n      if ( dict.has( elm ) ) {\\r\\n        dict = dict.set( elm, dict.get( elm ) + 1 );\\r\\n      } else {\\r\\n        dict = dict.set( elm, 1 );\\r\\n      }\\r\\n    }\\r\\n\\r\\n    return dict;\\r\\n  },\\r\\n  'apply': function( list, fun ) {\\r\\n    var dict = Map();\\r\\n\\r\\n    for ( var index = 0; index < list.size; index++ ) {\\r\\n      var elm = list.get( index );\\r\\n\\r\\n      dict = dict.set( elm, fun( elm ) );\\r\\n    }\\r\\n\\r\\n    return dict;\\r\\n  },\\r\\n  'insert': function( dict, key, value ) {\\r\\n    return dict.set( key, value );\\r\\n  },\\r\\n  'size': function( dict ) {\\r\\n    return dict.size;\\r\\n  },\\r\\n  'get': function( dict, elm ) {\\r\\n    return dict.get( elm );\\r\\n  },\\r\\n  'has-key': function( dict, elm ) {\\r\\n    return dict.has( elm );\\r\\n  },\\r\\n  'keys': function( dict ) {\\r\\n    return List( dict ).map( lst => lst[0] );\\r\\n  },\\r\\n  'values': function( dict ) {\\r\\n    return dict.toList();\\r\\n  },\\r\\n  'is-dict': function( dict ) {\\r\\n    return Map.isMap( dict );\\r\\n  }\\r\\n};\\r\\n\",\"timestamp\":1568484216655},{\"key\":\"prewritten/string-dict-immutable.arr.json\",\"content\":\"{\\r\\n  \\\"requires\\\": [],\\r\\n  \\\"provides\\\": {\\r\\n    \\\"shorthands\\\": {\\r\\n      \\\"dOfA\\\": [\\\"tyapp\\\", [\\\"local\\\", \\\"StringDict\\\"], [[\\\"tid\\\", \\\"a\\\"]]],\\r\\n      \\\"dOfI\\\": [\\\"tyapp\\\", [\\\"local\\\", \\\"StringDict\\\"], [\\\"Number\\\"]],\\r\\n      \\\"lOfA\\\": [\\\"tyapp\\\", { \\\"tag\\\": \\\"name\\\",\\r\\n                \\\"origin\\\": { \\\"import-type\\\": \\\"uri\\\", \\\"uri\\\": \\\"builtin://list-immutable\\\" },\\r\\n                \\\"name\\\": \\\"List\\\" }, [[\\\"tid\\\", \\\"a\\\"]]],\\r\\n      \\\"lOfS\\\": [\\\"tyapp\\\", { \\\"tag\\\": \\\"name\\\",\\r\\n                \\\"origin\\\": { \\\"import-type\\\": \\\"uri\\\", \\\"uri\\\": \\\"builtin://list-immutable\\\" },\\r\\n                \\\"name\\\": \\\"List\\\" }, [\\\"String\\\"]],\\r\\n      \\\"tva\\\": [\\\"tid\\\", \\\"a\\\"],\\r\\n      \\\"tvb\\\": [\\\"tid\\\", \\\"b\\\"]\\r\\n    },\\r\\n    \\\"values\\\": {\\r\\n      \\\"string-dict\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"Maker\\\", \\\"tany\\\", \\\"dOfA\\\"]],\\r\\n      \\\"make-string-dict\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [], \\\"dOfA\\\"]],\\r\\n      \\\"count\\\": [\\\"arrow\\\", [\\\"lOfS\\\"], \\\"dOfI\\\"],\\r\\n      \\\"apply\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfS\\\", [\\\"arrow\\\", [\\\"String\\\"], \\\"tva\\\"]], \\\"dOfA\\\"]],\\r\\n      \\\"insert\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"dOfA\\\", \\\"String\\\", \\\"tva\\\"], \\\"dOfA\\\"]],\\r\\n      \\\"size\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"dOfA\\\"], \\\"Number\\\"]],\\r\\n      \\\"get\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"dOfA\\\", \\\"String\\\"], \\\"tva\\\"]],\\r\\n      \\\"has-key\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"dOfA\\\", \\\"String\\\"], \\\"Boolean\\\"]],\\r\\n      \\\"keys\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"dOfA\\\"], \\\"lOfA\\\"]],\\r\\n      \\\"values\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"dOfA\\\"], \\\"lOfA\\\"]],\\r\\n      \\\"is-dict\\\": [\\\"arrow\\\", [\\\"tany\\\"], \\\"Boolean\\\"]\\r\\n    },\\r\\n    \\\"aliases\\\": {\\r\\n      \\\"StringDict\\\": [\\\"local\\\", \\\"StringDict\\\"]\\r\\n    },\\r\\n    \\\"datatypes\\\": {\\r\\n      \\\"StringDict\\\": [\\\"data\\\", \\\"StringDict\\\", [\\\"a\\\"], [], {}]\\r\\n    }\\r\\n  }\\r\\n}\\r\\n\",\"timestamp\":1568484216705},{\"key\":\"prewritten/string-dict.arr.js\",\"content\":\"module.exports = {\\r\\n  'string-dict': {\\r\\n    'make': function( dict ) {\\r\\n      return dict[0];\\r\\n    }\\r\\n  },\\r\\n  'make-string-dict': function() {\\r\\n    return {};\\r\\n  },\\r\\n  'count': function( list ) {\\r\\n    dict = {};\\r\\n\\r\\n    for ( elm of list ) {\\r\\n      if ( !dict.hasOwnProperty( elm ) ) {\\r\\n        dict[elm] = 0;\\r\\n      }\\r\\n      \\r\\n      dict[elm] += 1;\\r\\n    }\\r\\n\\r\\n    return dict;\\r\\n  },\\r\\n  'apply': function( list, fun ) {\\r\\n    dict = {};\\r\\n\\r\\n    for ( elm of list ) {\\r\\n      dict[elm] = fun( elm );\\r\\n    }\\r\\n\\r\\n    return dict;\\r\\n  },\\r\\n  'insert': function( dict, key, value ) {\\r\\n    dict[key] = value;\\r\\n    return dict;\\r\\n  },\\r\\n  'size': function( dict ) {\\r\\n    return Object.keys( dict ).length;\\r\\n  },\\r\\n  'get': function( dict, elm ) {\\r\\n    return dict[elm];\\r\\n  },\\r\\n  'has-key': function( dict, elm ) {\\r\\n    return dict.hasOwnProperty( elm );\\r\\n  },\\r\\n  'keys': function( dict ) {\\r\\n    return Object.keys( dict );\\r\\n  },\\r\\n  'values': function( dict ) {\\r\\n    return Object.values( dict );\\r\\n  },\\r\\n  'is-dict': function( dict ) {\\r\\n    return dict.constructor === 'object';\\r\\n  }\\r\\n};\",\"timestamp\":1568484216644},{\"key\":\"prewritten/string-dict.arr.json\",\"content\":\"{\\r\\n  \\\"requires\\\": [],\\r\\n  \\\"provides\\\": {\\r\\n    \\\"shorthands\\\": {\\r\\n      \\\"dOfA\\\": [\\\"tyapp\\\", [\\\"local\\\", \\\"StringDict\\\"], [[\\\"tid\\\", \\\"a\\\"]]],\\r\\n      \\\"dOfI\\\": [\\\"tyapp\\\", [\\\"local\\\", \\\"StringDict\\\"], [\\\"Number\\\"]],\\r\\n      \\\"lOfA\\\": [\\\"tyapp\\\", { \\\"tag\\\": \\\"name\\\",\\r\\n                \\\"origin\\\": { \\\"import-type\\\": \\\"uri\\\", \\\"uri\\\": \\\"builtin://list\\\" },\\r\\n                \\\"name\\\": \\\"List\\\" }, [[\\\"tid\\\", \\\"a\\\"]]],\\r\\n      \\\"lOfS\\\": [\\\"tyapp\\\", { \\\"tag\\\": \\\"name\\\",\\r\\n                \\\"origin\\\": { \\\"import-type\\\": \\\"uri\\\", \\\"uri\\\": \\\"builtin://list\\\" },\\r\\n                \\\"name\\\": \\\"List\\\" }, [\\\"String\\\"]],\\r\\n      \\\"tva\\\": [\\\"tid\\\", \\\"a\\\"],\\r\\n      \\\"tvb\\\": [\\\"tid\\\", \\\"b\\\"]\\r\\n    },\\r\\n    \\\"values\\\": {\\r\\n      \\\"string-dict\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"Maker\\\", \\\"tany\\\", \\\"dOfA\\\"]],\\r\\n      \\\"make-string-dict\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [], \\\"dOfA\\\"]],\\r\\n      \\\"count\\\": [\\\"arrow\\\", [\\\"lOfS\\\"], \\\"dOfI\\\"],\\r\\n      \\\"apply\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"lOfS\\\", [\\\"arrow\\\", [\\\"String\\\"], \\\"tva\\\"]], \\\"dOfA\\\"]],\\r\\n      \\\"insert\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"dOfA\\\", \\\"String\\\", \\\"tva\\\"], \\\"dOfA\\\"]],\\r\\n      \\\"size\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"dOfA\\\"], \\\"Number\\\"]],\\r\\n      \\\"get\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"dOfA\\\", \\\"String\\\"], \\\"tva\\\"]],\\r\\n      \\\"has-key\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"dOfA\\\", \\\"String\\\"], \\\"Boolean\\\"]],\\r\\n      \\\"keys\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"dOfA\\\"], \\\"lOfS\\\"]],\\r\\n      \\\"values\\\": [\\\"forall\\\", [\\\"a\\\"], [\\\"arrow\\\", [\\\"dOfA\\\"], \\\"lOfA\\\"]],\\r\\n      \\\"is-dict\\\": [\\\"arrow\\\", [\\\"tany\\\"], \\\"Boolean\\\"]\\r\\n    },\\r\\n    \\\"aliases\\\": {\\r\\n      \\\"StringDict\\\": [\\\"local\\\", \\\"StringDict\\\"]\\r\\n    },\\r\\n    \\\"datatypes\\\": {\\r\\n      \\\"StringDict\\\": [\\\"data\\\", \\\"StringDict\\\", [\\\"a\\\"], [], {}]\\r\\n    }\\r\\n  }\\r\\n}\\r\\n\",\"timestamp\":1568484216702},{\"key\":\"prewritten/string.arr.js\",\"content\":\"module.exports = {\\r\\n  'length': function( str ) {\\r\\n    return str.length;\\r\\n  },\\r\\n  'string-to-lower': function( str ) {\\r\\n    return str.toLowerCase();\\r\\n  },\\r\\n  'concat': function( strA, strB ) {\\r\\n    return strA.concat( strB );\\r\\n  },\\r\\n  'substring': function( str, start, end ) {\\r\\n    return str.substring( start, end );\\r\\n  },\\r\\n  'charAt': function( str, index ) {\\r\\n    return str.charAt( index );\\r\\n  },\\r\\n  'split': function( str, separator ) {\\r\\n    return str.split( separator );\\r\\n  },\\r\\n  'split-pattern': function( str, pattern ) {\\r\\n    return str.split( RegExp( pattern ) );\\r\\n  },\\r\\n\\t'match': function( str, pattern ) {\\r\\n\\t\\treturn str.match( RegExp( pattern ) );\\r\\n\\t}\\r\\n};\",\"timestamp\":1568484216639},{\"key\":\"prewritten/string.arr.json\",\"content\":\"{\\r\\n  \\\"requires\\\": [],\\r\\n  \\\"provides\\\": {\\r\\n    \\\"values\\\": {\\r\\n      \\\"length\\\": [\\\"arrow\\\", [\\\"String\\\"], \\\"Number\\\"],\\r\\n      \\\"string-to-lower\\\": [\\\"arrow\\\", [\\\"String\\\"], \\\"String\\\"],\\r\\n      \\\"concat\\\": [\\\"arrow\\\", [\\\"String\\\", \\\"String\\\"], \\\"String\\\"],\\r\\n      \\\"substring\\\": [\\\"arrow\\\", [\\\"String\\\", \\\"Number\\\", \\\"Number\\\"], \\\"String\\\"],\\r\\n      \\\"charAt\\\": [\\\"arrow\\\", [\\\"String\\\", \\\"Number\\\"], \\\"String\\\"],\\r\\n      \\\"split\\\": [\\\"arrow\\\", [\\\"String\\\", \\\"String\\\"], \\\"tany\\\"],\\r\\n      \\\"split-pattern\\\": [\\\"arrow\\\", [\\\"String\\\", \\\"String\\\"], \\\"tany\\\"],\\r\\n\\t\\t\\t\\\"match\\\": [\\\"arrow\\\", [\\\"String\\\", \\\"String\\\"], \\\"tany\\\"]\\r\\n    }\\r\\n  }\\r\\n}\",\"timestamp\":1568484216699},{\"key\":\"prewritten/tables.arr.js\",\"content\":\"// @ts-ignore\\nvar PyretOption = require(\\\"./option.arr.js\\\");\\nvar List = require(\\\"./list.arr.js\\\");\\nvar parse = require(\\\"csv-parse/lib/sync\\\");\\nvar fs = require(\\\"fs\\\");\\nfunction _primitiveEqual(a1, a2) {\\n    if (a1 === a2) {\\n        return true;\\n    }\\n    if (a1 == null || a2 == null) {\\n        return false;\\n    }\\n    if (Array.isArray(a1) && Array.isArray(a2)) {\\n        return _primitiveArraysEqual(a1, a2);\\n    }\\n    if (a1.$brand === '$table' && a2.$brand === '$table') {\\n        return _primitiveTablesEqual(a1, a2);\\n    }\\n    if (a1.$brand === '$row' && a2.$brand === '$row') {\\n        return _primitiveRowsEqual(a1, a2);\\n    }\\n    return false;\\n}\\nfunction _primitiveRowsEqual(a1, a2) {\\n    if (a1.$brand !== '$row') {\\n        throw new Error(\\\"expected an object with the field '$brand': '$row',\\\"\\n            + \\\" but received \\\" + JSON.stringify(a1) + \\\" instead\\\");\\n    }\\n    if (a2.$brand !== '$row') {\\n        throw new Error(\\\"expected an object with the field '$brand': '$row',\\\"\\n            + \\\" but received \\\" + JSON.stringify(a2) + \\\" instead\\\");\\n    }\\n    if (!_primitiveEqual(a1._headers, a2._headers)) {\\n        return false;\\n    }\\n    if (!_primitiveEqual(a1._elements, a2._elements)) {\\n        return false;\\n    }\\n    return true;\\n}\\n// Returns true if a1 and a2 contain identical primitive values.\\nfunction _primitiveArraysEqual(a1, a2) {\\n    if (a1 === a2) {\\n        return true;\\n    }\\n    if (!Array.isArray(a1)) {\\n        throw new Error(\\\"found non-array object: \\\" + a1);\\n    }\\n    if (!Array.isArray(a2)) {\\n        throw new Error(\\\"found non-array object: \\\" + a2);\\n    }\\n    if (a1.length !== a2.length) {\\n        return false;\\n    }\\n    for (var i = 0; i < a1.length; i++) {\\n        if (!_primitiveEqual(a1[i], a2[i])) {\\n            return false;\\n        }\\n    }\\n    return true;\\n}\\nfunction _primitiveTablesEqual(t1, t2) {\\n    if (t1.$brand !== '$table') {\\n        throw new Error(\\\"expected an object with the field '$brand': '$table',\\\"\\n            + \\\" but received \\\" + JSON.stringify(t1) + \\\" instead\\\");\\n    }\\n    if (t2.$brand !== '$table') {\\n        throw new Error(\\\"expected an object with the field '$brand': '$table',\\\"\\n            + \\\" but received \\\" + JSON.stringify(t2) + \\\" instead\\\");\\n    }\\n    var t1_headers = t1._headers;\\n    var t2_headers = t2._headers;\\n    if (!_primitiveArraysEqual(t1_headers, t2_headers)) {\\n        return false;\\n    }\\n    var t1_rows = t1._rows;\\n    var t2_rows = t2._rows;\\n    if (t1_rows.length !== t2_rows.length) {\\n        return false;\\n    }\\n    for (var i = 0; i < t1_rows.length; i++) {\\n        if (!_primitiveEqual(t1_rows[i], t2_rows[i])) {\\n            return false;\\n        }\\n    }\\n    return true;\\n}\\nfunction getColumnNames(row) {\\n    return List.list.make(row._headers);\\n}\\nfunction getValue(row, columnName) {\\n    var columnIndex = row._headers.indexOf(columnName);\\n    if (columnIndex === -1) {\\n        throw new Error(\\\"get-value: column does not exist\\\");\\n    }\\n    return row._elements[columnIndex];\\n}\\nfunction rowGet(row, columnName) {\\n    var columnIndex = row._headers.indexOf(columnName);\\n    if (columnIndex === -1) {\\n        // @ts-ignore\\n        return PyretOption.none;\\n    }\\n    else {\\n        // @ts-ignore\\n        return PyretOption.some(row._elements[columnIndex]);\\n    }\\n}\\nfunction rawRow(elements) {\\n    var headers = [];\\n    var rowElements = [];\\n    for (var i = 0; i < elements.length; i++) {\\n        var _a = elements[i], h = _a[0], e = _a[1];\\n        headers.push(h);\\n        rowElements.push(e);\\n    }\\n    var result = {\\n        '_headers': headers,\\n        '_elements': rowElements,\\n        'get-column-names': function () { return getColumnNames(result); },\\n        'get-value': function (columnName) { return getValue(result, columnName); },\\n        'get': function (columnName) { return rowGet(result, columnName); },\\n        '$brand': '$row'\\n    };\\n    return result;\\n}\\nfunction zipWith(f, xs, ys) {\\n    if (xs.length !== ys.length) {\\n        throw new Error(\\\"can't zipWith arrays of different lengths\\\");\\n    }\\n    var result = [];\\n    for (var i = 0; i < xs.length; i++) {\\n        result.push(f(xs[i], ys[i]));\\n    }\\n    return result;\\n}\\nfunction zip(xs, ys) {\\n    return zipWith(function (x, y) { return [x, y]; }, xs, ys);\\n}\\nfunction _row(table) {\\n    var columns = [];\\n    for (var _i = 1; _i < arguments.length; _i++) {\\n        columns[_i - 1] = arguments[_i];\\n    }\\n    var elements = zip(table._headers, columns);\\n    return rawRow(elements);\\n}\\nfunction _buildColumn(table, columnName, computeNewVal) {\\n    if (table._headers.indexOf(columnName) !== -1) {\\n        throw new Error(\\\"duplicate column name: \\\" + columnName);\\n    }\\n    var headers = _deepCopy(table._headers);\\n    var newHeaders = headers.slice();\\n    newHeaders.push(columnName);\\n    var rows = _deepCopy(table._rows);\\n    rows.forEach(function (row) {\\n        row.push(computeNewVal(rawRow(zip(headers, row))));\\n    });\\n    return _makeTable(newHeaders, rows);\\n}\\nfunction _addColumn(table, columnName, newVals) {\\n    var headers = _deepCopy(table._headers);\\n    for (var i = 0; i < headers.length; i++) {\\n        if (headers[i] === columnName) {\\n            throw new Error(\\\"duplicate column name: \\\" + columnName);\\n        }\\n    }\\n    var rows = _deepCopy(table._rows);\\n    if (rows.length !== newVals.length) {\\n        throw new Error(\\\"length of new column is different than the length of the table\\\");\\n    }\\n    for (var i = 0; i < rows.length; i++) {\\n        rows[i].push(newVals[i]);\\n    }\\n    headers.push(columnName);\\n    return _makeTable(headers, rows);\\n}\\nfunction _addRow(table, row) {\\n    var tableHeaders = table._headers;\\n    var rowHeaders = row._headers;\\n    if (!_primitiveArraysEqual(tableHeaders, rowHeaders)) {\\n        throw new Error(\\\"table does not have the same column names as the new row\\\");\\n    }\\n    var tableRows = _deepCopy(table._rows);\\n    tableRows.push(row._elements);\\n    return _makeTable(tableHeaders, tableRows);\\n}\\nfunction _rowN(table, index) {\\n    if (index >= table._rows.length) {\\n        throw new Error(\\\"index \\\" + index + \\\" out of bounds in table rows\\\");\\n    }\\n    return rawRow(zip(table._headers, table._rows[index]));\\n}\\nfunction _columnN(table, index) {\\n    if (index >= table._headers.length) {\\n        throw new Error(\\\"index \\\" + index + \\\" out of bounds in table columns\\\");\\n    }\\n    return List.list.make(table._rows.map(function (row) { return row[index]; }));\\n}\\nfunction _columnNames(table) {\\n    return List.list.make(table._headers);\\n}\\nfunction _allRows(table) {\\n    return List.list.make(table._rows\\n        .map(function (row) {\\n        return rawRow(zip(table._headers, row));\\n    }));\\n}\\nfunction _allColumns(table) {\\n    var rows = table._rows;\\n    var headers = table._headers;\\n    var columns = headers.map(function (_) { return []; });\\n    for (var i = 0; i < columns.length; i++) {\\n        for (var j = 0; j < rows.length; j++) {\\n            columns[i].push(rows[j][i]);\\n        }\\n    }\\n    return columns;\\n}\\nfunction _makeTable(headers, rows) {\\n    var headerIndex = {};\\n    for (var i = 0; i < headers.length; i++) {\\n        headerIndex[\\\"column:\\\" + headers[i]] = i;\\n    }\\n    var table = {\\n        'add-column': function (columnName, newVals) { return _addColumn(table, columnName, newVals); },\\n        'add-row': function (row) { return _addRow(table, row); },\\n        'all-columns': function () { return _allColumns(table); },\\n        'all-rows': function () { return _allRows(table); },\\n        'build-column': function (columnName, computeNewVal) { return _buildColumn(table, columnName, computeNewVal); },\\n        'column-n': function (index) { return _columnN(table, index); },\\n        'column-names': function () { return _columnNames(table); },\\n        'decreasing-by': function (columnName) { return decreasingBy(table, columnName); },\\n        'drop': function (columnName) { return drop(table, columnName); },\\n        'empty': function () { return empty(table); },\\n        'filter': function (predicate) { return filter(table, predicate); },\\n        'filter-by': function (columnName, predicate) { return filterBy(table, columnName, predicate); },\\n        'get-column': function (columnName) { return getColumn(table, columnName); },\\n        'increasing-by': function (columnName) { return increasingBy(table, columnName); },\\n        'length': function () { return rows.length; },\\n        'order-by': function (columnName, asc) { return orderBy(table, columnName, asc); },\\n        'order-by-columns': function (columns) { return orderByColumns(table, columns); },\\n        'rename-column': function (oldName, newName) { return renameColumn(table, oldName, newName); },\\n        'row': function () {\\n            var columns = [];\\n            for (var _i = 0; _i < arguments.length; _i++) {\\n                columns[_i] = arguments[_i];\\n            }\\n            return _row.apply(void 0, [table].concat(columns));\\n        },\\n        'row-n': function (index) { return _rowN(table, index); },\\n        'select-columns': function (columnNames) { return _selectColumns(table, columnNames); },\\n        'stack': function (bot) { return stack(table, bot); },\\n        'transform-column': function (columnName, update) { return transformColumn(table, columnName, update); },\\n        '_headerIndex': headerIndex,\\n        '_headers': headers,\\n        '_rows': rows,\\n        '$brand': '$table'\\n    };\\n    return table;\\n}\\nfunction _tableSkeletonChangeHeaders(skeleton, newHeaders) {\\n    if (newHeaders.length !== skeleton.headers.length) {\\n        throw new Error(\\\"Expected \\\" + skeleton.headers.length + \\\" headers, but got \\\"\\n            + newHeaders.length + \\\" in \\\" + newHeaders);\\n    }\\n    return { headers: newHeaders, rows: skeleton.rows };\\n}\\nfunction _makeTableSkeletonFromCSVString(s) {\\n    var headers = [];\\n    var csv = parse(s, {\\n        columns: function (header) {\\n            return header.map(function (column) {\\n                headers.push(column);\\n                return column;\\n            });\\n        }\\n    });\\n    var rows = csv.map(function (row) {\\n        var result = [];\\n        for (var i = 0; i < headers.length; i++) {\\n            result.push(row[headers[i]]);\\n        }\\n        return result;\\n    });\\n    return { headers: headers, rows: rows };\\n}\\nfunction _makeTableSkeletonFromCSVFile(path) {\\n    var contents = fs.readFileSync(path, { encoding: \\\"utf-8\\\" });\\n    return _makeTableSkeletonFromCSVString(contents);\\n}\\nfunction _makeTableFromTableSkeleton(s) {\\n    return _makeTable(s.headers, s.rows);\\n}\\nfunction _makeTableFromCSVString(s) {\\n    var skeleton = _makeTableSkeletonFromCSVString(s);\\n    return _makeTableFromTableSkeleton(skeleton);\\n}\\nfunction _makeTableFromCSVFile(path) {\\n    var contents = fs.readFileSync(path, { encoding: \\\"utf-8\\\" });\\n    return _makeTableFromCSVString(contents);\\n}\\n// Changes the elements of a table in the specified column using the given function\\nfunction _transformColumnMutable(table, columnName, func) {\\n    if (!hasColumn(table, columnName)) {\\n        throw new Error(\\\"transformColumnMutable: tried changing the column \\\" + columnName + \\\" but it doesn't exist\\\");\\n    }\\n    // index of the column to change\\n    var i = table[\\\"_headerIndex\\\"]['column:' + columnName];\\n    table._rows.forEach(function (row) {\\n        return row[i] = func(row[i]);\\n    });\\n}\\n// Creates a new table and mutates the specified columns with the given functions\\nfunction _tableTransform(table, columnNames, updates) {\\n    var headers = table._headers;\\n    var newRows = _deepCopy(table._rows);\\n    var newTable = _makeTable(headers, newRows);\\n    for (var i = 0; i < columnNames.length; i++) {\\n        _transformColumnMutable(newTable, columnNames[i], updates[i]);\\n    }\\n    return newTable;\\n}\\n// transformColumn :: (Table, String, Function) -> Table\\n// Creates a new table that mutates the specified column for the given function\\nfunction transformColumn(table, columnName, update) {\\n    var headers = table._headers;\\n    var newRows = _deepCopy(table._rows);\\n    var newTable = _makeTable(headers, newRows);\\n    _transformColumnMutable(newTable, columnName, update);\\n    return newTable;\\n}\\n// returns a deep copy of (nested) arrays\\nfunction _deepCopy(arr) {\\n    var i, copy;\\n    if (Array.isArray(arr)) {\\n        copy = arr.slice(0);\\n        for (i = 0; i < copy.length; i++) {\\n            copy[i] = _deepCopy(copy[i]);\\n        }\\n        return copy;\\n    }\\n    else {\\n        return arr;\\n    }\\n}\\n// _tableFilter :: Table -> (Array -> Boolean) -> Table\\n// Creates a new Table which contains the rows from table that satisfy predicate.\\nfunction _tableFilter(table, predicate) {\\n    var headers = table._headers;\\n    var rows = table._rows;\\n    return _makeTable(headers, rows.filter(predicate));\\n}\\n// filter :: (Table, (Row -> Boolean)) -> Table\\n// creates a new table containing only the rows for which the predicate\\n// returned true\\nfunction filter(table, predicate) {\\n    var headers = table._headers;\\n    var rows = table._rows;\\n    var newRows = [];\\n    for (var i = 0; i < rows.length; i++) {\\n        if (predicate(_rowN(table, i))) {\\n            newRows.push(rows[i]);\\n        }\\n    }\\n    return _makeTable(headers, newRows);\\n}\\n// filter-by :: (Table, String, (Col -> Boolean)) -> Table\\n// creates a new table containing only the rows for which the predicate\\n// returned true for that column\\nfunction filterBy(table, columnName, predicate) {\\n    var headers = table._headers;\\n    var newRows = [];\\n    var column = getColumn(table, columnName);\\n    for (var i = 0; i < column.length; i++) {\\n        if (predicate(column[i])) {\\n            newRows.push(table._rows[i]);\\n        }\\n    }\\n    return _makeTable(headers, newRows);\\n}\\n// _tableGetColumnIndex :: Table -> String -> Integer\\n// Returns the index of columnName, or throws an error if columnName is not a\\n// column in table.\\nfunction _tableGetColumnIndex(table, columnName) {\\n    var headers = table._headers;\\n    for (var index = 0; index < headers.length; index++) {\\n        if (headers[index] === columnName) {\\n            return index;\\n        }\\n    }\\n    throw new Error(\\\"Column \\\" + columnName + \\\" is not valid\\\");\\n}\\n// Creates a Table that is like table, except that its rows are sorted according\\n// to columnOrders. An element in columnOrders specifies a column name and an\\n// ordering, either ascending or descending.\\nfunction _tableOrder(table, columnOrders) {\\n    var headers = table._headers;\\n    var rows = table._rows;\\n    function ordering(a, b) {\\n        for (var i = 0; i < columnOrders.length; i++) {\\n            var columnOrder = columnOrders[i];\\n            var name_1 = columnOrder[\\\"column\\\"];\\n            var order = columnOrder[\\\"direction\\\"];\\n            var index = _tableGetColumnIndex(table, name_1);\\n            var elemA = a[index];\\n            var elemB = b[index];\\n            if (order === \\\"ascending\\\") {\\n                if (elemA < elemB) {\\n                    return -1;\\n                }\\n                else if (elemA > elemB) {\\n                    return 1;\\n                }\\n            }\\n            else if (order === \\\"descending\\\") {\\n                if (elemA < elemB) {\\n                    return 1;\\n                }\\n                else if (elemA > elemB) {\\n                    return -1;\\n                }\\n            }\\n        }\\n        return 0;\\n    }\\n    // Array.prototype.sort() mutates the array it sorts, so we need to create a\\n    // copy with Array.prototype.slice() first.\\n    var sortedRows = rows.slice().sort(ordering);\\n    return _makeTable(headers, sortedRows);\\n}\\n// _selectColumns :: (Table, Array<String>) -> Table\\nfunction _selectColumns(table, columnNames) {\\n    //var colnamesList = ffi.toArray(columnNames);\\n    // This line of code below relies on anchor built-in lists being js arrays\\n    var colnamesList = columnNames;\\n    if (colnamesList.length === -1) {\\n        throw new Error(\\\"zero-columns\\\");\\n    }\\n    for (var i = 0; i < colnamesList.length; i += 1) {\\n        if (!hasColumn(table, colnamesList[i])) {\\n            throw new Error(\\\"no-such-column\\\");\\n        }\\n    }\\n    var newRows = [];\\n    for (var i = 0; i < table['_rows'].length; i += 1) {\\n        console.log(i);\\n        newRows[i] = [];\\n        for (var j = 0; j < colnamesList.length; j += 1) {\\n            var colIndex = _tableGetColumnIndex(table, colnamesList[j]);\\n            newRows[i].push(table['_rows'][i][colIndex]);\\n        }\\n    }\\n    return _makeTable(colnamesList, newRows);\\n}\\nfunction _tableExtractColumn(table, columnName) {\\n    // throws an error if columnName is not in table\\n    var index = _tableGetColumnIndex(table, columnName);\\n    var rows = table._rows;\\n    var extracted = List[\\\"empty-list\\\"]();\\n    for (var i = 0; i < rows.length; i++) {\\n        var element = rows[i][index];\\n        List.push(extracted, element);\\n    }\\n    return extracted;\\n}\\nvar runningSum = {\\n    one: function (element) { return [element, element]; },\\n    reduce: function (acc, element) {\\n        var sum = acc + element;\\n        return [sum, sum];\\n    }\\n};\\nfunction isReducingExtension(x) {\\n    return x.type === 'reduce';\\n}\\nfunction isMappingExtension(x) {\\n    return x.type === 'map';\\n}\\n// Creates a new Table that is like table, except that it has one or more new columns,\\n// as specified by the supplied TableExtensions.\\nfunction _tableReduce(table, extensions) {\\n    var headers = table['_headers'];\\n    var rows = table['_rows'];\\n    var extendedColumns = extensions.map(function (extension) { return extension.extending; });\\n    var newHeaders = headers.concat(extendedColumns);\\n    var newRows = rows.slice();\\n    var newTable = _makeTable(newHeaders, newRows);\\n    if (rows.length === 0) {\\n        return newTable;\\n    }\\n    var accumulators = [];\\n    for (var i = 0; i < extensions.length; i++) {\\n        var extension = extensions[i];\\n        if (isMappingExtension(extension)) {\\n            var mapping = extension;\\n            var row = newRows[0];\\n            var extending = mapping.extending;\\n            var extendingIndex = _tableGetColumnIndex(newTable, extending);\\n            row[extendingIndex] = mapping.reduce(0);\\n        }\\n        else if (isReducingExtension(extension)) {\\n            var reducing = extension;\\n            var row = newRows[0];\\n            var one = reducing.one;\\n            var using = reducing.using;\\n            var extending = reducing.extending;\\n            var usingIndex = _tableGetColumnIndex(newTable, using);\\n            var extendingIndex = _tableGetColumnIndex(newTable, extending);\\n            var _a = one(row[usingIndex]), acc = _a[0], outVal = _a[1];\\n            accumulators[i] = acc;\\n            row[extendingIndex] = outVal;\\n        }\\n        else {\\n            throw \\\"_tableReduce: extension is not a TableExtension\\\";\\n        }\\n    }\\n    for (var i = 1; i < newRows.length; i++) {\\n        for (var j = 0; j < extensions.length; j++) {\\n            var extension = extensions[j];\\n            if (isMappingExtension(extension)) {\\n                var mapping = extension;\\n                var row = newRows[i];\\n                var extending = mapping.extending;\\n                var extendingIndex = _tableGetColumnIndex(newTable, extending);\\n                row[extendingIndex] = mapping.reduce(i);\\n            }\\n            else if (isReducingExtension(extension)) {\\n                var reducing = extension;\\n                var row = newRows[i];\\n                var reduce = reducing.reduce;\\n                var using = reducing.using;\\n                var extending = reducing.extending;\\n                var usingIndex = _tableGetColumnIndex(newTable, using);\\n                var extendingIndex = _tableGetColumnIndex(newTable, extending);\\n                var _b = reduce(accumulators[j], row[usingIndex]), acc = _b[0], outVal = _b[1];\\n                accumulators[j] = acc;\\n                row[extendingIndex] = outVal;\\n            }\\n            else {\\n                throw \\\"_tableReduce: extension is not a TableExtension\\\";\\n            }\\n        }\\n    }\\n    return _makeTable(newHeaders, newRows);\\n}\\nfunction hasColumn(table, columnName) {\\n    return table._headers.includes(columnName);\\n}\\nfunction getColumn(table, columnName) {\\n    // Raise error if table lacks column\\n    if (!hasColumn(table, columnName)) {\\n        throw new Error(\\\"no such column: \\\" + columnName);\\n    }\\n    var columnIndex;\\n    Object.keys(table._headers).forEach(function (i) {\\n        if (table._headers[i] == columnName) {\\n            columnIndex = i;\\n        }\\n    });\\n    return table._rows.map(function (row) { return row[columnIndex]; });\\n}\\nfunction _length(table) {\\n    return table._rows.length;\\n}\\n// creates a new table with a column renamed\\nfunction renameColumn(table, oldName, newName) {\\n    // check if oldName exists\\n    if (!hasColumn(table, oldName)) {\\n        throw new Error(\\\"no such column to change: \\\" + oldName);\\n    }\\n    var newHeaders = _deepCopy(table._headers);\\n    var rows = table._rows;\\n    var colIndex = _tableGetColumnIndex(table, oldName);\\n    newHeaders[colIndex] = newName;\\n    var newTable = _makeTable(newHeaders, rows);\\n    return newTable;\\n}\\n// orders column in ascending order\\nfunction increasingBy(table, columnName) {\\n    // check if columnName exists\\n    if (!hasColumn(table, columnName)) {\\n        throw new Error(\\\"no such column\\\");\\n    }\\n    var headers = table._headers;\\n    var newRows = _deepCopy(table._rows);\\n    var colIndex = _tableGetColumnIndex(table, columnName);\\n    function ordering(a, b) {\\n        var elemA = a[colIndex];\\n        var elemB = b[colIndex];\\n        if (elemA < elemB) {\\n            return -1;\\n        }\\n        else if (elemA > elemB) {\\n            return 1;\\n        }\\n        return 0;\\n    }\\n    var sortedRows = newRows.slice().sort(ordering);\\n    var newTable = _makeTable(headers, sortedRows);\\n    return newTable;\\n}\\n// orders column in descending order\\nfunction decreasingBy(table, columnName) {\\n    // check if columnName exists\\n    if (!hasColumn(table, columnName)) {\\n        throw new Error(\\\"no such column\\\");\\n    }\\n    var newHeaders = table._headers;\\n    var newRows = _deepCopy(table._rows);\\n    var colIndex = _tableGetColumnIndex(table, columnName);\\n    function ordering(a, b) {\\n        var elemA = a[colIndex];\\n        var elemB = b[colIndex];\\n        if (elemA < elemB) {\\n            return 1;\\n        }\\n        else if (elemA > elemB) {\\n            return -1;\\n        }\\n        return 0;\\n    }\\n    var sortedRows = newRows.slice().sort(ordering);\\n    var newTable = _makeTable(newHeaders, sortedRows);\\n    return newTable;\\n}\\n// orders a column ascending or descending depending on the boolean\\nfunction orderBy(table, columnName, asc) {\\n    if (asc) {\\n        return increasingBy(table, columnName);\\n    }\\n    else {\\n        return decreasingBy(table, columnName);\\n    }\\n}\\nfunction orderByColumns(table, columns) {\\n    var headers = table._headers;\\n    var rows = table._rows;\\n    function ordering(a, b) {\\n        for (var i = 0; i < columns.length; i++) {\\n            var columnOrder = columns[i];\\n            var name_2 = columnOrder[0];\\n            var order = columnOrder[1];\\n            var index = _tableGetColumnIndex(table, name_2);\\n            var elemA = a[index];\\n            var elemB = b[index];\\n            if (order === true) {\\n                if (elemA < elemB) {\\n                    return -1;\\n                }\\n                else if (elemA > elemB) {\\n                    return 1;\\n                }\\n            }\\n            else if (order === false) {\\n                if (elemA < elemB) {\\n                    return 1;\\n                }\\n                else if (elemA > elemB) {\\n                    return -1;\\n                }\\n            }\\n        }\\n        return 0;\\n    }\\n    // Array.prototype.sort() mutates the array it sorts, so we need to create a\\n    // copy with Array.prototype.slice() first.\\n    var sortedRows = rows.slice().sort(ordering);\\n    return _makeTable(headers, sortedRows);\\n}\\n// returns an empty Table with the same column headers\\nfunction empty(table) {\\n    var headers = table._headers;\\n    var newTable = _makeTable(headers, []);\\n    return newTable;\\n}\\n// returns a new table without the specified column\\nfunction drop(table, columnName) {\\n    // check if columnName exists\\n    if (!hasColumn(table, columnName)) {\\n        throw new Error(\\\"no such column: \\\" + columnName);\\n    }\\n    var newHeaders = table._headers;\\n    var newRows = _deepCopy(table._rows);\\n    var columnIndex = _tableGetColumnIndex(table, columnName);\\n    newHeaders.splice(columnIndex, 1);\\n    for (var i = 0; i < newRows.length; i++) {\\n        newRows[i].splice(columnIndex, 1);\\n    }\\n    var newTable = _makeTable(newHeaders, newRows);\\n    return newTable;\\n}\\n// returns a new table with elements of both tables\\nfunction stack(table, bot) {\\n    var tableHeaders = table._headers;\\n    var headersToSort = _deepCopy(table._headers);\\n    var botHeaders = _deepCopy(bot._headers);\\n    if (!(_primitiveArraysEqual(headersToSort.sort(), botHeaders.sort()))) {\\n        throw new Error(\\\"headers do not match\\\");\\n    }\\n    var newRows = _deepCopy(table._rows);\\n    for (var i = 0; i < bot._rows.length; i++) {\\n        newRows.push([]);\\n        for (var j = 0; j < tableHeaders.length; j++) {\\n            newRows[newRows.length - 1]\\n                .push(bot._rows[i][_tableGetColumnIndex(bot, tableHeaders[j])]);\\n        }\\n    }\\n    var newTable = _makeTable(tableHeaders, newRows);\\n    return newTable;\\n}\\nfunction tableFromRows(rows) {\\n    if (rows.length === 0) {\\n        throw new Error(\\\"table-from-rows: expected one or more rows\\\");\\n    }\\n    var headers = rows.map(function (row) { return row._headers; });\\n    for (var i = 0; i < headers.length; i++) {\\n        if (!_primitiveArraysEqual(headers[i], headers[0])) {\\n            throw \\\"table-from-rows: row name mismatch\\\";\\n        }\\n    }\\n    var elements = rows.map(function (row) { return row._elements; });\\n    return _makeTable(headers[0], elements);\\n}\\nfunction tableFromColumns(columns) {\\n    if (columns.length === 0) {\\n        throw new Error(\\\"expected at least one column\\\");\\n    }\\n    var headers = columns.map(function (column) { return column[0]; });\\n    var sortedHeaders = headers.slice().sort();\\n    for (var i = 0; i < sortedHeaders.length - 1; i++) {\\n        if (sortedHeaders[i] === sortedHeaders[i + 1]) {\\n            throw new Error(\\\"duplicate header: \\\" + sortedHeaders[i]);\\n        }\\n    }\\n    var rowLength = columns[0][1].length;\\n    for (var i = 0; i < columns.length; i++) {\\n        if (columns[i][1].length !== rowLength) {\\n            throw new Error(\\\"columns must have the same number of elements\\\");\\n        }\\n    }\\n    var rows = columns[0][1].map(function () { return []; });\\n    for (var i = 0; i < columns.length; i++) {\\n        for (var j = 0; j < columns[i][1].length; j++) {\\n            rows[j].push(columns[i][1][j]);\\n        }\\n    }\\n    return _makeTable(headers, rows);\\n}\\nfunction tableFromColumn(columnName, values) {\\n    var col = [columnName, values];\\n    return tableFromColumns([col]);\\n}\\nmodule.exports = {\\n    '_makeTableSkeletonFromCSVString': _makeTableSkeletonFromCSVString,\\n    '_makeTableFromTableSkeleton': _makeTableFromTableSkeleton,\\n    '_tableSkeletonChangeHeaders': _tableSkeletonChangeHeaders,\\n    'csv-open': _makeTableSkeletonFromCSVFile,\\n    '_makeTableFromCSVFile': _makeTableFromCSVFile,\\n    '_makeTableFromCSVString': _makeTableFromCSVString,\\n    '_primitiveEqual': _primitiveEqual,\\n    'table-from-column': tableFromColumn,\\n    'table-from-columns': {\\n        'make': tableFromColumns\\n    },\\n    'table-from-rows': {\\n        'make': tableFromRows\\n    },\\n    'raw-row': {\\n        'make': rawRow\\n    },\\n    '_makeTable': _makeTable,\\n    '_tableFilter': _tableFilter,\\n    '_tableGetColumnIndex': _tableGetColumnIndex,\\n    '_tableOrder': _tableOrder,\\n    '_tableExtractColumn': _tableExtractColumn,\\n    '_tableTransform': _tableTransform,\\n    '_selectColumns': _selectColumns,\\n    'decreasing-by': decreasingBy,\\n    'drop': drop,\\n    'empty': empty,\\n    'filter': filter,\\n    'filter-by': filterBy,\\n    'get-column': getColumn,\\n    'increasing-by': increasingBy,\\n    '_length': _length,\\n    'order-by': orderBy,\\n    'order-by-columns': orderByColumns,\\n    'rename-column': renameColumn,\\n    'select-columns': _selectColumns,\\n    'stack': stack,\\n    'transform-column': transformColumn,\\n    '_tableReduce': _tableReduce,\\n    'running-sum': runningSum\\n};\\n\",\"timestamp\":1568484168329},{\"key\":\"prewritten/tables.arr.json\",\"content\":\"{\\n  \\\"requires\\\": [ ],\\n  \\\"provides\\\": {\\n    \\\"values\\\": {\\n      \\\"_makeTableSkeletonFromCSVString\\\": [\\\"arrow\\\", [\\\"String\\\"], \\\"Any\\\"],\\n      \\\"_makeTableFromTableSkeleton\\\": [\\\"arrow\\\", [\\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"_tableSkeletonChangeHeaders\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], \\\"Any\\\"],\\n      \\\"csv-open\\\": [\\\"arrow\\\", [\\\"String\\\"], \\\"Any\\\"],\\n      \\\"_makeTableFromCSVFile\\\": [\\\"arrow\\\", [\\\"String\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"_makeTableFromCSVString\\\": [\\\"arrow\\\", [\\\"String\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"_primitiveEqual\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], \\\"Boolean\\\"],\\n      \\\"table-from-column\\\": [\\\"arrow\\\", [\\\"String\\\", \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"table-from-columns\\\": \\\"Any\\\",\\n      \\\"table-from-rows\\\": \\\"Any\\\",\\n      \\\"raw-row\\\": [\\\"Maker\\\", \\\"Any\\\", [\\\"local\\\", \\\"Row\\\"]],\\n      \\\"_makeTable\\\":  [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"_tableFilter\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"_tableGetColumnIndex\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], \\\"Boolean\\\"],\\n      \\\"_tableOrder\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"_tableExtractColumn\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], \\\"Any\\\"],\\n      \\\"_tableTransform\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"_selectColumns\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"decreasing-by\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"drop\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"empty\\\": [\\\"arrow\\\", [\\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"filter\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"filter-by\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"String\\\", \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"get-column\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], \\\"Any\\\"],\\n      \\\"increasing-by\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"_length\\\": [\\\"arrow\\\", [\\\"Any\\\"], \\\"Number\\\"],\\n      \\\"order-by\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"order-by-columns\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"rename-column\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"_tableReduce\\\": [\\\"arrow\\\", [[\\\"local\\\", \\\"Table\\\"], \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"running-sum\\\": \\\"Any\\\",\\n      \\\"select-columns\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"stack\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]],\\n      \\\"transform-column\\\": [\\\"arrow\\\", [\\\"Any\\\", \\\"Any\\\", \\\"Any\\\"], [\\\"local\\\", \\\"Table\\\"]]\\n    },\\n    \\\"aliases\\\": {\\n      \\\"Row\\\": [\\\"local\\\", \\\"Row\\\"],\\n      \\\"Table\\\": [\\\"local\\\", \\\"Table\\\"]\\n    },\\n    \\\"datatypes\\\": {\\n      \\\"Row\\\": [\\\"data\\\", \\\"Row\\\", [], [], {}],\\n      \\\"Table\\\": [\\\"data\\\", \\\"Table\\\", [], [], {}]\\n    }\\n  }\\n}\\n\\n\",\"timestamp\":1568484216687},{\"key\":\"prewritten/unified.arr.js\",\"content\":\"var G2 = require(\\\"././global.arr.js\\\");\\nvar _global = require(\\\"././global.arr.js\\\");\\nvar _runtime = require(\\\"././runtime.js\\\");\\nvar _nothing = undefined;\\nvar nothing1 = G2.nothing;\\nreturn module[\\\"exports\\\"] = {\\\"$answer\\\":_global[\\\"trace-value\\\"](\\\"srcloc\\\",nothing1),\\n\\\"$checks\\\":_runtime[\\\"$checkResults\\\"]()};\\n\",\"timestamp\":1568484220123},{\"key\":\"prewritten/unified.arr.json\",\"content\":\"{\\\"theMap\\\":\\\"{\\\\\\\"version\\\\\\\":3,\\\\\\\"sources\\\\\\\":[\\\\\\\"file:///mnt/c/users/andro/Desktop/cs5152/pyret-lang/src/runtime-arr/unified.arr\\\\\\\"],\\\\\\\"names\\\\\\\":[\\\\\\\"file:///mnt/c/users/andro/Desktop/cs5152/pyret-lang/src/runtime-arr/unified.arr\\\\\\\"],\\\\\\\"mappings\\\\\\\":\\\\\\\"AAACA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA;AAAAA\\\\\\\",\\\\\\\"file\\\\\\\":\\\\\\\"file:///mnt/c/users/andro/Desktop/cs5152/pyret-lang/src/runtime-arr/unified.arr\\\\\\\"}\\\",\\n\\\"nativeRequires\\\":[],\\n\\\"provides\\\":{\\\"modules\\\":{},\\n\\\"values\\\":{},\\n\\\"datatypes\\\":{},\\n\\\"aliases\\\":{}},\\n\\\"requires\\\":[]}\",\"timestamp\":1568484220120}]");

/***/ }),

/***/ "./src/runtime-loader.ts":
/*!*******************************!*\
  !*** ./src/runtime-loader.ts ***!
  \*******************************/
/*! exports provided: load */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "load", function() { return load; });
const load = (fs, prewrittenDirectory, uncompiledDirectory, runtimeFiles) => {
  if (!fs.existsSync(prewrittenDirectory)) {
    fs.mkdirSync(prewrittenDirectory);
  }

  for (var index in runtimeFiles) {
    var path = runtimeFiles[index].key;
    var content = runtimeFiles[index].content;
    var canonicalTimestamp = runtimeFiles[index].timestamp;

    if (fs.existsSync(path)) {
      let statResult = fs.statSync(path);
      let localTimestamp = statResult.mtime.getTime();

      if (localTimestamp < canonicalTimestamp) {
        fs.writeFileSync(path, content);
      }
    } else {
      fs.writeFileSync(path, content);
    }
  }
};

/***/ }),

/***/ "./src/serviceWorker.js":
/*!******************************!*\
  !*** ./src/serviceWorker.js ***!
  \******************************/
/*! exports provided: register, unregister */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "register", function() { return register; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "unregister", function() { return unregister; });
// This optional code is used to register a service worker.
// register() is not called by default.
// This lets the app load faster on subsequent visits in production, and gives
// it offline capabilities. However, it also means that developers (and users)
// will only see deployed updates on subsequent visits to a page, after all the
// existing tabs open on the page have been closed, since previously cached
// resources are updated in the background.
// To learn more about the benefits of this model and instructions on how to
// opt-in, read https://bit.ly/CRA-PWA
const isLocalhost = Boolean(window.location.hostname === 'localhost' || // [::1] is the IPv6 localhost address.
window.location.hostname === '[::1]' || // 127.0.0.1/8 is considered localhost for IPv4.
window.location.hostname.match(/^127(?:\.(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}$/));
function register(config) {
  if (false) {}
}

function registerValidSW(swUrl, config) {
  navigator.serviceWorker.register(swUrl).then(registration => {
    registration.onupdatefound = () => {
      const installingWorker = registration.installing;

      if (installingWorker == null) {
        return;
      }

      installingWorker.onstatechange = () => {
        if (installingWorker.state === 'installed') {
          if (navigator.serviceWorker.controller) {
            // At this point, the updated precached content has been fetched,
            // but the previous service worker will still serve the older
            // content until all client tabs are closed.
            console.log('New content is available and will be used when all ' + 'tabs for this page are closed. See https://bit.ly/CRA-PWA.'); // Execute callback

            if (config && config.onUpdate) {
              config.onUpdate(registration);
            }
          } else {
            // At this point, everything has been precached.
            // It's the perfect time to display a
            // "Content is cached for offline use." message.
            console.log('Content is cached for offline use.'); // Execute callback

            if (config && config.onSuccess) {
              config.onSuccess(registration);
            }
          }
        }
      };
    };
  }).catch(error => {
    console.error('Error during service worker registration:', error);
  });
}

function checkValidServiceWorker(swUrl, config) {
  // Check if the service worker can be found. If it can't reload the page.
  fetch(swUrl).then(response => {
    // Ensure service worker exists, and that we really are getting a JS file.
    const contentType = response.headers.get('content-type');

    if (response.status === 404 || contentType != null && contentType.indexOf('javascript') === -1) {
      // No service worker found. Probably a different app. Reload the page.
      navigator.serviceWorker.ready.then(registration => {
        registration.unregister().then(() => {
          window.location.reload();
        });
      });
    } else {
      // Service worker found. Proceed as normal.
      registerValidSW(swUrl, config);
    }
  }).catch(() => {
    console.log('No internet connection found. App is running in offline mode.');
  });
}

function unregister() {
  if ('serviceWorker' in navigator) {
    navigator.serviceWorker.ready.then(registration => {
      registration.unregister();
    });
  }
}

/***/ }),

/***/ 0:
/*!***********************************************************************************!*\
  !*** multi ./node_modules/react-dev-utils/webpackHotDevClient.js ./src/index.tsx ***!
  \***********************************************************************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(/*! /mnt/c/users/andro/Desktop/cs5152/pyret-lang/ide/node_modules/react-dev-utils/webpackHotDevClient.js */"./node_modules/react-dev-utils/webpackHotDevClient.js");
module.exports = __webpack_require__(/*! /mnt/c/users/andro/Desktop/cs5152/pyret-lang/ide/src/index.tsx */"./src/index.tsx");


/***/ })

},[[0,"runtime~main",0]]]);
//# sourceMappingURL=main.chunk.js.map