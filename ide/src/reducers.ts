import { combineReducers } from 'redux';
import * as control from './control';
import { EditorMode } from './Editor';
import { CompileState } from './State';
import * as action from './action';

const initialState = {
  browseRoot: "/",
  browsePath: ["/", "projects"],
  currentFileDirectory: ["/", "projects"],
  currentFileName: "program.arr",
  currentFileContents: control.openOrCreateFile("/projects/program.arr"),
  typeCheck: true,
  checks: [],
  interactions: [{
    key: "Note",
    name: "Note",
    value: "Press Run to compile and run"
  }],
  interactionErrors: [],
  lintFailures: {},
  runKind: control.backend.RunKind.Async,
  autoRun: true,
  updateTimer: setTimeout(() => { return; }, 0),
  dropdownVisible: false,
  editorMode: EditorMode.Chunks,
  fontSize: 12,
  message: "Ready to rock",
  definitionsHighlights: [],
  fsBrowserVisible: false,
  compileState: CompileState.Startup,
  currentRunner: undefined,
  currentChunk: 0,
};

export function ideApp(state = initialState, action: action.ideAction) {
  switch (action.type) {
    case "setEditorMode":
      return Object.assign({}, state, {
        editorMode: action.mode,
      });
    default:
      return state;
  }
}
