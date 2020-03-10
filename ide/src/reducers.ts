import { combineReducers } from 'redux';
import * as control from './control';
import { EditorMode } from './Editor';
import { CompileState, makeResult } from './State';
import * as action from './action';
import { store } from './store';
import * as reducers from './reducersImpl'

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
  editorMode: EditorMode.Text,
  fontSize: 12,
  message: "Ready to rock",
  definitionsHighlights: [],
  fsBrowserVisible: false,
  compileState: CompileState.Uninitialized,
  currentRunner: undefined,
  currentChunk: 0,
  needLoadFile: false,
};

function dispatchCompileState<a>(compileState: CompileState,
                                 actions: {state: CompileState, action: () => a}[]) {
  for (let i = 0; i < actions.length; i++) {
    if (actions[i].state === compileState) {
      console.log(`dispatching state ${CompileState[compileState]}`);
      return actions[i].action();
    }
  }

  throw new Error(`dispatchCompileState: no action for state ${CompileState[compileState]}`);
}

const reducersArray = Object.getOwnPropertyNames(reducers)
  .filter((key) => key.indexOf("__") !== 0)
  .map((key) => (reducers as any)[key]);

export function ideApp(state = initialState, action: action.ideAction) {
  const newState = reducersArray
    .reduce(
      (state, r) => {
        return Object.assign({}, state, r(state, action));
      },
      state);

  return Object.assign({}, newState);
}
