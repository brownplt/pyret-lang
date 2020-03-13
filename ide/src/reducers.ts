import * as action from './action';
import { CompileState, makeResult } from './State';
import { dispatchCompileState, on, onDispatch } from './dispatch';
import * as control from './control';
import { EditorMode } from './Editor';
import { Check } from './Check';
import { LintFailure } from './DefChunks';

export type ideAppState = {
  browseRoot: string,
  browsePath: string,
  currentFile: string | undefined,
  currentFileContents: string | undefined,
  typeCheck: boolean,
  checks: Check[],
  interactions: { key: any, name: any, value: any }[],
  interactionErrors: string[],
  lintFailures: {[name : string]: LintFailure},
  runKind: control.backend.RunKind,
  autoRun: boolean,
  updateTimer: NodeJS.Timer,
  dropdownVisible: boolean,
  editorMode: EditorMode,
  fontSize: number,
  message: string,
  definitionsHighlights: number[][],
  fsBrowserVisible: boolean,
  compileState: CompileState,
  currentRunner: any,
  currentChunk: number,
  needLoadFile: boolean,
  updateQueued: boolean,
  firstUpdatableChunk: undefined | number,
}

const initialState: ideAppState = {
  browseRoot: "/",
  browsePath: "/projects",
  currentFile: undefined,
  currentFileContents: undefined,
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
  compileState: CompileState.Uninitialized,
  currentRunner: undefined,
  currentChunk: 0,
  needLoadFile: false,
  updateQueued: false,
  firstUpdatableChunk: 0,
};

export function ideApp(state = initialState, action: action.ideAction): ideAppState {
  const newState = reducers
    .reduce(
      (state, r) => {
        return Object.assign({}, state, r(state, action));
      },
      state);

  return Object.assign({}, newState);
}

const reducers = [
  onDispatch("beginStartup", () => {
    return [{
      state: CompileState.Uninitialized,
      action: { compileState: CompileState.NeedsStartup }
    }];
  }),
  onDispatch("startupCompleted", [{
    state: CompileState.NeedsStartup,
    action: { compileState: CompileState.Startup }
  }]),
  onDispatch("finishSetup", [
    {
      state: CompileState.Startup,
      action: (state: any, action: any) => {
        if (state.editorMode === EditorMode.Chunks) {
          return { compileState: CompileState.ChunkNeedsRepl };
        } else {
          return { compileState: CompileState.Ready };
        }
      }
    }
  ]),
  on("queueRun", (state: any, action: any) => {
    return { updateQueued: true };
  }),
  onDispatch("finishCreateRepl", [
    {
      state: CompileState.ChunkNeedsRepl,
      action: { compileState: CompileState.Ready }
    }
  ]),
  onDispatch("finishRun", [
    {
      state: CompileState.Running,
      action: { compileState: CompileState.Ready }
    },
    {
      state: CompileState.RunningWithStops,
      action: { compileState: CompileState.Ready }
    },
    {
      state: CompileState.RunningWithStopsNeedsStop,
      action: { compileState: CompileState.Ready }
    }
  ]),
  onDispatch("stop", [
    {
      state: CompileState.RunningWithStops,
      action: { compileState: CompileState.RunningWithStopsNeedsStop }
    }
  ]),
  onDispatch("compile", [
    {
      state: CompileState.Ready,
      action: { compileState: CompileState.Compile, updateQueued: false }
    }
  ]),
  onDispatch("compileFailure", [
    {
      state: CompileState.Compile,
      action: (state: any, action: any) => {
        const places: any = [];
        for (let i = 0; i < action.errors.length; i++) {
          const matches = action.errors[i].match(/:\d+:\d+-\d+:\d+/g);
          if (matches !== null) {
            matches.forEach((m: any) => {
              places.push(m.match(/\d+/g)!.map(Number));
            });
          }
        }
        return {
          compileState: CompileState.Ready,
          interactionErrors: action.errors,
          definitionsHighlights: places
        };
      }
    }
  ]),
  onDispatch("runFailure", (() => {
    function makeResult(newState: CompileState) {
      return (state: any, action: any) => ({
        compileState: newState,
        interactionErrors: [action.errors.toString()]
      })
    }
    return [
      {
        state: CompileState.Running,
        action: makeResult(CompileState.Ready)
      },
      {
        state: CompileState.RunningWithStops,
        action: makeResult(CompileState.Ready)
      },
      {
        state: CompileState.RunningWithStopsNeedsStop,
        action: makeResult(CompileState.Ready)
      },
      {
        state: CompileState.Compile, // TODO how does this happen?
        action: makeResult(CompileState.Compile)
      },
    ];
  })()),
  on("lintFailure", () => {
    console.log("lintFailure not yet implemented");
    return {};
  }),
  on("lintSuccess", () => {
    console.log("lintSucccess not yet implemented");
    return {};
  }),
  onDispatch("compileSuccess", [
    {
      state: CompileState.Compile,
      action: (state: any, action: any) => {
        const newCompileState = state.updateQueued ?
          CompileState.Ready : CompileState.NeedsRun;
        return {
          compileState: newCompileState,
          interactionErrors: [],
          definitionsHighlights: []
        }
      }
    }
  ]),
  on("runFinished", (state: any, action: any) => {
    const data = (() => {
      if (action.result !== undefined
          && action.result.result.error === undefined
          && state.currentFile === undefined) {
        throw new Error("state.currentFile should not be undefined");
      } else if (action.result !== undefined
          && action.result.result.error === undefined) {

        const results =
          makeResult(action.result.result, "file:// " + state.currentFile);

        if (results[0] !== undefined
            && results[0].name === "error") {
          return {
            interactions: results,
            checks: action.result.result.$checks,
            interactionErrors: action.result.result.error
          };
        } else {
          return {
            interactions: results,
            checks: action.result.result.$checks,
          };
        }
      } else if (action.result !== undefined) {
        return {
          interactionErrors: [action.result.result.error]
        };
      } else {
        return {};
      }
    })();

    const makeAction = (newState: CompileState) => () => {
      return Object.assign({}, {compileState: newState}, data);
    }

    return dispatchCompileState("runFinished", state, action, [
      {
        state: CompileState.RunningWithStops,
        action: makeAction(CompileState.Ready)
      },
      {
        state: CompileState.RunningWithStopsNeedsStop,
        action: makeAction(CompileState.Ready)
      },
      {
        state: CompileState.Running,
        action: makeAction(CompileState.Ready)
      },
    ]);
  }),
  onDispatch("runStarted", [
    {
      state: CompileState.NeedsRun,
      action: { compileState: CompileState.RunningWithStops }
    }
  ]),
  on("updateContents", (state: any, action: any) => ({
    currentFileContents: action.contents,
    needLoadFile: false,
    updateQueued: state.autoRun
  })),
  on("updateChunkContents", (state: any, action: any) => ({
    currentFileContents: action.contents,
    needLoadFile: false,
    updateQueued: state.autoRun,
    firstUpdatableChunk: action.index
  })),
  on("traverseUp", (state: any, action: any) => {
    return { browsePath: action.path };
  }),
  on("traverseDown", (state: any, action: any) => {
    return { browsePath: action.path };
  }),
  on("expandChild", (state: any, action: any) => {
    return {
      currentFile: action.path,
      needLoadFile: true
    };
  }),
  on("setEditorMode", (state: any, action: any) => {
    return {
      editorMode: action.mode,
    }
  })
];
