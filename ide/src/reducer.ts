import { Action, isActionType, ActionOfType } from './action';
import { CompileState, EditorMode, makeResult, State, initialState } from './state';
import {
  applyMatchingStateUpdate,
  guard,
  guardUpdates,
  SemiReducer,
  combineSemiReducers,
  PartialState
} from './semiReducer';

const semiReducers = [
  guardUpdates("beginStartup", [{
    state: CompileState.Uninitialized,
    change: { compileState: CompileState.NeedsStartup }
  }]),
  guardUpdates("startupCompleted", [{
    state: CompileState.NeedsStartup,
    change: { compileState: CompileState.Startup }
  }]),
  guardUpdates("finishSetup", [
    {
      state: CompileState.Startup,
      change: (state, action): PartialState => {
        if (state.editorMode === EditorMode.Chunks) {
          return { compileState: CompileState.ChunkNeedsRepl };
        } else {
          return { compileState: CompileState.Ready };
        }
      }
    }
  ]),
  guard("queueRun", (state, action) => {
    return { updateQueued: true };
  }),
  guardUpdates("finishCreateRepl", [
    {
      state: CompileState.ChunkNeedsRepl,
      change: { compileState: CompileState.Ready }
    }
  ]),
  guardUpdates("finishRun", [
    {
      state: CompileState.Running,
      change: { compileState: CompileState.Ready }
    },
    {
      state: CompileState.RunningWithStops,
      change: { compileState: CompileState.Ready }
    },
    {
      state: CompileState.RunningWithStopsNeedsStop,
      change: { compileState: CompileState.Ready }
    }
  ]),
  guardUpdates("stop", [
    {
      state: CompileState.RunningWithStops,
      change: { compileState: CompileState.RunningWithStopsNeedsStop }
    }
  ]),
  guardUpdates("compile", [
    {
      state: CompileState.Ready,
      change: { compileState: CompileState.Compile, updateQueued: false }
    }
  ]),
  guardUpdates("compileFailure", [
    {
      state: CompileState.Compile,
      change: (state, action): PartialState => {
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
  guardUpdates("runFailure", (() => {
    function makeResult(newState: CompileState) {
      return (state: State, action: ActionOfType<"runFailure">): PartialState => ({
        compileState: newState,
        interactionErrors: [action.errors.toString()]
      })
    }
    return [
      {
        state: CompileState.Running,
        change: makeResult(CompileState.Ready)
      },
      {
        state: CompileState.RunningWithStops,
        change: makeResult(CompileState.Ready)
      },
      {
        state: CompileState.RunningWithStopsNeedsStop,
        change: makeResult(CompileState.Ready)
      },
      {
        state: CompileState.Compile, // TODO how does this happen?
        change: makeResult(CompileState.Compile)
      },
    ];
  })()),
  guard("lintFailure", () => {
    console.log("lintFailure not yet implemented");
    return {};
  }),
  guard("lintSuccess", () => {
    console.log("lintSucccess not yet implemented");
    return {};
  }),
  guardUpdates("compileSuccess", [
    {
      state: CompileState.Compile,
      change: (state, action): PartialState => {
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
  guard("runFinished", (state, action) => {
    function makeData(): PartialState {
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
    }

    const data = makeData();

    const makeAction = (newState: CompileState) => () => {
      return Object.assign({}, {compileState: newState}, data);
    }

    const readyAction = makeAction(CompileState.Ready);

    return applyMatchingStateUpdate("runFinished", state, action, [
      {
        state: CompileState.RunningWithStops,
        change: readyAction
      },
      {
        state: CompileState.RunningWithStopsNeedsStop,
        change: readyAction
      },
      {
        state: CompileState.Running,
        change: readyAction
      },
    ]);
  }),
  guardUpdates("runStarted", [
    {
      state: CompileState.NeedsRun,
      change: { compileState: CompileState.RunningWithStops }
    }
  ]),
  guard("updateContents", (state, action): PartialState => ({
    currentFileContents: action.contents,
    needLoadFile: false,
    updateQueued: state.autoRun,
  })),
  guard("updateChunkContents", (state, action): PartialState => ({
    currentFileContents: action.contents,
    needLoadFile: false,
    updateQueued: state.autoRun,
    firstUpdatableChunk: action.index
  })),
  guard("traverseUp", (state, action): PartialState => {
    return { browsePath: action.path };
  }),
  guard("traverseDown", (state, action): PartialState => {
    return { browsePath: action.path };
  }),
  guard("expandChild", (state, action): PartialState => {
    return {
      currentFile: action.path,
      needLoadFile: true
    };
  }),
  guard("setEditorMode", (state, action): PartialState => {
    return {
      editorMode: action.mode,
    }
  })
];

const rootReducer = combineSemiReducers(semiReducers);

export function ideApp(state = initialState, action: Action): State {
  return Object.assign({}, rootReducer(state, action));
}
