import * as action from './action';
import { CompileState, EditorMode, makeResult, ideAppState, initialState } from './State';
import { applyMatchingStateUpdate, guard, guardUpdates } from './dispatch';
import * as control from './control';
import { Check } from './Check';
import { LintFailure } from './DefChunks';

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
      change: (state: any, action: any) => {
        if (state.editorMode === EditorMode.Chunks) {
          return { compileState: CompileState.ChunkNeedsRepl };
        } else {
          return { compileState: CompileState.Ready };
        }
      }
    }
  ]),
  guard("queueRun", (state: any, action: any) => {
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
      change: (state: any, action: any) => {
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
      return (state: any, action: any) => ({
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
      change: (state: any, action: any) => {
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
  guard("runFinished", (state: any, action: any) => {
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

    return applyMatchingStateUpdate("runFinished", state, action, [
      {
        state: CompileState.RunningWithStops,
        change: makeAction(CompileState.Ready)
      },
      {
        state: CompileState.RunningWithStopsNeedsStop,
        change: makeAction(CompileState.Ready)
      },
      {
        state: CompileState.Running,
        change: makeAction(CompileState.Ready)
      },
    ]);
  }),
  guardUpdates("runStarted", [
    {
      state: CompileState.NeedsRun,
      change: { compileState: CompileState.RunningWithStops }
    }
  ]),
  guard("updateContents", (state: any, action: any) => ({
    currentFileContents: action.contents,
    needLoadFile: false,
    updateQueued: state.autoRun
  })),
  guard("updateChunkContents", (state: any, action: any) => ({
    currentFileContents: action.contents,
    needLoadFile: false,
    updateQueued: state.autoRun,
    firstUpdatableChunk: action.index
  })),
  guard("traverseUp", (state: any, action: any) => {
    return { browsePath: action.path };
  }),
  guard("traverseDown", (state: any, action: any) => {
    return { browsePath: action.path };
  }),
  guard("expandChild", (state: any, action: any) => {
    return {
      currentFile: action.path,
      needLoadFile: true
    };
  }),
  guard("setEditorMode", (state: any, action: any) => {
    return {
      editorMode: action.mode,
    }
  })
];
