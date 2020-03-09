import { combineReducers } from 'redux';
import * as control from './control';
import { EditorMode } from './Editor';
import { CompileState, makeResult } from './State';
import * as action from './action';
import { store } from './store';

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

export function ideApp(state = initialState, action: action.ideAction) {
  console.log(action);
  const changes = (() => {
    switch (action.type) {
      case "beginStartup":
        return dispatchCompileState(state.compileState, [
          {
            state: CompileState.Uninitialized,
            action: () => {
              return { compileState: CompileState.TextNeedsStartup };
            }
          }
        ]);
      case "startupCompleted":
        return dispatchCompileState(state.compileState, [
          {
            state: CompileState.TextNeedsStartup,
            action: () => {
              return { compileState: CompileState.TextStartup };
            }
          }
        ]);
      case "finishSetup":
        return dispatchCompileState(state.compileState, [
          {
            state: CompileState.TextStartup,
            action: () => {
              return { compileState: CompileState.TextReady };
            }
          },
          {
            state: CompileState.TextStartupQueue,
            action: () => {
              return { compileState: CompileState.TextReadyQueue };
            }
          },
          {
            state: CompileState.ChunkStartup,
            action: () => {
              return { compileState: CompileState.ChunkNeedsRepl };
            }
          },
          {
            state: CompileState.ChunkStartupQueue,
            action: () => {
              return { compileState: CompileState.ChunkNeedsReplQueue };
            }
          }
        ]);
      case "queueRun":
        return dispatchCompileState(state.compileState, [
          {
            state: CompileState.TextStartup,
            action: () => {
              return { compileState: CompileState.TextStartupQueue };
            }
          },
          {
            state: CompileState.TextStartupQueue,
            action: () => {
              return { compileState: CompileState.TextStartupQueue };
            }
          },
          {
            state: CompileState.ChunkStartup,
            action: () => {
              return { compileState: CompileState.ChunkStartupQueue };
            }
          },
          {
            state: CompileState.ChunkNeedsRepl,
            action: () => {
              return { compileState: CompileState.ChunkNeedsReplQueue };
            }
          },
          {
            state: CompileState.ChunkStartupQueue,
            action: () => {
              return { compileState: CompileState.ChunkStartupQueue };
            }
          },
          {
            state: CompileState.ChunkNeedsReplQueue,
            action: () => {
              return { compileState: CompileState.ChunkNeedsReplQueue };
            }
          }
        ]);
      case "finishCreateRepl":
        return dispatchCompileState(state.compileState, [
          {
            state: CompileState.ChunkNeedsRepl,
            action: () => {
              return { compileState: CompileState.ChunkReady };
            }
          },
          {
            state: CompileState.ChunkNeedsReplQueue,
            action: () => {
              return { compileState: CompileState.ChunkReadyQueue };
            }
          }
        ]);
      case "runText":
        return dispatchCompileState(state.compileState, [
          {
            state: CompileState.TextReady,
            action: () => {
              return { compileState: CompileState.TextReadyQueue };
            }
          },
          {
            state: CompileState.TextReadyQueue,
            action: () => {
              return { compileState: CompileState.TextReadyQueue };
            }
          }
        ]);
      case "finishRunText":
        return dispatchCompileState(state.compileState, [
          {
            state: CompileState.TextRunning,
            action: () => {
              return { compileState: CompileState.TextReady };
            }
          },
          {
            state: CompileState.TextRunningWithStops,
            action: () => {
              return { compileState: CompileState.TextReady };
            }
          },
          {
            state: CompileState.TextRunningQueue,
            action: () => {
              return { compileState: CompileState.TextReadyQueue };
            }
          },
          {
            state: CompileState.TextRunningWithStopsQueue,
            action: () => {
              return { compileState: CompileState.TextReadyQueue };
            }
          },
          {
            state: CompileState.TextRunningWithStopsNeedsStop,
            action: () => {
              return { compileState: CompileState.TextReady };
            }
          }
        ]);
      case "stopText":
        return dispatchCompileState(state.compileState, [
          {
            state: CompileState.TextRunningWithStops,
            action: () => {
              return { compileState: CompileState.TextRunningWithStopsNeedsStop };
            }
          }
        ]);
      case "textCompileQueue":
        return dispatchCompileState(state.compileState, [
          {
            state: CompileState.TextReadyQueue,
            action: () => {
              return { compileState: CompileState.TextCompileQueue };
            }
          }
        ]);
      case "textCompileFailure":
        return dispatchCompileState(state.compileState, [
          {
            state: CompileState.TextCompileQueue,
            action: () => {
              const places: any = [];
              for (let i = 0; i < action.errors.length; i++) {
                const matches = action.errors[i].match(/:\d+:\d+-\d+:\d+/g);
                if (matches !== null) {
                  matches.forEach((m) => {
                    places.push(m.match(/\d+/g)!.map(Number));
                  });
                }
              }
              return {
                compileState: CompileState.TextReady,
                interactionErrors: action.errors,
                definitionsHighlights: places
              };
            }
          }
        ]);
      case "textRunFailure":
        const result = (newState: CompileState) => () => {
          return {
            compileState: newState,
            interactionErrors: [action.errors.toString()]
          };
        };
        return dispatchCompileState(state.compileState, [
          {
            state: CompileState.TextRunning,
            action: result(CompileState.TextReady)
          },
          {
            state: CompileState.TextRunningWithStops,
            action: result(CompileState.TextReady)
          },
          {
            state: CompileState.TextRunningQueue,
            action: result(CompileState.TextReadyQueue)
          },
          {
            state: CompileState.TextRunningWithStopsQueue,
            action: result(CompileState.TextReadyQueue)
          },
          {
            state: CompileState.TextRunningWithStopsNeedsStop,
            action: result(CompileState.TextReady)
          }
        ]);
      case "textLintFailure":
        console.log("textLintFailure not yet implemented");
        return;
      case "textLintSuccess":
        console.log("textLintSuccess not yet implemented");
        return;
      case "textCompileSuccess":
        return dispatchCompileState(state.compileState, [
          {
            state: CompileState.TextCompileQueue,
            action: () => {
              return { compileState: CompileState.TextNeedsRun };
            }
          }
        ]);
      case "textRunFinished":
        const data = (() => {
          if (action.result !== undefined) {
            if (action.result.result.error === undefined) {
              const results =
                makeResult(
                  action.result.result,
                  "file:// " +
                    control.bfsSetup.path.join(
                      control.path.compileBase,
                      state.currentFileName));

              console.log(results);

              if (results[0] !== undefined && results[0].name === "error") {
                return {
                  interactions: results,
                  checks: action.result.result.$checks,
                  interactionErrors: action.result.result.error
                };
              }

              return {
                interactions: results,
                checks: action.result.result.$checks,
              };
            }

            return {
              interactionErrors: [action.result.result.error]
            };
          }

          return {};
        })();

        console.log("data", data);

        const makeAction = (newState: CompileState) => () => {
          return Object.assign({}, {compileState: newState}, data);
        }

        return dispatchCompileState(state.compileState, [
          {
            state: CompileState.TextRunningWithStops,
            action: makeAction(CompileState.TextReady)
          },
          {
            state: CompileState.TextRunningWithStopsQueue,
            action: makeAction(CompileState.TextReadyQueue)
          },
          {
            state: CompileState.TextRunningWithStopsNeedsStop,
            action: makeAction(CompileState.TextReady)
          },
          {
            state: CompileState.TextRunning,
            action: makeAction(CompileState.TextReady)
          },
          {
            state: CompileState.TextRunningQueue,
            action: makeAction(CompileState.TextReadyQueue)
          }
        ]);
      case "updateRunner":
        return { currentRunner: action.runner };
      case "textRunStarted":
        return dispatchCompileState(state.compileState, [
          {
            state: CompileState.TextNeedsRun,
            action: () => {
              return { compileState: CompileState.TextRunningWithStops };
            }
          }
        ]);
      case "textUpdateContents":
        console.log("changing contents ...");
        return { currentFileContents: action.contents };
      default:
        return {};
    }
  })();

  return Object.assign({}, state, changes);
}
