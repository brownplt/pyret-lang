/* Handles side effects. */

import { createStore } from 'redux';
import ideApp from './reducer';
import { IDE } from './ide';
import {
  BackendCmd,
  EditorMode,
  MessageTabIndex,
  State,
} from './state';
import {
  Chunk,
  makeChunksFromString,
  CHUNKSEP,
} from './chunk';
import { Action } from './action';
import { RunKind } from './backend';
import { Effect } from './effect';
import * as control from './control';
import * as ideRt from './ide-rt-override';

type Dispatch = (action: Action) => void;

/* Tracks the current runner of a running program that was compiled with
   Stopify. Used for stopping the program when the user hits the "stop" button. */
let currentRunner: any;

function handleStartEditTimer(dispatch: Dispatch, editTimer: NodeJS.Timer | false) {
  if (editTimer) {
    clearTimeout(editTimer);
  }

  dispatch({
    type: 'effectEnded',
    status: 'succeeded',
    effectKey: 'startEditTimer',
    timer: setTimeout(() => {
      dispatch({
        type: 'effectEnded',
        status: 'succeeded',
        effectKey: 'editTimer',
      });
    }, 200),
  });
}

function handleLoadFile(
  dispatch: Dispatch,
  currentFile: string,
  editorMode: EditorMode,
) {
  const contents = control.openOrCreateFile(currentFile);

  switch (editorMode) {
    case EditorMode.Text:
      dispatch({ type: 'update', key: 'currentFileContents', value: contents });
      break;
    case EditorMode.Chunks: {
      const chunks = makeChunksFromString(contents);

      dispatch({
        type: 'update',
        key: 'chunks',
        value: {
          chunks,
          modifiesText: true,
        },
      });

      break;
    }
    default:
  }

  dispatch({
    type: 'effectEnded',
    status: 'succeeded',
    effectKey: 'loadFile',
  });
}

function handleSetupWorkerMessageHandler(dispatch: Dispatch) {
  function handleLog(message: string): void {
    console.log(message);
  }

  function handleSetupFinished(): void {
    dispatch({
      type: 'effectEnded',
      status: 'succeeded',
      effectKey: 'setup',
    });
  }

  function handleCompileFailure(errors: string[]): void {
    dispatch({
      type: 'update',
      key: 'messageTabIndex',
      value: MessageTabIndex.ErrorMessages,
    });

    dispatch({
      type: 'effectEnded',
      status: 'failed',
      effectKey: 'compile',
      errors,
    });
  }

  function handleRuntimeFailure(errors: string[]): void {
    dispatch({
      type: 'update',
      key: 'messageTabIndex',
      value: MessageTabIndex.ErrorMessages,
    });

    dispatch({
      type: 'effectEnded',
      status: 'failed',
      effectKey: 'run',
      errors,
    });
  }

  function handleLintFailure(lintFailure: { name: string, errors: string[] }): void {
    dispatch({
      type: 'update',
      key: 'messageTabIndex',
      value: MessageTabIndex.ErrorMessages,
    });

    dispatch({
      type: 'effectEnded',
      status: 'failed',
      effectKey: 'lint',
      name: lintFailure.name,
      errors: lintFailure.errors,
    });
  }

  function handleLintSuccess(lintSuccess: { name: string }): void {
    dispatch({
      type: 'effectEnded',
      status: 'succeeded',
      effectKey: 'lint',
      name: lintSuccess.name,
    });
  }

  function handleCompileSuccess(): void {
    dispatch({
      type: 'effectEnded',
      status: 'succeeded',
      effectKey: 'compile',
    });
  }

  function handleCreateReplSuccess(): void {
    dispatch({
      type: 'effectEnded',
      status: 'succeeded',
      effectKey: 'createRepl',
    });
  }

  function handleCompileInteractionSuccess(): void {
    console.log('compile interaction success (nyi)');
  }

  function handleCompileInteractionFailure(): void {
    console.log('compile interaction failure (nyi)');
  }

  control.setupWorkerMessageHandler(
    handleLog,
    handleSetupFinished,
    handleCompileFailure,
    handleRuntimeFailure,
    handleLintFailure,
    handleLintSuccess,
    handleCompileSuccess,
    handleCreateReplSuccess,
    handleCompileInteractionSuccess,
    handleCompileInteractionFailure,
  );

  dispatch({
    type: 'effectEnded',
    status: 'succeeded',
    effectKey: 'setupWorkerMessageHandler',
  });
}

function handleCreateRepl() {
  control.createRepl();
}

function handleSaveFile(
  dispatch: Dispatch,
  mode: EditorMode,
  path: string,
  contents: string,
  chunks: Chunk[],
) {
  switch (mode) {
    case EditorMode.Text:
      control.fs.writeFileSync(path, contents);
      break;
    case EditorMode.Chunks:
      control.fs.writeFileSync(
        path,
        chunks.map((chunk) => chunk.text).join(CHUNKSEP),
      );
      break;
    default:
      throw new Error('handleSaveFile: unknown editor mode');
  }

  dispatch({
    type: 'effectEnded',
    status: 'succeeded',
    effectKey: 'saveFile',
  });
}

function handleCompile(dispatch: Dispatch, path: string, typeCheck: boolean) {
  const { dir, base } = control.bfsSetup.path.parse(path);
  control.compile(dir, base, typeCheck);
}

function handleRun(dispatch: Dispatch, currentFile: string, runKind: RunKind) {
  const { base } = control.bfsSetup.path.parse(currentFile);
  // TODO(alex): Maybe clear messages when compilation starts
  dispatch({
    type: 'update',
    key: 'rhs',
    value: 'reset-rt-messages',
  });
  control.run(
    control.path.runBase,
    `${base}.js`,
    (runResult: any) => {
      if (runResult.result.error === undefined) {
        dispatch({
          type: 'update',
          key: 'messageTabIndex',
          value: MessageTabIndex.RuntimeMessages,
        });

        dispatch({
          type: 'effectEnded',
          status: 'succeeded',
          effectKey: 'run',
          result: runResult,
        });
      } else {
        dispatch({
          type: 'update',
          key: 'messageTabIndex',
          value: MessageTabIndex.ErrorMessages,
        });

        dispatch({
          type: 'effectEnded',
          status: 'failed',
          effectKey: 'run',
          errors: runResult.result.result,
        });
      }
    },
    (runner: any) => {
      currentRunner = runner;
    },
    runKind,
    {
      spyMessgeHandler: ideRt.defaultSpyMessage,
      spyExprHandler: ideRt.defaultSpyExpr,
      imgUrlProxy: ideRt.defaultImageUrlProxy,
      checkBlockRunner: ideRt.noCheckBlocks,
    },
  );
}

function handleStop(dispatch: Dispatch) {
  currentRunner.pause((line: number) => {
    dispatch({
      type: 'effectEnded',
      status: 'succeeded',
      effectKey: 'stop',
      line,
    });
  });
}

function handleTextLint(currentFileContents: string): void {
  control.lint(currentFileContents, 'text');
}

function handleChunkLint(text: string, id: string): void {
  control.lint(text, id);
}

// Removes consecutive, duplicate effects, so we don't do extra work.
function collapseEffectQueue(effectQueue: Effect[]): Effect[] {
  const collapsedEffectQueue: Effect[] = [];
  let lastElement: undefined | Effect;

  effectQueue.forEach((effect) => {
    if (lastElement !== effect) {
      collapsedEffectQueue.push(effect);
      lastElement = effect;
    }
  });

  return collapsedEffectQueue;
}

/* Picks the first effect in the effect queue that can be applied. Returns an
   object containing the selected index and a function that applies the side
   effect. */
function handleFirstActionableEffect(
  state: State,
  dispatch: Dispatch,
): false | { effect: number, applyEffect: () => void } {
  const { effectQueue } = state;

  for (let i = 0; i < effectQueue.length; i += 1) {
    const effect = effectQueue[i];

    switch (effect.effectKey) {
      case 'startEditTimer': {
        const { editTimer } = state;
        return {
          effect: i,
          applyEffect: () => handleStartEditTimer(dispatch, editTimer),
        };
      }
      case 'loadFile':
        {
          console.log('loadFile');
          const { currentFile, editorMode } = state;
          if (currentFile !== undefined) {
            return {
              effect: i,
              applyEffect: () => handleLoadFile(dispatch, currentFile, editorMode),
            };
          }
        }
        break;
      case 'saveFile':
        {
          const {
            editorMode, currentFile, currentFileContents, chunks,
          } = state;
          if (currentFile !== undefined && currentFileContents !== undefined) {
            return {
              effect: i,
              applyEffect: () => handleSaveFile(
                dispatch,
                editorMode,
                currentFile,
                currentFileContents,
                chunks,
              ),
            };
          }
        }
        break;
      case 'setupWorkerMessageHandler':
        {
          console.log('setupWorkerMessageHandler');
          const { isMessageHandlerReady } = state;
          if (!isMessageHandlerReady) {
            return {
              effect: i,
              applyEffect: () => handleSetupWorkerMessageHandler(dispatch),
            };
          }
        }
        break;
      case 'createRepl':
        {
          console.log('createRepl');
          const { isReplReady } = state;
          if (!isReplReady) {
            return {
              effect: i,
              applyEffect: () => handleCreateRepl(),
            };
          }
        }
        break;
      case 'initCmd':
      {
        const currBackendCmd = state.backendCmd;
        const initBackendCmd = effect.cmd;

        console.log(`Init cmd: ${initBackendCmd}`);
        console.log(`Curr cmd: ${currBackendCmd}`);

        // Backend already busy
        if (currBackendCmd !== BackendCmd.None) {
          return {
            effect: i,
            applyEffect: () => {
              dispatch({
                type: 'effectEnded',
                status: 'failed',
                effectKey: 'initCmd',
              });
            },
          };
        }

        return {
          effect: i,
          applyEffect: () => {
            dispatch({
              type: 'update',
              key: 'backendCmd',
              value: initBackendCmd,
            });

            dispatch({
              type: 'enqueueEffect',
              effect: { effectKey: 'saveFile' },
            });

            dispatch({
              type: 'effectEnded',
              status: 'succeeded',
              effectKey: 'initCmd',
            });
          },
        };
      }
      case 'lint': {
        const {
          editorMode,
          chunks,
          currentFileContents,
          isSetupFinished,
          isFileSaved,
        } = state;

        if (isSetupFinished && isFileSaved) {
          if (currentFileContents !== undefined && editorMode === EditorMode.Text) {
            return {
              effect: i,
              applyEffect: () => handleTextLint(currentFileContents),
            };
          }

          if (editorMode === EditorMode.Chunks) {
            const sendLintRequests = (): void => {
              chunks.forEach(({ text, errorState, id }) => {
                if (errorState.status !== 'succeeded') {
                  console.log(`linting chunk ${id}`);
                  handleChunkLint(text, id);
                }
              });
            };

            return {
              effect: i,
              applyEffect: sendLintRequests,
            };
          }
        }

        break;
      }
      case 'compile':
        {
          console.log('compile');
          const {
            currentFile,
            typeCheck,
            isMessageHandlerReady,
            isSetupFinished,
            compiling,
            running,
            isFileSaved,
            chunks,
            editorMode,
          } = state;
          let allLinted = true;

          if (editorMode === EditorMode.Chunks) {
            for (let j = 0; j < chunks.length; j += 1) {
              if (chunks[j].errorState.status !== 'succeeded') {
                allLinted = false;
                break;
              }
            }
          }

          if (isMessageHandlerReady
              && isSetupFinished
              && isFileSaved
              && compiling !== true
              && !running
              && allLinted) {
            return {
              effect: i,
              applyEffect: () => handleCompile(dispatch, currentFile, typeCheck),
            };
          }
        }
        break;
      case 'run': {
        console.log('run');
        const {
          currentFile,
          runKind,
          isMessageHandlerReady,
          isSetupFinished,
          compiling,
          running,
        } = state;
        if (isMessageHandlerReady && isSetupFinished && !compiling && !running) {
          return {
            effect: i,
            applyEffect: () => handleRun(dispatch, currentFile, runKind),
          };
        }
        break;
      }
      case 'stop': {
        const { running } = state;
        if (running && currentRunner !== undefined) {
          return {
            effect: i,
            applyEffect: () => handleStop(dispatch),
          };
        }

        return {
          effect: i,
          applyEffect: () => { },
        };
      }
      default:
        throw new Error('getFirstActionableEffect: unknown effect');
    }
  }

  return false;
}

const store = createStore(
  ideApp,
  (window as any).__REDUX_DEVTOOLS_EXTENSION__ && (window as any).__REDUX_DEVTOOLS_EXTENSION__(),
);

const ide: IDE = {
  dispatchSpyMessage: (loc: string, message: string | undefined) => {
    store.dispatch({
      type: 'update',
      key: 'rt-message',
      value: {
        tag: 'spy-message',
        message: true,
        value: message,
        loc,
      },
    });
  },
  dispatchSpyValue: (loc: string, key: string, value: any) => {
    store.dispatch({
      type: 'update',
      key: 'rt-message',
      value: {
        tag: 'spy-value',
        value: {
          key,
          value,
        },
        loc,
      },
    });
  },
};

// @ts-ignore
window.ide = ide;

/* This callback is triggered after a reducer is run. It looks though the effect
   queue and applies the first effect it can apply.

   This is somewhat of a hack to get around the fact that Redux doesn't have a
   built-in way of handling side effects. The logic of how this works goes
   something like this:
   - An action (see action.ts) is dispatched of type enqueueEffect
   - A reducer (see reducer.ts) adds the corresponding effect to state's effect queue
     (see state.ts)
   - This callback is triggered as a result of the reducer running
   - This callback selects a side effect to apply from the state's effect queue
   - This callback dispatches an action of type effectStarted
   - A reducer marks that action as active, by setting either compiling, linting, etc.
     to true. It also removes the effect from the effect queue.
   - This callback calls a function that does the side effect.
   - That function, when the side effect is finished, dispatches an effectEnded action.
   - A reducer marks that action as inactive, by setting either compiling, linting, etc.
     to false. */
store.subscribe(() => {
  const state = store.getState();

  const { dispatch } = store;
  const { effectQueue } = state;

  const collapsedState = {
    ...state,
    effectQueue: collapseEffectQueue(effectQueue),
  };

  const maybeEffect = handleFirstActionableEffect(collapsedState, dispatch);

  if (!maybeEffect) {
    return;
  }

  const { effect, applyEffect } = maybeEffect;

  dispatch({ type: 'effectStarted', effect });
  applyEffect();
});

store.dispatch({ type: 'enqueueEffect', effect: { effectKey: 'setupWorkerMessageHandler' } });

/* Try to load a Chunk mode program from the URI component ?program=uri-encoded-program */

const maybeEncodedProgram: null | string = new URLSearchParams(window.location.search).get('program');

if (maybeEncodedProgram !== null) {
  const decodedProgram = decodeURIComponent(maybeEncodedProgram);
  const chunks: Chunk[] = makeChunksFromString(decodedProgram);
  store.dispatch({ type: 'update', key: 'chunks', value: { chunks, modifiesText: true } });
  store.dispatch({ type: 'enqueueEffect', effect: { effectKey: 'saveFile' } });
}

store.dispatch({ type: 'enqueueEffect', effect: { effectKey: 'loadFile' } });

export default store;
