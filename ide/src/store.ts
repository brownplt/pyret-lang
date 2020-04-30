import { createStore } from 'redux';
import ideApp from './reducer';
import { EditorMode, CHUNKSEP, State } from './state';
import { Effect } from './effect';
import {
  newId,
  Chunk,
} from './chunk';
import { Action } from './action';
import { RunKind } from './backend';
import * as control from './control';

type Dispatch = (action: Action) => void;

function handleLoadFile(
  dispatch: Dispatch,
  currentFile: string,
  editorMode: EditorMode,
) {
  const contents = control.openOrCreateFile(currentFile);

  // dispatch({ type: 'update', key: 'currentFileContents', value: contents });

  switch (editorMode) {
    case EditorMode.Text:
      // nothing more to do
      break;
    case EditorMode.Chunks:
      {
        const chunkStrings = contents.split(CHUNKSEP);
        let totalLines = 0;
        const chunks = chunkStrings.map((chunkString) => {
          const chunk = {
            text: chunkString,
            startLine: totalLines,
            editor: undefined,
            id: newId(),
          };

          totalLines += chunkString.split('\n').length;

          return chunk;
        });

        dispatch({ type: 'update', key: 'chunks', value: chunks });
      }
      break;
    default:

      //  throw new Error('handleLoadFile: unknown editor mode');
  }
}

function handleSetupWorkerMessageHandler(dispatch: Dispatch) {
  function handleLog(message: string): void {
    console.log(message);
  }

  function handleSetupFinished(): void {
    const action: any = {
      type: 'setAsyncStatus',
      status: 'succeeded',
      process: 'setup',
    };
    dispatch(action);
  }

  function handleCompileFailure(errors: string[]): void {
    const action: any = {
      type: 'setAsyncStatus',
      status: 'failed',
      process: 'compile',
      errors,
    };
    dispatch(action);
  }

  function handleRuntimeFailure(errors: string[]): void {
    const action: any = {
      type: 'setAsyncStatus',
      status: 'failed',
      process: 'run',
      errors,
    };
    dispatch(action);
  }

  function handleLintFailure(lintFailure: { name: string, errors: string[] }): void {
    const action: any = {
      type: 'setAsyncStatus',
      status: 'failed',
      process: 'lint',
      errors: lintFailure,
    };
    dispatch(action);
  }

  function handleLintSuccess(lintSuccess: { name: string }): void {
    const action: any = {
      type: 'setAsyncStatus',
      status: 'succeeded',
      process: 'lint',
      name: lintSuccess.name,
    };
    dispatch(action);
  }

  function handleCompileSuccess(): void {
    const action: any = {
      type: 'setAsyncStatus',
      status: 'succeeded',
      process: 'compile',
    };
    dispatch(action);
  }

  function handleCreateReplSuccess(): void {
    const action: any = {
      type: 'setAsyncStatus',
      status: 'succeeded',
      process: 'createRepl',
    };
    dispatch(action);
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

  dispatch({ type: 'update', key: 'isMessageHandlerReady', value: true });
}

function handleCreateRepl(dispatch: Dispatch) {
  dispatch({
    type: 'setAsyncStatus',
    status: 'started',
    process: 'createRepl',
  });
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
}

function handleCompile(dispatch: Dispatch, path: string, typeCheck: boolean) {
  dispatch({
    type: 'setAsyncStatus',
    status: 'started',
    process: 'compile',
  });
  const { dir, base } = control.bfsSetup.path.parse(path);
  control.compile(dir, base, typeCheck);
}

function handleRun(dispatch: Dispatch, runKind: RunKind) {
  dispatch({
    type: 'setAsyncStatus',
    status: 'started',
    process: 'run',
  });
  const { runBase, runProgram } = control.path;
  control.run(
    runBase,
    runProgram,
    (runResult: any) => {
      console.log('runResult', runResult);
      const action: any = {
        type: 'setAsyncStatus',
        status: 'succeeded', // TODO: not every run is a success
        process: 'run',
        result: runResult,
      };
      dispatch(action);
    },
    (runner: any) => {
      dispatch({
        type: 'update',
        key: 'currentRunner',
        value: runner,
      });
    },
    runKind,
  );
}

function handleFirstActionableEffect(
  state: State,
  dispatch: Dispatch,
): { effectQueue: Effect[], applyEffect: () => void } {
  const { effectQueue } = state;

  if (effectQueue.length === 0) {
    return { effectQueue: [], applyEffect: () => { } };
  }

  function getNewEffectQueue(indexToRemove: number) {
    return [
      ...effectQueue.slice(0, indexToRemove),
      ...effectQueue.slice(indexToRemove + 1, effectQueue.length),
    ];
  }

  for (let i = 0; i < effectQueue.length; i += 1) {
    const effect = effectQueue[i];

    switch (effect) {
      case 'loadFile':
        {
          console.log('loadFile');
          const { currentFile, editorMode } = state;
          if (currentFile !== undefined) {
            return {
              effectQueue: getNewEffectQueue(i),
              applyEffect: () => handleLoadFile(dispatch, currentFile, editorMode),
            };
          }
        }
        break;
      case 'saveFile':
        {
          console.log('saveFile');
          const {
            editorMode, currentFile, currentFileContents, chunks,
          } = state;
          if (currentFile !== undefined && currentFileContents !== undefined) {
            return {
              effectQueue: getNewEffectQueue(i),
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
              effectQueue: getNewEffectQueue(i),
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
              effectQueue: getNewEffectQueue(i),
              applyEffect: () => handleCreateRepl(dispatch),
            };
          }
        }
        break;
      case 'lint':
        console.log('applyFirstActionableEffect: warning: lint effect ignored (nyi)');
        return {
          effectQueue: getNewEffectQueue(i),
          applyEffect: () => { },
        };
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
          } = state;
          if (isMessageHandlerReady && isSetupFinished && !compiling && !running) {
            return {
              effectQueue: getNewEffectQueue(i),
              applyEffect: () => handleCompile(dispatch, currentFile, typeCheck),
            };
          }
        }
        break;
      case 'run': {
        console.log('run');
        const {
          runKind,
          isMessageHandlerReady,
          isSetupFinished,
          compiling,
          running,
        } = state;
        if (isMessageHandlerReady && isSetupFinished && !compiling && !running) {
          return {
            effectQueue: getNewEffectQueue(i),
            applyEffect: () => handleRun(dispatch, runKind),
          };
        }
        break;
      }
      case 'stop':
        // TODO
        console.log('applyFirstActionableEffect: warning: stop effect ignored (nyi)');
        return {
          effectQueue: getNewEffectQueue(i),
          applyEffect: () => { },
        };
      default:
        throw new Error('getFirstActionableEffect: unknown effect');
    }
  }

  return { // we didn't handle any effects
    effectQueue,
    applyEffect: () => { },
  };
}

const store = createStore(
  ideApp,
  (window as any).__REDUX_DEVTOOLS_EXTENSION__ && (window as any).__REDUX_DEVTOOLS_EXTENSION__(),
);

store.subscribe(() => {
  const state = store.getState();

  const oldEffectQueue = state.effectQueue;
  const { dispatch } = store;

  const { effectQueue, applyEffect } = handleFirstActionableEffect(state, dispatch);

  if (oldEffectQueue.length !== effectQueue.length) {
    dispatch({
      type: 'update',
      key: 'effectQueue',
      value: effectQueue,
    });
    applyEffect();
  } // else if (oldEffectQueue.length > 0) {
  //   throw new Error('could not apply any effect in queue');
  // }
});

store.dispatch({ type: 'queueEffect', effect: 'setupWorkerMessageHandler' });
store.dispatch({ type: 'queueEffect', effect: 'loadFile' });

export default store;

// store.subscribe(() => {
//   const state = store.getState();
//
//   switch (state.compileState) {
//     case CompileState.NeedsStartup:
//       control.setupWorkerMessageHandler(
//         handleLog,
//         handleSetupFinished,
//         handleCompileFailure,
//         handleRuntimeFailure,
//         handleLintFailure,
//         handleLintSuccess,
//         handleCompileSuccess,
//         handleCreateReplSuccess,
//         handleCompileInteractionSuccess,
//         handleCompileInteractionFailure,
//       );
//       store.dispatch({ type: 'startupCompleted' });
//       return;
//     case CompileState.ChunkNeedsRepl:
//       control.createRepl();
//       return;
//     case CompileState.Ready:
//       if (state.updateQueued) {
//         const parsed = control.bfsSetup.path.parse(state.currentFile);
//         if (state.editorMode === EditorMode.Text) {
//           control.fs.writeFileSync(
//             state.currentFile,
//             state.currentFileContents,
//           );
//         } else if (state.editorMode === EditorMode.Chunks) {
//           control.fs.writeFileSync(
//             state.currentFile,
//             state.chunks.map((chunk) => chunk.text).join(CHUNKSEP),
//           );
//         }
//         control.compile(
//           parsed.dir,
//           parsed.base,
//           state.typeCheck,
//         );
//         store.dispatch({ type: 'compile' });
//       }
//       return;
//     case CompileState.NeedsRun:
//       store.dispatch({ type: 'runStarted' });
//       control.run(
//         control.path.runBase,
//         control.path.runProgram,
//         (runResult: any) => {
//           store.dispatch({ type: 'runFinished', result: runResult });
//         },
//         (runner: any) => {
//           store.dispatch({ type: 'updateRunner', runner });
//         },
//         control.backend.RunKind.Async,
//       );
//       break;
//     default:
//   }
// });

// store.dispatch({ type: 'beginStartup' });
// store.dispatch({ type: 'expandChild', path: '/projects/program.arr' });
