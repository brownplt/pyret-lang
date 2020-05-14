import { createStore } from 'redux';
import ideApp from './reducer';
import { EditorMode, CHUNKSEP, State } from './state';
import {
  newId,
  Chunk,
} from './chunk';
import { Action } from './action';
import { RunKind } from './backend';
import * as control from './control';

type Dispatch = (action: Action) => void;

let currentRunner: any;

function handleStartEditTimer(dispatch: Dispatch, editTimer: NodeJS.Timer | false) {
  if (editTimer) {
    clearTimeout(editTimer);
  }

  dispatch({
    type: 'effectEnded',
    status: 'succeeded',
    effect: 'startEditTimer',
    timer: setTimeout(() => {
      dispatch({
        type: 'effectEnded',
        status: 'succeeded',
        effect: 'editTimer',
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
      const chunkStrings = contents.split(CHUNKSEP);
      let totalLines = 0;
      const chunks = chunkStrings.map((chunkString) => {
        const chunk: Chunk = {
          text: chunkString,
          startLine: totalLines,
          id: newId(),
          errorState: { status: 'notLinted' },
          editor: false,
        };

        totalLines += chunkString.split('\n').length;

        return chunk;
      });

      dispatch({ type: 'update', key: 'chunks', value: chunks });
      break;
    }
    default:
  }

  dispatch({
    type: 'effectEnded',
    status: 'succeeded',
    effect: 'loadFile',
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
      effect: 'setup',
    });
  }

  function handleCompileFailure(errors: string[]): void {
    dispatch({
      type: 'effectEnded',
      status: 'failed',
      effect: 'compile',
      errors,
    });
  }

  function handleRuntimeFailure(errors: string[]): void {
    dispatch({
      type: 'effectEnded',
      status: 'failed',
      effect: 'run',
      errors,
    });
  }

  function handleLintFailure(lintFailure: { name: string, errors: string[] }): void {
    dispatch({
      type: 'effectEnded',
      status: 'failed',
      effect: 'lint',
      name: lintFailure.name,
      errors: lintFailure.errors,
    });
  }

  function handleLintSuccess(lintSuccess: { name: string }): void {
    dispatch({
      type: 'effectEnded',
      status: 'succeeded',
      effect: 'lint',
      name: lintSuccess.name,
    });
  }

  function handleCompileSuccess(): void {
    dispatch({
      type: 'effectEnded',
      status: 'succeeded',
      effect: 'compile',
    });
  }

  function handleCreateReplSuccess(): void {
    dispatch({
      type: 'effectEnded',
      status: 'succeeded',
      effect: 'createRepl',
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
    effect: 'setupWorkerMessageHandler',
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
    effect: 'saveFile',
  });
}

function handleCompile(dispatch: Dispatch, path: string, typeCheck: boolean) {
  const { dir, base } = control.bfsSetup.path.parse(path);
  control.compile(dir, base, typeCheck);
}

function handleRun(dispatch: Dispatch, currentFile: string, runKind: RunKind) {
  const { base } = control.bfsSetup.path.parse(currentFile);
  control.run(
    control.path.runBase,
    `${base}.js`,
    (runResult: any) => {
      console.log('runResult', runResult);
      if (runResult.result.error === undefined) {
        dispatch({
          type: 'effectEnded',
          status: 'succeeded',
          effect: 'run',
          result: runResult,
        });
      } else {
        dispatch({
          type: 'effectEnded',
          status: 'failed',
          effect: 'run',
          errors: runResult.result.result,
        });
      }
    },
    (runner: any) => {
      currentRunner = runner;
    },
    runKind,
  );
}

function handleStop(dispatch: Dispatch) {
  currentRunner.pause((line: number) => {
    dispatch({
      type: 'effectEnded',
      status: 'succeeded',
      effect: 'stop',
      line,
    });
  });
}

function handleTextLint(currentFileContents: string): void {
  control.lint(currentFileContents, 'text');
}

function handleChunkLint(text: string, id: number): void {
  control.lint(text, String(id));
}

function handleFirstActionableEffect(
  state: State,
  dispatch: Dispatch,
): false | { effect: number, applyEffect: () => void } {
  const { effectQueue } = state;

  for (let i = 0; i < effectQueue.length; i += 1) {
    const effect = effectQueue[i];

    switch (effect) {
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
          } = state;
          let allLinted = true;

          for (let j = 0; j < chunks.length; j += 1) {
            if (chunks[j].errorState.status !== 'succeeded') {
              allLinted = false;
              break;
            }
          }

          if (isMessageHandlerReady
              && isSetupFinished
              && isFileSaved
              && !compiling
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

store.subscribe(() => {
  const state = store.getState();

  const { dispatch } = store;

  const maybeEffect = handleFirstActionableEffect(state, dispatch);

  if (!maybeEffect) {
    return;
  }

  const { effect, applyEffect } = maybeEffect;

  dispatch({ type: 'effectStarted', effect });
  applyEffect();
});

store.dispatch({ type: 'enqueueEffect', effect: 'setupWorkerMessageHandler' });
store.dispatch({ type: 'enqueueEffect', effect: 'loadFile' });

export default store;
