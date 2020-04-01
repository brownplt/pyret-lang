import { createStore } from 'redux';
import ideApp from './reducer';
import { CompileState, EditorMode, CHUNKSEP } from './state';
import * as control from './control';

const store = createStore(
  ideApp,
  (window as any).__REDUX_DEVTOOLS_EXTENSION__ && (window as any).__REDUX_DEVTOOLS_EXTENSION__(),
);

export default store;

function handleLog(message: string): void {
  console.log(message);
}

function handleSetupFinished(): void {
  store.dispatch({ type: 'finishSetup' });
}

function handleCompileFailure(errors: string[]): void {
  store.dispatch({
    type: 'compileFailure',
    errors,
  });
}

function handleRuntimeFailure(errors: string[]): void {
  store.dispatch({
    type: 'runFailure',
    errors,
  });
}

function handleLintFailure(lintFailure: { name: string, errors: string[] }): void {
  store.dispatch({
    type: 'lintFailure',
    lintFailure,
  });
}

function handleLintSuccess(lintSuccess: { name: string }): void {
  store.dispatch({
    type: 'lintSuccess',
    lintSuccess,
  });
}

function handleCompileSuccess(): void {
  store.dispatch({
    type: 'compileSuccess',
  });
}

function handleCreateReplSuccess(): void {
  store.dispatch({
    type: 'finishCreateRepl',
  });
}

function handleCompileInteractionSuccess(): void {
  console.log('compile interaction success (nyi)');
}

function handleCompileInteractionFailure(): void {
  console.log('compile interaction failure (nyi)');
}

store.subscribe(() => {
  const state = store.getState();

  if (state.needLoadFile && state.currentFile !== undefined) {
    if (state.editorMode === EditorMode.Text) {
      store.dispatch({
        type: 'updateContents',
        contents: control.openOrCreateFile(state.currentFile),
      });
    } else if (state.editorMode === EditorMode.Chunks) {
      const fileContents = control.openOrCreateFile(state.currentFile);
      const chunkStrings = fileContents.split(CHUNKSEP);
      let totalLines = 0;
      const chunks = chunkStrings.map((chunkString, i) => {
        const chunk = {
          text: chunkString,
          id: String(i),
          startLine: totalLines,
        };

        totalLines += chunkString.split('\n').length;

        return chunk;
      });
      store.dispatch({
        type: 'updateContents',
        contents: fileContents,
      });
      store.dispatch({ type: 'setChunks', chunks });
    }
  }

  switch (state.compileState) {
    case CompileState.NeedsStartup:
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
      store.dispatch({ type: 'startupCompleted' });
      return;
    case CompileState.ChunkNeedsRepl:
      control.createRepl();
      return;
    case CompileState.Ready:
      if (state.updateQueued) {
        const parsed = control.bfsSetup.path.parse(state.currentFile);
        if (state.editorMode === EditorMode.Text) {
          control.fs.writeFileSync(
            state.currentFile,
            state.currentFileContents,
          );
        } else if (state.editorMode === EditorMode.Chunks) {
          control.fs.writeFileSync(
            state.currentFile,
            state.TMPchunks.map((chunk) => chunk.text).join(CHUNKSEP),
          );
        }
        control.compile(
          parsed.dir,
          parsed.base,
          state.typeCheck,
        );
        store.dispatch({ type: 'compile' });
      }
      return;
    case CompileState.NeedsRun:
      store.dispatch({ type: 'runStarted' });
      control.run(
        control.path.runBase,
        control.path.runProgram,
        (runResult: any) => {
          store.dispatch({ type: 'runFinished', result: runResult });
        },
        (runner: any) => {
          store.dispatch({ type: 'updateRunner', runner });
        },
        control.backend.RunKind.Async,
      );
      break;
    default:
  }
});

store.dispatch({ type: 'beginStartup' });
store.dispatch({ type: 'expandChild', path: '/projects/program.arr' });
