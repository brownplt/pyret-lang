import { createStore } from 'redux';
import { ideApp } from './reducers';
import { CompileState } from './State';
import * as control from './control';

export const store = createStore(ideApp);

function handleLog(message: string): void {
  console.log("log");
  console.log(message);
}

function handleSetupFinished(): void {
  console.log("setup finished");
  store.dispatch({ type: "finishSetup"} )
}

function handleCompileFailure(errors: string[]): void {
  console.log("compile failure");
  store.dispatch({
    type: "textCompileFailure",
    errors
  });
}

function handleRuntimeFailure(errors: string[]): void {
  console.log("runtime failure");
  store.dispatch({
    type: "textRunFailure",
    errors
  })
}

function handleLintFailure(lintFailure: { name: string, errors: string[] }): void {
  console.log("lint failure");
  store.dispatch({
    type: "textLintFailure",
    lintFailure
  })
}

function handleLintSuccess(lintSuccess: { name: string }): void {
  console.log("lint success");
  store.dispatch({
    type: "textLintSuccess",
    lintSuccess
  })
}

function handleCompileSuccess(): void {
  console.log("compile success");
  store.dispatch({
    type: "textCompileSuccess"
  });
}

function handleCreateReplSuccess(): void {
  console.log("create repl succes (nyi)");
}

function handleCompileInteractionSuccess(): void {
  console.log("compile interaction success (nyi)");
}

function handleCompileInteractionFailure(): void {
  console.log("compile interaction failure (nyi)");
}

store.subscribe(() => {
  const state = store.getState();

  switch (state.compileState) {
    case CompileState.TextNeedsStartup:
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
      return;
    case CompileState.TextReadyQueue:
      control.fs.writeFileSync(
        control.bfsSetup.path.join(
          ...state.currentFileDirectory,
          state.currentFileName),
        state.currentFileContents);
      control.compile(
        control.bfsSetup.path.join(...state.currentFileDirectory),
        state.currentFileName,
        state.typeCheck);
      store.dispatch({ type: "textCompileQueue" });
      return;
    default:
      return;
  }
});
