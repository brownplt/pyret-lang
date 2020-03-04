// This file is used to track the state of the editor.

// Possible states for the editor.
export enum TextCompileState {
    // Starting state for the application. We are waiting for the webworker to
    // give us confirmation that it has finished its setup phase and is ready
    // to receive compilation requests.
    //
    // Startup -> StartupQueue
    //   The user edits the definitions area or clicks "run".
    //
    // Startup -> Ready
    //   The webworker finishes its setup.
    Startup,

    // We are waiting for the webworker to give us confirmation that it has
    // finished its setup phase so that we can satisfy a queued compilation
    // request.
    //
    // StartupQueue -> StartupQueue
    //   The user edits the definitions area or clicks "run".
    //
    // StartupQueue -> Compile
    //   The webworker finishes its setup
    StartupQueue,

    // We are able to immediately satisfy any compilation requests.
    //
    // Ready -> Compile
    //   The user edits the definitions area or clicks "run".
    Ready,

    // We are compiling the program. Any compilation request generated during
    // this state will queue it for later.
    //
    // Compile -> CompileQueue
    //   The user edits the definitions area or clicks "run".
    //
    // Compile -> RunningWithStops
    //   Compilation (with stopify) succeeded and autoRun is enabled. The
    //   program is run.
    //
    // Compile -> RunningWithoutStops
    //   Compilation (without stopify) succeeded and autoRun is enabled. The
    //   program is run.
    //
    // Compile -> Ready
    //   Compilation failed.
    Compile,

    // We have received a compilation request during a compilation.
    //
    // CompileQueue -> CompileQueue
    //   The user edits the definitions area or clicks "run".
    //
    // CompileQueue -> Compile
    //   Compilation either succeeded or failed. Either way, the program is not
    //   run.
    CompileQueue,

    CompileRun,
    CompileRunQueue,

    // The program (which has been compiled with Stopify) is running. It can be
    // stopped by the user when the press the "stop" button.
    //
    // RunningWithStops -> Stopped
    //   The user presses the "stop" button. The program is stopped.
    //
    // RunningWithStops -> Ready
    //   The program finishes its execution.
    //
    // RunningWithStops -> Compile
    //   The user edits the definitions area or hits "run".
    RunningWithStops,

    // The program is running. It has not been compiled with Stopify, so it
    // cannot be interrupted.
    //
    // RunningWithoutStops -> Ready
    //   The program finishes its execution
    RunningWithoutStops,

    // Stopped -> Compile
    //   The user edits the definitions area or hits  the "run" button.
    Stopped,
}

export const compileStateToString = (state: TextCompileState): string => {
    // TODO(michael): these could be more pirate-themed
    if (state === TextCompileState.Startup) {
        return "Finishing setup";
    } else if (state === TextCompileState.StartupQueue) {
        return "Compile request on hold: finishing setup";
    } else if (state === TextCompileState.Ready) {
        return "Ready";
    } else if (state === TextCompileState.Compile) {
        return "Compiling";
    } else if (state === TextCompileState.CompileQueue) {
        return "Compile request on hold: already compiling";
    } else if (state === TextCompileState.CompileRun) {
        return "Waiting to run: compiling";
    } else if (state === TextCompileState.CompileRunQueue) {
        return "Compile and run requests on hold: already compiling"
    } else if (state === TextCompileState.RunningWithStops) {
        return "Running (stop button enabled)";
    } else if (state === TextCompileState.RunningWithoutStops) {
        return "Running (stop button disabled)";
    } else if (state === TextCompileState.Stopped) {
        return "Program execution stopped"
    } else {
        const assertNever = (_arg: never): never => {
            throw new Error("assertNever");
        };

        return assertNever(state);
    }
};

export const invalidTextCompileState = (state: TextCompileState): void => {
    throw new Error(`illegal TextCompileState reached: ${state}`);
};
