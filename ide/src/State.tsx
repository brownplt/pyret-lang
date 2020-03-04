// This file is used to track the state of the editor.

import { Editor, EditorMode } from './Editor';
import { CHUNKSEP } from './DefChunks';
import * as control from './control';

// Possible states for the editor.
export enum CompileState {
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

export const compileStateToString = (state: CompileState): string => {
    // TODO(michael): these could be more pirate-themed
    if (state === CompileState.Startup) {
        return "Finishing setup";
    } else if (state === CompileState.StartupQueue) {
        return "Compile request on hold: finishing setup";
    } else if (state === CompileState.Ready) {
        return "Ready";
    } else if (state === CompileState.Compile) {
        return "Compiling";
    } else if (state === CompileState.CompileQueue) {
        return "Compile request on hold: already compiling";
    } else if (state === CompileState.CompileRun) {
        return "Waiting to run: compiling";
    } else if (state === CompileState.CompileRunQueue) {
        return "Compile and run requests on hold: already compiling"
    } else if (state === CompileState.RunningWithStops) {
        return "Running (stop button enabled)";
    } else if (state === CompileState.RunningWithoutStops) {
        return "Running (stop button disabled)";
    } else if (state === CompileState.Stopped) {
        return "Program execution stopped"
    } else {
        const assertNever = (_arg: never): never => {
            throw new Error("assertNever");
        };

        return assertNever(state);
    }
};

const invalidCompileState = (state: CompileState): void => {
    throw new Error(`illegal CompileState reached: ${state}`);
};

export const handleLog = (editor: Editor) => {
    return (message: string) => {
        console.log(message);
    };
};

export const handleSetupFinished = (editor: Editor) => {
    return () => {
        console.log("setup finished");

        if (editor.state.compileState === CompileState.Startup) {
            editor.setState({compileState: CompileState.Ready});
        } else if (editor.state.compileState === CompileState.StartupQueue) {
            editor.setState({compileState: CompileState.Ready});
            editor.update();
        } else {
            invalidCompileState(editor.state.compileState);
        }
    };
};

export const handleCompileFailure = (editor: Editor) => {
    return (errors: string[]) => {
        console.log("COMPILE FAILURE");
        if (editor.state.compileState === CompileState.Compile
            || editor.state.compileState === CompileState.CompileRun) {
            editor.setState({compileState: CompileState.Ready});

            const places: any = [];
            for (let i = 0; i < errors.length; i++) {
                const matches = errors[i].match(/:\d+:\d+-\d+:\d+/g);
                if (matches !== null) {
                    matches.forEach((m) => {
                        places.push(m.match(/\d+/g)!.map(Number));
                    });
                }
            }
            editor.setState(
                {
                    interactionErrors: errors,
                    definitionsHighlights: places
                }
            );
        } else if (editor.state.compileState === CompileState.CompileQueue
                   || editor.state.compileState === CompileState.CompileRunQueue) {
            editor.setState({compileState: CompileState.Ready});
            editor.update();
        } else {
            invalidCompileState(editor.state.compileState);
        }
    };
};

export const handleRuntimeFailure = (editor: Editor) => {
    return (errors: string[]) => {
        editor.setState(
            {
                interactionErrors: [errors.toString()],
            }
        );
    };
};

export const handleLintFailure = (editor: Editor) => {
    return (lintFailure : { name: string, errors: string[]}) => {
        let newFailures = editor.state.lintFailures;
        const name = lintFailure.name;
        newFailures[name] = lintFailure;
        editor.setState({ lintFailures: newFailures });
    };
};

export const handleLintSuccess = (editor: Editor) => {
    return (lintSuccess : { name: string}) => {
        let newFailures = editor.state.lintFailures;
        const name = lintSuccess.name;
        if(name in newFailures) { delete newFailures[name]; }
        editor.setState({ lintFailures: newFailures });
    };
};

const makeResult = (
    result: any,
    moduleUri: string): { key: string, name: string, value: any }[] => {
    const compareLocations = (a: any, b: any): number => {
        return a.srcloc[1] - b.srcloc[1];
    };

    // There may be toplevel expressions in many modules, but we only want to
    // show the ones from the main module we're working on
    const mainTraces = result.$traces.filter((t : any) => t.srcloc[0] === moduleUri);

    const allWithLocs = result.$locations.concat(mainTraces);

    // We combine and then sort to get the traces interleaved correctly with named values
    const allSorted = allWithLocs.sort(compareLocations);
    return allSorted.map((key: any) => {
        if('name' in key) {
            return {
                name: key.name,
                key: key.name,
                line: key.srcloc[1],
                value: result[key.name]
            };
        }
        else {
            return {
                name: "",
                key: String(key.srcloc[1]),
                line: key.srcloc[1],
                value: key.value
            };
        }
    });
};

export const handleCompileSuccess = (editor: Editor) => {
    return () => {
        console.log("COMPILE SUCCESS");
        if (editor.state.compileState === CompileState.Compile) {
            editor.setState({compileState: CompileState.Ready});
        } else if (editor.state.compileState === CompileState.CompileQueue
                   || editor.state.compileState === CompileState.CompileRunQueue) {
            editor.setState({compileState: CompileState.Ready});
            editor.update();
        } else if (editor.state.compileState === CompileState.CompileRun) {
            if (editor.stopify) {
                editor.setState({compileState: CompileState.RunningWithStops});
            } else {
                editor.setState({compileState: CompileState.RunningWithoutStops});
            }
            const x = new Date();
            console.log(`Run ${x} started`);
            control.run(
                control.path.runBase,
                control.path.runProgram,
                (runResult: any) => {
                    editor.setState({compileState: CompileState.Ready});
                    console.log(`Run ${x} finished`);
                    console.log(runResult);
                    if (runResult.result !== undefined) {
                        if (runResult.result.error === undefined) {
                            const results =
                                makeResult(
                                    runResult.result,
                                    'file://' +
                                    control.bfsSetup.path.join(
                                        control.path.compileBase,
                                        editor.state.currentFileName));
                            const checks = runResult.result.$checks;
                            editor.setState({
                                interactions: results,
                                checks: checks
                            });

                            if (results[0] !== undefined && results[0].name === "error") {
                                editor.setState(
                                    {
                                        interactionErrors: runResult.result.error,
                                    }
                                );
                            }
                        } else {
                            editor.setState({
                                interactionErrors: [runResult.result.error],
                            });
                        }
                    }
                },
                (runner: any) => {
                    editor.setState({currentRunner: runner});
                },
                editor.state.runKind);
        } else {
            invalidCompileState(editor.state.compileState);
        }
    };
};

export const handleCreateReplSuccess = (editor: Editor) => {
    return () => {
        console.log("REPL successfully created");
    };
};

export const handleCompileInteractionSuccess = (editor: Editor) => {
    return (response: { program: string }) => {
        console.log(`Chunk ${response.program} successfully compiled.`);
        return;
    };
};

export const handleCompileInteractionFailure = (editor: Editor) => {
    return (response: { program: string }) => {
        console.error(`Failed to compile ${response.program}.`);
        return;
    };
};

export const handleRun = (editor: Editor) => {
    return (runAfterwards: boolean) => {
        editor.setState(
            {
                interactionErrors: [],
                definitionsHighlights: []
            }
        );
        if (editor.isPyretFile) {
            if (editor.state.compileState === CompileState.Startup) {
                editor.setState({compileState: CompileState.StartupQueue});
            } else if (editor.state.compileState === CompileState.StartupQueue) {
                // state remains as StartupQueue
            } else if (editor.state.compileState === CompileState.Ready
                       || editor.state.compileState === CompileState.Stopped) {
                if (runAfterwards || editor.state.autoRun) {
                    editor.setState({compileState: CompileState.CompileRun});
                } else {
                    editor.setState({compileState: CompileState.Compile});
                }
                control.compile(
                    editor.currentFileDirectory,
                    editor.currentFileName,
                    editor.state.typeCheck);
            } else if (editor.state.compileState === CompileState.Compile) {
                editor.setState({compileState: CompileState.CompileQueue});
            } else if (editor.state.compileState === CompileState.CompileRun) {
                editor.setState({compileState: CompileState.CompileRunQueue});
            } else if (editor.state.compileState === CompileState.CompileQueue) {
                // state remains as CompileQueue
            } else if (editor.state.compileState === CompileState.CompileRunQueue) {
                // state remains as CompileRunQueue
            } else if (editor.state.compileState === CompileState.RunningWithStops) {
                editor.stop();
                editor.update();
                // state remains as RunningWithStops
            } else if (editor.state.compileState === CompileState.RunningWithoutStops) {
                // state remains as RunningWithoutStops
            } else {
                invalidCompileState(editor.state.compileState);
            }
        } else {
            editor.setState({
                interactions: [
                    {
                        key: "Error",
                        name: "Error",
                        value: "Run is not supported on editor file type"
                    },
                    {
                        key: "File",
                        name: "File",
                        value: editor.currentFile
                    }],
                interactionErrors: ["Error: Run is not supported on editor file type"],
            });
        }
    };
};

export const handleUpdate = (editor: Editor) => {
    return (): void => {
        control.fs.writeFileSync(
            editor.currentFile,
            editor.state.currentFileContents);

        if (editor.state.editorMode === EditorMode.Chunks) {
            const chunkstrs = editor.state.currentFileContents.split(CHUNKSEP);
            for (let i = 0; i < chunkstrs.length; i++) {
                control.fs.writeFileSync(
                    `${editor.currentFile}.chunk.${i}`,
                    chunkstrs[i]);
            }

            for (let i = 0; i < chunkstrs.length; i++) {
                console.log(`Sending message to webworker to compile: ${editor.currentFile}.chunk.${i}`)
                control.backend.compileInteraction(control.worker, `${editor.currentFile}.chunk.${i}`);
                // todo: wait for response before compiling more chunks
            }
        } else {
            editor.run(false);
        }
    };
};

export const handleEdit = (editor: Editor) => {
    return (value: string): void => {
        clearTimeout(editor.state.updateTimer);
        editor.setState({
            //        currentFileContents: value,
            updateTimer: setTimeout(() => {
                editor.setState({currentFileContents: value});
                editor.update();
            }, 250),
        });
    };
};
