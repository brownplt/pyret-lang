// This file is used to track the state of the editor. Any function that calls
// .setState on an Editor should be written here

import { Editor, EditorMode } from './Editor';
import { CHUNKSEP } from './DefChunks';
import { Check } from './Check';
import * as control from './control';

// Possible states for the editor.
export enum CompileState {
    TextNeedsStartup,
    TextStartup,
    TextStartupQueue,
    TextReady,
    TextReadyQueue,
    TextCompileQueue,
    TextNeedsCompile,
    TextRunning,
    TextRunningWithStops,
    TextRunningQueue,
    TextRunningWithStopsQueue,
    TextRunningWithStopsNeedsStop,

    ChunkStartup,
    ChunkStartupQueue,
    ChunkNeedsRepl,
    ChunkNeedsReplQueue,
    ChunkReady,
    ChunkReadyQueue,
//    // Starting state for the application.
//    //
//    // Text Mode - We are able to handle compilation requests as soon as the
//    // webworker finishes its setup.
//    //
//    //   Startup -> StartupQueue
//    //     The user edits the definitions area or clicks "run".
//    //
//    //   Startup -> Ready
//    //     The webworker finishes its setup.
//    //
//    // Chunk Mode - We must initialize a REPL after the webworker finishes its
//    // setup before we can compile anything.
//    //
//    //   Startup -> StartupQueue
//    //     The user edits a chunk or clicks "run".
//    //
//    //   Startup -> StartupRepl
//    //     The webworker finishes its setup
//    Startup,
//
//    NeedsRepl,
//
//    // Text Mode - we should never enter this state
//    //
//    // Chunk Mode - We are waiting for the webworker to respond about our
//    // request to initialize a REPL.
//    //
//    //   StartupRepl -> StartupReplQueue
//    //     The user edits a chunk or clicks "run".
//    //
//    //   StartupRepl -> Ready
//    //     The webworker successfully creates a Repl.
//    StartupRepl,
//
//    // We are waiting for the webworker to give us confirmation that it has
//    // finished its setup phase.
//    //
//    // Text Mode - we are waiting to satisfy a queued compilation request.
//    //
//    //   StartupQueue -> StartupQueue
//    //     The user edits the definitions area or clicks "run".
//    //
//    //   StartupQueue -> Compile
//    //     The webworker finishes its setup.
//    //
//    // Chunk Mode - we are waiting for setup to finish so we can create a REPL
//    // which we will use to satisfy a queued compilation request.
//    //
//    //   StartupQueue -> StartupQueue
//    //     The user edits a chunk or clicks "run".
//    //
//    //   StartupQueue -> StartupReplQueue
//    //     The webworker finishes its setup.
//    StartupQueue,
//
//    // Text Mode - we should never enter this state.
//    //
//    // Chunk Mode - we are waiting for a REPL to be created which we will use to
//    // satisfy a queued compilation request.
//    //
//    //    StartupReplQueue -> StartupReplQueue
//    //      The user edits a chunk or clicks "run".
//    //
//    //    StartupReplQueue -> Compile
//    //      The webworker creates a REPL.
//    StartupReplQueue,
//
//    // Text Mode & Chunk Mode - We are able to immediately satisfy any
//    // compilation requests.
//    //
//    //   Ready -> Compile
//    //     The user edits the definitions area or clicks "run".
//    Ready,
//
//    // Text Mode & Chunk Mode - We are compiling a program. Any
//    // compilation request generated during this state will queue it for later.
//    //
//    //   Compile -> CompileQueue
//    //     The user edits the definitions area / chunk, or clicks "run".
//    //
//    //   Compile -> RunningWithStops
//    //     Compilation (with stopify) succeeded and autoRun is enabled. The
//    //     program is run.
//    //
//    //   Compile -> RunningWithoutStops
//    //     Compilation (without stopify) succeeded and autoRun is enabled. The
//    //     program is run.
//    //
//    //   Compile -> Ready
//    //     Compilation failed.
//    Compile,
//
//    // Text Mode & Chunk Mode - We have received a compilation request during a
//    // compilation.
//    //
//    //   CompileQueue -> CompileQueue
//    //     The user edits the definitions area or clicks "run".
//    //
//    //   CompileQueue -> Compile
//    //     Compilation either succeeded or failed. Either way, the program is not
//    //     run.
//    CompileQueue,
//
//    CompileRun,
//    CompileRunQueue,
//
//    // Text Mode - The program (which has been compiled with Stopify) is running.
//    // It can be stopped by the user when they press the "stop" button.
//    //
//    //   RunningWithStops -> Stopped
//    //     The user presses the "stop" button. The program is stopped.
//    //
//    //   RunningWithStops -> Ready
//    //     The program finishes its execution.
//    //
//    //   RunningWithStops -> Compile
//    //     The user edits the definitions area or hits "run".
//    //
//    // Chunk Mode - A chunk (which has been compiled with Stopify) is running. It
//    // can be stopped by the user when they press the "stop" button. Once the chunk
//    // terminates, the next chunk (the one below it) is compiled.
//    //
//    //   RunningWithStops -> Stopped
//    //     The user presses the "stop" button. The chunk is stopped.
//    //
//    //   RunningWithStops -> Ready
//    //     The chunk finishes its execution and it was the last chunk (i.e., there
//    //     are no chunks below it on the page to compile and run).
//    //
//    //   RunningWithStops -> Compile
//    //     The chunk finishes its execution and is not the last chunk on the page.
//    //     The next chunk will now be compiled.
//    //
//    //   RunningWithStops -> Compile
//    //     The user edits a chunk or hits "run".
//    RunningWithStops,
//
//    // Text Mode - The program is running. It has not been compiled with Stopify,
//    // so it cannot be interrupted.
//    //
//    //   RunningWithoutStops -> Ready
//    //     The program finishes its execution
//    //
//    // Chunk Mode - The chunk is running. It has not been compiled with Stopify, so
//    // it cannot be interrupted.
//    //
//    //   RunningWithoutStops -> Ready
//    //     The chunk finishes its execution and it was the last chunk on the page.
//    //
//    //   RunningWithoutStops => Compile
//    //     The chunk finishes its execution and it was not the last chunk on the page.
//    //     The next chunk will now be compiled.
//    RunningWithoutStops,
//
//    // Text Mode & Chunk Mode - The running of a program or chunk has been stopped
//    // with the "stop" button.
//    //
//    //   Stopped -> Compile
//    //     The user edits the definitions area or hits the "run" button.
//    Stopped,
}

export const editorStateToString = (editor: Editor): string => {
    // TODO(michael): these could be more pirate-themed
    const state = editor.state.compileState;
    if (state === CompileState.Startup) {
        return "Finishing setup";
    } else if (state === CompileState.StartupRepl) {
        return "initializing REPL";
    } else if (state === CompileState.StartupQueue) {
        return "Compile request on hold: finishing setup";
    } else if (state === CompileState.StartupReplQueue) {
        return "Compile request on hold: initializing REPL"
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

export const invalidCompileState = (state: CompileState): void => {
    throw new Error(`illegal CompileState reached: ${state}`);
};

type LintFailure = {
    name: string,
    errors: string[]
}

export type EditorState = {
    browseRoot: string;
    browsePath: string[];
    currentFileDirectory: string[];
    currentFileName: string;
    currentFileContents: string;
    typeCheck: boolean;
    checks: Check[],
    interactions: { key: string, name: string, value: any }[];
    interactionErrors: string[];
    lintFailures: {[name : string]: LintFailure};
    runKind: control.backend.RunKind;
    autoRun: boolean;
    updateTimer: NodeJS.Timer;
    dropdownVisible: boolean;
    fontSize: number;
    editorMode: EditorMode,
    message: string;
    definitionsHighlights: number[][];
    fsBrowserVisible: boolean;
    compileState: CompileState;
    currentRunner: any;
    currentChunk: number;
};

export const makeDefaultEditorState = (props: any) => {
    return {
        browseRoot: props.browseRoot,
        browsePath: props.browsePath,
        currentFileDirectory: props.currentFileDirectory,
        currentFileName: props.currentFileName,
        currentFileContents: control.openOrCreateFile(
            control.bfsSetup.path.join(
                ...props.currentFileDirectory,
                props.currentFileName)),
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
        editorMode: EditorMode.Chunks,
        fontSize: 12,
        message: "Ready to rock",
        definitionsHighlights: [],
        fsBrowserVisible: false,
        compileState: CompileState.Startup,
        currentRunner: undefined,
        currentChunk: 0,
    };
};

export const handleLog = (editor: Editor) => {
    return (message: string) => {
        console.log(message);
    };
};

export const handleSetupFinished = (editor: Editor) => {
    return () => {
        console.log("setup finished");

        if (editor.state.editorMode === EditorMode.Text) {
            if (editor.state.compileState === CompileState.Startup) {
                editor.setState({compileState: CompileState.Ready});
            } else if (editor.state.compileState === CompileState.StartupQueue) {
                editor.setState({compileState: CompileState.Ready});
                editor.update();
            } else {
                invalidCompileState(editor.state.compileState);
            }
        } else if (editor.state.editorMode === EditorMode.Chunks) {
            if (editor.state.compileState === CompileState.Startup) {
                editor.setState({compileState: CompileState.StartupRepl});
                control.createRepl();
            } else if (editor.state.compileState === CompileState.StartupQueue) {
                editor.setState({compileState: CompileState.StartupReplQueue});
                control.createRepl();
            } else {
                invalidCompileState(editor.state.compileState);
            }
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

const getChunks = (editor: Editor) => {
    return editor.state.currentFileContents.split(CHUNKSEP);
};

export const handleCompileSuccess = (editor: Editor) => {
    return () => {
        if (editor.state.editorMode === EditorMode.Text) {
            console.log("COMPILE SUCCESS");
        } else if (editor.state.editorMode === EditorMode.Chunks) {
            console.log(`COMPILE SUCCESS (chunk #${editor.state.currentChunk})`);
        }

        if (editor.state.compileState === CompileState.Compile) {
            editor.setState({compileState: CompileState.Ready});

            if (editor.state.editorMode === EditorMode.Chunks) {
                const numberOfChunks = getChunks(editor).length;
                if (editor.state.currentChunk < numberOfChunks) {
                    editor.setState(
                        {currentChunk: editor.state.currentChunk + 1},
                        () => editor.update());
                }
            }
        } else if (editor.state.compileState === CompileState.CompileQueue
                   || editor.state.compileState === CompileState.CompileRunQueue) {
            editor.setState({compileState: CompileState.Ready});
            editor.update();
        } else if (editor.state.compileState === CompileState.CompileRun) {
            if (editor.state.editorMode === EditorMode.Text) {
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
            } else if (editor.state.editorMode === EditorMode.Chunks) {
                console.log("Skipping run (not yet implemented for chunks)");
                const numberOfChunks = getChunks(editor).length;
                if (editor.state.currentChunk < numberOfChunks) {
                    editor.setState({
                        currentChunk: editor.state.currentChunk + 1,
                        compileState: CompileState.Ready
                    });
                    editor.update();
                }
            }
        } else {
            invalidCompileState(editor.state.compileState);
        }
    };
};

export const handleCreateReplSuccess = (editor: Editor) => {
    return () => {
        console.log("REPL successfully created");
        if (editor.state.compileState === CompileState.StartupRepl) {
            editor.setState({compileState: CompileState.Ready});
        } else if (editor.state.compileState === CompileState.StartupReplQueue) {
            editor.setState({compileState: CompileState.Ready});
            editor.update();
        }
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
            const chunkstrs = getChunks(editor);
            for (let i = 0; i < chunkstrs.length; i++) {
                control.fs.writeFileSync(
                    `${editor.currentFile}.chunk.${i}`,
                    chunkstrs[i]);
            }

            console.log(`Sending message to webworker to compile: ${editor.currentFile}.chunk.${editor.state.currentChunk}`)
            control.backend.compileInteraction(
                control.worker,
                `${editor.currentFile}.chunk.${editor.state.currentChunk}`)
        } else {
            editor.run(false);
        }
    };
};

export const handleTextEdit = (editor: Editor) => {
    return (value: string): void => {
        clearTimeout(editor.state.updateTimer);
        editor.setState({
            updateTimer: setTimeout(() => {
                editor.setState({currentFileContents: value});
                editor.update();
            }, 250),
        });
    };
};

export const handleChunkEdit = (editor: Editor) => {
    return (index: number, value: string): void => {
        clearTimeout(editor.state.updateTimer);
        editor.setState({
            currentChunk: index,
            updateTimer: setTimeout(() => {
                editor.setState({currentFileContents: value});
                editor.update();
            }, 250),
        });
    };
};

export const handleTraverseDown = (editor: Editor): any => {
    return (path: string[]) => {
        editor.setState({
            browsePath: path,
        });
    };
};

export const handleTraverseUp = (editor: Editor): any => {
    return (path: string[]) => {
        editor.setState({
            browsePath: path,
        });
    };
};

export const handleExpandChild = (editor: Editor): any => {
    return (child: string, fullChildPath: string): void => {
        editor.setState({
            interactions: [{
                key: "Note",
                name: "Note",
                value: "Press Run to compile and run"
            }],
            currentFileDirectory: editor.state.browsePath,
            currentFileName: child,
            currentFileContents: control.fs
                                        .readFileSync(fullChildPath, "utf-8"),
        });
    };
};

export const handleSetEditorMode = (editor: Editor) => {
    return (editorMode: EditorMode) => {
        editor.setState({ editorMode });
    };
};

export const handleToggleDropdownVisibility = (editor: Editor) => {
    return (e: any) => {
        editor.setState({
            dropdownVisible: !editor.state.dropdownVisible
        });
    };
};

export const handleToggleAutoRun = (editor: Editor) => {
    return () => {
        editor.setState({
            autoRun: !editor.state.autoRun
        });
    };
};

export const handleToggleStopify = (editor: Editor) => {
    return () => {
        if (editor.stopify) {
            editor.setState({
                runKind: control.backend.RunKind.Sync
            });
        } else {
            editor.setState({
                runKind: control.backend.RunKind.Async
            })
        }
    };
};

export const handleToggleTypeCheck = (editor: Editor) => {
    return () => {
        editor.setState({
            typeCheck: !editor.state.typeCheck
        });
    };
};

export const handleDecreaseFontSize = (editor: Editor) => {
    return () => {
        if (editor.state.fontSize > 1) {
            editor.setState({
                fontSize: editor.state.fontSize - 1
            });
        }
    };
};

export const handleIncreaseFontSize = (editor: Editor) => {
    return () => {
        editor.setState({
            fontSize: editor.state.fontSize + 1
        });
    };
};

export const handleResetFontSize = (editor: Editor) => {
    return () => {
        editor.setState({
            fontSize: 12
        });
    };
};

export const handleRemoveDropdown = (editor: Editor) => {
    return () => {
        editor.setState({
            dropdownVisible: false
        });
    };
};

export const handleSetMessage = (editor: Editor) => {
    return (newMessage: string) => {
        editor.setState({
            message: newMessage
        });
    };
};

export const handleStop = (editor: Editor) => {
    return () => {
        if (editor.state.currentRunner !== undefined) {
            editor.state.currentRunner.pause(
                (line: number) => console.log("paused on line", line))
            editor.setState({
                currentRunner: undefined,
                compileState: CompileState.Stopped
            });
        }
    };
};
