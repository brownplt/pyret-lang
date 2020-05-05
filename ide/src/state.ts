// This file is used to track the state of the editor. Any function that calls
// .setState on an Editor should be written here

import { Check } from './Check';
import { Chunk } from './chunk';
import { Effect } from './effect';
import * as control from './control';

export type State = {
  effectQueue: Effect[];
  browseRoot: string,
  browsePath: string,
  currentFile: string,
  currentFileContents: string | undefined,
  typeCheck: boolean,
  checks: Check[],
  interactions: { key: any, name: any, value: any }[],
  interactionErrors: string[],
  lintFailures: LintFailures,
  runKind: control.backend.RunKind,
  autoRun: boolean,
  editTimer: NodeJS.Timer | false,
  dropdownVisible: boolean,
  editorMode: EditorMode,
  fontSize: number,
  message: string,
  definitionsHighlights: number[][],
  fsBrowserVisible: boolean,
  currentRunner: any,
  currentChunk: number,
  firstUpdatableChunk: number | undefined,
  chunks: Chunk[],
  focusedChunk: number | undefined,

  isFileSaved: boolean,
  isSetupFinished: boolean,
  isMessageHandlerReady: boolean,
  isReplReady: boolean,
  settingUp: boolean,
  creatingRepl: boolean,
  linting: boolean,
  linted: boolean,
  compiling: boolean | 'out-of-date',
  running: boolean,
  compiledSinceLastEdit: boolean,
};

export enum EditorMode {
  Chunks,
  Text,
}

export type LintFailure = {
  name: string,
  errors: string[]
};

export type LintFailures = {
  [name : string]: LintFailure
};

export const initialState: State = {
  browseRoot: '/',
  browsePath: '/projects',
  currentFile: '/projects/program.arr',
  currentFileContents: undefined,
  typeCheck: true,
  checks: [],
  interactions: [{
    key: 'Note',
    name: 'Note',
    value: 'Press Run to compile and run',
  }],
  interactionErrors: [],
  lintFailures: {},
  runKind: control.backend.RunKind.Async,
  autoRun: true,
  editTimer: false,
  dropdownVisible: false,
  editorMode: EditorMode.Chunks,
  fontSize: 12,
  message: 'Ready to rock',
  definitionsHighlights: [],
  fsBrowserVisible: false,
  currentRunner: undefined,
  currentChunk: 0,
  firstUpdatableChunk: 0,

  effectQueue: [],
  isFileSaved: false,
  isMessageHandlerReady: false,
  isReplReady: false,
  isSetupFinished: false,
  settingUp: true,
  creatingRepl: false,
  linting: false,
  linted: false,
  compiling: false,
  running: false,
  compiledSinceLastEdit: false,

  chunks: [],
  focusedChunk: undefined,
};

export const CHUNKSEP = '#.CHUNK#\n';

export const makeResult = (
  result: any,
  moduleUri: string,
): { key: string, name: string, value: any }[] => {
  const compareLocations = (a: any, b: any): number => a.srcloc[1] - b.srcloc[1];

  // There may be toplevel expressions in many modules, but we only want to
  // show the ones from the main module we're working on
  const mainTraces = result.$traces.filter((t : any) => t.srcloc[0] === moduleUri);

  const allWithLocs = result.$locations.concat(mainTraces);

  // We combine and then sort to get the traces interleaved correctly with named values
  const allSorted = allWithLocs.sort(compareLocations);
  return allSorted.map((key: any) => {
    if ('name' in key) {
      return {
        name: key.name,
        key: key.name,
        line: key.srcloc[1],
        value: result[key.name],
      };
    }

    return {
      name: '',
      key: String(key.srcloc[1]),
      line: key.srcloc[1],
      value: key.value,
    };
  });
};
/*
 * const getChunks = (editor: Editor) => {
 *     return editor.state.currentFileContents.split(CHUNKSEP);
 * };
 *
 * export const handleCompileSuccess = (editor: Editor) => {
 *     return () => {
 *         if (editor.state.editorMode === EditorMode.Text) {
 *             console.log("COMPILE SUCCESS");
 *         } else if (editor.state.editorMode === EditorMode.Chunks) {
 *             console.log(`COMPILE SUCCESS (chunk #${editor.state.currentChunk})`);
 *         }
 *
 *         if (editor.state.compileState === CompileState.Compile) {
 *             editor.setState({compileState: CompileState.Ready});
 *
 *             if (editor.state.editorMode === EditorMode.Chunks) {
 *                 const numberOfChunks = getChunks(editor).length;
 *                 if (editor.state.currentChunk < numberOfChunks) {
 *                     editor.setState(
 *                         {currentChunk: editor.state.currentChunk + 1},
 *                         () => editor.update());
 *                 }
 *             }
 *         } else if (editor.state.compileState === CompileState.CompileQueue
 *                    || editor.state.compileState === CompileState.CompileRunQueue) {
 *             editor.setState({compileState: CompileState.Ready});
 *             editor.update();
 *         } else if (editor.state.compileState === CompileState.CompileRun) {
 *             if (editor.state.editorMode === EditorMode.Text) {
 *                 if (editor.stopify) {
 *                     editor.setState({compileState: CompileState.RunningWithStops});
 *                 } else {
 *                     editor.setState({compileState: CompileState.RunningWithoutStops});
 *                 }
 *                 const x = new Date();
 *                 console.log(`Run ${x} started`);
 *                 control.run(
 *                     control.path.runBase,
 *                     control.path.runProgram,
 *                     (runResult: any) => {
 *                         editor.setState({compileState: CompileState.Ready});
 *                         console.log(`Run ${x} finished`);
 *                         console.log(runResult);
 *                         if (runResult.result !== undefined) {
 *                             if (runResult.result.error === undefined) {
 *                                 const results =
 *                                     makeResult(
 *                                         runResult.result,
 *                                         'file://' +
 *                                         control.bfsSetup.path.join(
 *                                             control.path.compileBase,
 *                                             editor.state.currentFileName));
 *                                 const checks = runResult.result.$checks;
 *                                 editor.setState({
 *                                     interactions: results,
 *                                     checks: checks
 *                                 });
 *
 *                                 if (results[0] !== undefined && results[0].name === "error") {
 *                                     editor.setState(
 *                                         {
 *                                             interactionErrors: runResult.result.error,
 *                                         }
 *                                     );
 *                                 }
 *                             } else {
 *                                 editor.setState({
 *                                     interactionErrors: [runResult.result.error],
 *                                 });
 *                             }
 *                         }
 *                     },
 *                     (runner: any) => {
 *                         editor.setState({currentRunner: runner});
 *                     },
 *                     editor.state.runKind);
 *             } else if (editor.state.editorMode === EditorMode.Chunks) {
 *                 console.log("Skipping run (not yet implemented for chunks)");
 *                 const numberOfChunks = getChunks(editor).length;
 *                 if (editor.state.currentChunk < numberOfChunks) {
 *                     editor.setState({
 *                         currentChunk: editor.state.currentChunk + 1,
 *                         compileState: CompileState.Ready
 *                     });
 *                     editor.update();
 *                 }
 *             }
 *         } else {
 *             invalidCompileState(editor.state.compileState);
 *         }
 *     };
 * };
 *
 * export const handleCreateReplSuccess = (editor: Editor) => {
 *     return () => {
 *         console.log("REPL successfully created");
 *         if (editor.state.compileState === CompileState.StartupRepl) {
 *             editor.setState({compileState: CompileState.Ready});
 *         } else if (editor.state.compileState === CompileState.StartupReplQueue) {
 *             editor.setState({compileState: CompileState.Ready});
 *             editor.update();
 *         }
 *     };
 * };
 *
 * export const handleCompileInteractionSuccess = (editor: Editor) => {
 *     return (response: { program: string }) => {
 *         console.log(`Chunk ${response.program} successfully compiled.`);
 *         return;
 *     };
 * };
 *
 * export const handleCompileInteractionFailure = (editor: Editor) => {
 *     return (response: { program: string }) => {
 *         console.error(`Failed to compile ${response.program}.`);
 *         return;
 *     };
 * };
 *
 * export const handleRun = (editor: Editor) => {
 *     return (runAfterwards: boolean) => {
 *         editor.setState(
 *             {
 *                 interactionErrors: [],
 *                 definitionsHighlights: []
 *             }
 *         );
 *         if (editor.isPyretFile) {
 *             if (editor.state.compileState === CompileState.Startup) {
 *                 editor.setState({compileState: CompileState.StartupQueue});
 *             } else if (editor.state.compileState === CompileState.StartupQueue) {
 *                 // state remains as StartupQueue
 *             } else if (editor.state.compileState === CompileState.Ready
 *                        || editor.state.compileState === CompileState.Stopped) {
 *                 if (runAfterwards || editor.state.autoRun) {
 *                     editor.setState({compileState: CompileState.CompileRun});
 *                 } else {
 *                     editor.setState({compileState: CompileState.Compile});
 *                 }
 *                 control.compile(
 *                     editor.currentFileDirectory,
 *                     editor.currentFileName,
 *                     editor.state.typeCheck);
 *             } else if (editor.state.compileState === CompileState.Compile) {
 *                 editor.setState({compileState: CompileState.CompileQueue});
 *             } else if (editor.state.compileState === CompileState.CompileRun) {
 *                 editor.setState({compileState: CompileState.CompileRunQueue});
 *             } else if (editor.state.compileState === CompileState.CompileQueue) {
 *                 // state remains as CompileQueue
 *             } else if (editor.state.compileState === CompileState.CompileRunQueue) {
 *                 // state remains as CompileRunQueue
 *             } else if (editor.state.compileState === CompileState.RunningWithStops) {
 *                 editor.stop();
 *                 editor.update();
 *                 // state remains as RunningWithStops
 *             } else if (editor.state.compileState === CompileState.RunningWithoutStops) {
 *                 // state remains as RunningWithoutStops
 *             } else {
 *                 invalidCompileState(editor.state.compileState);
 *             }
 *         } else {
 *             editor.setState({
 *                 interactions: [
 *                     {
 *                         key: "Error",
 *                         name: "Error",
 *                         value: "Run is not supported on editor file type"
 *                     },
 *                     {
 *                         key: "File",
 *                         name: "File",
 *                         value: editor.currentFile
 *                 }],
 *                 interactionErrors: ["Error: Run is not supported on editor file type"],
 *             });
 *         }
 *     };
 * };
 *
 * export const handleUpdate = (editor: Editor) => {
 *     return (): void => {
 *         control.fs.writeFileSync(
 *             editor.currentFile,
 *             editor.state.currentFileContents);
 *
 *         if (editor.state.editorMode === EditorMode.Chunks) {
 *             const chunkstrs = getChunks(editor);
 *             for (let i = 0; i < chunkstrs.length; i++) {
 *                 control.fs.writeFileSync(
 *                     `${editor.currentFile}.chunk.${i}`,
 *                     chunkstrs[i]);
 *             }
 *
 *             control.backend.compileInteraction(
 *                 control.worker,
 *                 `${editor.currentFile}.chunk.${editor.state.currentChunk}`)
 *         } else {
 *             editor.run(false);
 *         }
 *     };
 * };
 *
 * export const handleTextEdit = (editor: Editor) => {
 *     return (value: string): void => {
 *         clearTimeout(editor.state.updateTimer);
 *         editor.setState({
 *             updateTimer: setTimeout(() => {
 *                 editor.setState({currentFileContents: value});
 *                 editor.update();
 *             }, 250),
 *         });
 *     };
 * };
 *
 * export const handleChunkEdit = (editor: Editor) => {
 *     return (index: number, value: string): void => {
 *         clearTimeout(editor.state.updateTimer);
 *         editor.setState({
 *             currentChunk: index,
 *             updateTimer: setTimeout(() => {
 *                 editor.setState({currentFileContents: value});
 *                 editor.update();
 *             }, 250),
 *         });
 *     };
 * };
 *
 * export const handleTraverseDown = (editor: Editor): any => {
 *     return (path: string[]) => {
 *         editor.setState({
 *             browsePath: path,
 *         });
 *     };
 * };
 *
 * export const handleTraverseUp = (editor: Editor): any => {
 *     return (path: string[]) => {
 *         editor.setState({
 *             browsePath: path,
 *         });
 *     };
 * };
 *
 * export const handleExpandChild = (editor: Editor): any => {
 *     return (child: string, fullChildPath: string): void => {
 *         editor.setState({
 *             interactions: [{
 *                 key: "Note",
 *                 name: "Note",
 *                 value: "Press Run to compile and run"
 *             }],
 *             currentFileDirectory: editor.state.browsePath,
 *             currentFileName: child,
 *             currentFileContents: control.fs
 *                                         .readFileSync(fullChildPath, "utf-8"),
 *         });
 *     };
 * };
 *
 * export const handleSetEditorMode = (editor: Editor) => {
 *     return (editorMode: EditorMode) => {
 *         editor.setState({ editorMode });
 *     };
 * };
 *
 * export const handleToggleDropdownVisibility = (editor: Editor) => {
 *     return (e: any) => {
 *         editor.setState({
 *             dropdownVisible: !editor.state.dropdownVisible
 *         });
 *     };
 * };
 *
 * export const handleToggleAutoRun = (editor: Editor) => {
 *     return () => {
 *         editor.setState({
 *             autoRun: !editor.state.autoRun
 *         });
 *     };
 * };
 *
 * export const handleToggleStopify = (editor: Editor) => {
 *     return () => {
 *         if (editor.stopify) {
 *             editor.setState({
 *                 runKind: control.backend.RunKind.Sync
 *             });
 *         } else {
 *             editor.setState({
 *                 runKind: control.backend.RunKind.Async
 *             })
 *         }
 *     };
 * };
 *
 * export const handleToggleTypeCheck = (editor: Editor) => {
 *     return () => {
 *         editor.setState({
 *             typeCheck: !editor.state.typeCheck
 *         });
 *     };
 * };
 *
 * export const handleDecreaseFontSize = (editor: Editor) => {
 *     return () => {
 *         if (editor.state.fontSize > 1) {
 *             editor.setState({
 *                 fontSize: editor.state.fontSize - 1
 *             });
 *         }
 *     };
 * };
 *
 * export const handleIncreaseFontSize = (editor: Editor) => {
 *     return () => {
 *         editor.setState({
 *             fontSize: editor.state.fontSize + 1
 *         });
 *     };
 * };
 *
 * export const handleResetFontSize = (editor: Editor) => {
 *     return () => {
 *         editor.setState({
 *             fontSize: 12
 *         });
 *     };
 * };
 *
 * export const handleRemoveDropdown = (editor: Editor) => {
 *     return () => {
 *         editor.setState({
 *             dropdownVisible: false
 *         });
 *     };
 * };
 *
 * export const handleSetMessage = (editor: Editor) => {
 *     return (newMessage: string) => {
 *         editor.setState({
 *             message: newMessage
 *         });
 *     };
 * };
 *
 * export const handleStop = (editor: Editor) => {
 *     return () => {
 *         if (editor.state.currentRunner !== undefined) {
 *             editor.state.currentRunner.pause(
 *                 (line: number) => console.log("paused on line", line))
 *             editor.setState({
 *                 currentRunner: undefined,
 *                 compileState: CompileState.Stopped
 *             });
 *         }
 *     };
 * }; */
