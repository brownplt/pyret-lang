import {
  AsyncProcess,
  AsyncFailure,
  AsyncSuccess,
  AsyncFailureForProcess,
  AsyncStatus,
  Action,
} from './action';

import {
  EditorMode,
  State,
  initialState,
  CHUNKSEP,
} from './state';

import {
  Chunk,
  newId,
} from './chunk';

import {
  Effect,
} from './effect';

// import {
//   applyMatchingStateUpdate,
//   guard,
//   guardUpdates,
//   SemiReducer,
//   combineSemiReducers,
//   PartialState,
// } from './semiReducer';

// const semiReducers: Array<SemiReducer<ActionType>> = [
//   guardUpdates('beginStartup', [{
//     state: CompileState.Uninitialized,
//     change: { compileState: CompileState.NeedsStartup },
//   }]),
//   guardUpdates('startupCompleted', [{
//     state: CompileState.NeedsStartup,
//     change: { compileState: CompileState.Startup },
//   }]),
//   guardUpdates('finishSetup', [
//     {
//       state: CompileState.Startup,
//       change: (state): PartialState => {
//         if (state.editorMode === EditorMode.Chunks) {
//           return { compileState: CompileState.ChunkNeedsRepl };
//         }
//         return { compileState: CompileState.Ready };
//       },
//     },
//   ]),
//   guard('queueRun', (): PartialState => ({ updateQueued: true })),
//   guardUpdates('finishCreateRepl', [
//     {
//       state: CompileState.ChunkNeedsRepl,
//       change: { compileState: CompileState.Ready },
//     },
//   ]),
//   guardUpdates('finishRun', [
//     {
//       state: CompileState.Running,
//       change: { compileState: CompileState.Ready },
//     },
//     {
//       state: CompileState.RunningWithStops,
//       change: { compileState: CompileState.Ready },
//     },
//     {
//       state: CompileState.RunningWithStopsNeedsStop,
//       change: { compileState: CompileState.Ready },
//     },
//   ]),
//   guardUpdates('stop', [
//     {
//       state: CompileState.RunningWithStops,
//       change: { compileState: CompileState.RunningWithStopsNeedsStop },
//     },
//   ]),
//   guardUpdates('compile', [
//     {
//       state: CompileState.Ready,
//       change: { compileState: CompileState.Compile, updateQueued: false },
//     },
//   ]),
//   guardUpdates('compileFailure', [
//     {
//       state: CompileState.Compile,
//       change: (state, action): PartialState => {
//       const places: any = [];
//       for (let i = 0; i < action.errors.length; i += 1) {
//         const matches = action.errors[i].match(/:\d+:\d+-\d+:\d+/g);
//         if (matches !== null) {
//           matches.forEach((m: any) => {
//             places.push(m.match(/\d+/g)!.map(Number));
//           });
//         }
//       }
//         return {
//           compileState: CompileState.Ready,
//           interactionErrors: action.errors,
//           definitionsHighlights: places,
//         };
//       },
//     },
//   ]),
//   guardUpdates('runFailure', (() => {
//     function makeRunFailureResult(newState: CompileState) {
//       return (state: State, action: ActionOfType<'runFailure'>): PartialState => ({
//         compileState: newState,
//         interactionErrors: [action.errors.toString()],
//       });
//     }
//     return [
//       {
//         state: CompileState.Running,
//         change: makeRunFailureResult(CompileState.Ready),
//       },
//       {
//         state: CompileState.RunningWithStops,
//         change: makeRunFailureResult(CompileState.Ready),
//       },
//       {
//         state: CompileState.RunningWithStopsNeedsStop,
//         change: makeRunFailureResult(CompileState.Ready),
//       },
//       {
//         state: CompileState.Compile, // TODO how does this happen?
//         change: makeRunFailureResult(CompileState.Compile),
//       },
//     ];
//   })()),
//   guard('lintFailure', (): PartialState => {
//     console.log('lintFailure not yet implemented');
//     return {};
//   }),
//   guard('lintSuccess', (): PartialState => {
//     console.log('lintSucccess not yet implemented');
//     return {};
//   }),
//   guardUpdates('compileSuccess', [
//     {
//       state: CompileState.Compile,
//       change: (state): PartialState => {
//         const newCompileState = state.updateQueued
//           ? CompileState.Ready : CompileState.NeedsRun;
//         return {
//           compileState: newCompileState,
//           interactionErrors: [],
//           definitionsHighlights: [],
//         };
//       },
//     },
//   ]),
//   guard('runFinished', (state, action): PartialState => {
//     function makeData(): PartialState {
//       if (action.result !== undefined
//           && action.result.result.error === undefined
//           && state.currentFile === undefined) {
//         throw new Error('state.currentFile should not be undefined');
//       } else if (action.result !== undefined
//                  && action.result.result.error === undefined) {
//         const results = makeResult(action.result.result, `file:// ${state.currentFile}`);
//
//         if (results[0] !== undefined
//             && results[0].name === 'error') {
//           return {
//             interactions: results,
//             checks: action.result.result.$checks,
//             interactionErrors: action.result.result.error,
//           };
//         }
//         return {
//           interactions: results,
//           checks: action.result.result.$checks,
//         };
//       } else if (action.result !== undefined) {
//         return {
//           interactionErrors: [action.result.result.error],
//         };
//       } else {
//         return {};
//       }
//     }
//
//     const data = makeData();
//
//     const makeAction = (newState: CompileState) => () => ({ compileState: newState, ...data });
//
//     const readyAction = makeAction(CompileState.Ready);
//
//     return applyMatchingStateUpdate('runFinished', state, action, [
//       {
//         state: CompileState.RunningWithStops,
//         change: readyAction,
//       },
//       {
//         state: CompileState.RunningWithStopsNeedsStop,
//         change: readyAction,
//       },
//       {
//         state: CompileState.Running,
//         change: readyAction,
//       },
//     ]);
//   }),
//   guardUpdates('runStarted', [
//     {
//       state: CompileState.NeedsRun,
//       change: { compileState: CompileState.RunningWithStops },
//     },
//   ]),
//   guard('updateContents', (state, action): PartialState => ({
//     currentFileContents: action.contents,
//     needLoadFile: false,
//     updateQueued: state.autoRun,
//   })),
//   guard('updateChunkContents', (state, action): PartialState => {
//     const chunks = [...state.chunks];
//     chunks[action.index] = {
//       startLine: chunks[action.index].startLine,
//       text: action.contents,
//       editor: chunks[action.index].editor,
//       id: chunks[action.index].id,
//     };
//     return {
//       needLoadFile: false,
//       updateQueued: state.autoRun,
//       firstUpdatableChunk: action.index,
//       chunks,
//     };
//   }),
//   guard('traverseUp', (state, action): PartialState => ({ browsePath: action.path })),
//   guard('traverseDown', (state, action): PartialState => ({ browsePath: action.path })),
//   guard('expandChild', (state, action): PartialState => ({
//     currentFile: action.path,
//     needLoadFile: true,
//   })),
//   guard('setEditorMode', (state, action): PartialState => {
//     if (action.mode === EditorMode.Text && state.editorMode === EditorMode.Chunks) {
//       if (state.chunks.length === 0) {
//         return {
//           editorMode: EditorMode.Text,
//           currentFileContents: '',
//         };
//       }
//       return {
//         editorMode: EditorMode.Text,
//         currentFileContents: state.chunks.map((chunk) => chunk.text).join(CHUNKSEP),
//       };
//     } if (action.mode === EditorMode.Chunks && state.editorMode === EditorMode.Text) {
//       if (state.currentFileContents !== undefined) {
//         let totalLines = 0;
//         const chunks: Chunk[] = [];
//
//         state.currentFileContents.split(CHUNKSEP).forEach((chunkString) => {
//           chunks.push({
//             text: chunkString,
//             startLine: totalLines,
//             editor: undefined,
//             id: newId(),
//           });
//
//           totalLines += chunkString.split('\n').length;
//         });
//
//         return {
//           editorMode: EditorMode.Chunks,
//           chunks,
//         };
//       }
//
//       return {
//         editorMode: EditorMode.Chunks,
//         chunks: [],
//       };
//     }
//     return {};
//   }),
//   guard('setChunks', (state, action): PartialState => ({
//     chunks: action.chunks,
//     needLoadFile: false,
//     updateQueued: state.autoRun,
//   })),
//   guard('setChunkIndexCounter', (state, action): PartialState => ({
//     TMPchunkIndexCounter: action.chunkIndexCounter,
//   })),
//   guard('setFocusedChunk', (state, action): PartialState => ({
//     focusedChunk: action.index,
//   })),
//   guard('unfocusChunk', (state, action): PartialState => {
//     if (state.focusedChunk === action.index) {
//       return {
//         focusedChunk: undefined,
//       };
//     }
//
//     return {};
//   }),
// ];
//
// const rootReducer = combineSemiReducers(semiReducers);
//
// export default function ideApp(state = initialState, action: Action): State {
//   return { ...rootReducer(state, action) };
// }

function handleAsyncStarted(state: State, status: AsyncStatus<AsyncProcess>): State {
  switch (status.process) {
    case 'createRepl':
      return {
        ...state,
        creatingRepl: true,
      };
    case 'lint':
      return {
        ...state,
        linting: true,
      };
    case 'compile':
      return {
        ...state,
        compiling: true,
      };
    case 'run':
      return {
        ...state,
        running: true,
      };
    default:
      throw new Error('handleAsyncStarted: unknown process');
  }
}

function handleCreateReplSuccess(state: State): State {
  return {
    ...state,
    creatingRepl: false,
    isReplReady: true,
  };
}

function handleLintSuccess(state: State): State {
  // TODO: use status.name here somehow
  return {
    ...state,
    linting: false,
  };
}

function handleCompileSuccess(state: State): State {
  return {
    ...state,
    compiling: false,
    interactionErrors: [],
    definitionsHighlights: [],
  };
}

function handleRunSuccess(state: State): State {
  // TODO
  console.log('handleRunSuccess: [mostly] ignored; (nyi)');
  return {
    ...state,
    running: false,
  };
}

function handleAsyncSuccess(state: State, status: AsyncSuccess): State {
  switch (status.process) {
    case 'createRepl':
      return handleCreateReplSuccess(state);
    case 'lint':
      return handleLintSuccess(state);
    case 'compile':
      return handleCompileSuccess(state);
    case 'run':
      return handleRunSuccess(state);
    default:
      throw new Error('handleAsyncSuccess: unknown process');
  }
}

function handleCreateReplFailure(
// state: State,
// status: AsyncFailureForProcess<'createRepl'>,
): State {
  throw new Error('handleCreateReplFailure: failed to create a REPL');
}

function handleLintFailure(state: State): State {
  console.log('handleLintFailure: ignored (nyi)');
  return {
    ...state,
    linting: false,
  };
}

function handleCompileFailure(
  state: State,
  status: AsyncFailureForProcess<'compile'>,
): State {
  const places: any = [];
  for (let i = 0; i < status.errors.length; i += 1) {
    const matches = status.errors[i].match(/:\d+:\d+-\d+:\d+/g);
    if (matches !== null) {
      matches.forEach((m: any) => {
        places.push(m.match(/\d+/g)!.map(Number));
      });
    }
  }
  return {
    ...state,
    compiling: false,
    interactionErrors: status.errors,
    definitionsHighlights: places,
  };
}

function handleRunFailure(state: State, status: AsyncFailureForProcess<'run'>) {
  return {
    ...state,
    running: false,
    interactionErrors: [status.errors.toString()],
  };
}

function handleAsyncFailure(state: State, status: AsyncFailure): State {
  switch (status.process) {
    case 'createRepl':
      return handleCreateReplFailure();
    case 'lint':
      return handleLintFailure(state);
    case 'compile':
      return handleCompileFailure(state, status);
    case 'run':
      return handleRunFailure(state, status);
    default:
      throw new Error('handleAsyncFailure: unknown process');
  }
}

function handleSetAsyncStatus(state: State, status: AsyncStatus<AsyncProcess>): State {
  switch (status.status) {
    case 'started':
      return handleAsyncStarted(state, status);
    case 'succeeded':
      return handleAsyncSuccess(state, status);
    case 'failed':
      return handleAsyncFailure(state, status);
    default:
      throw new Error('handleSetAsyncStatus: unknown status');
  }
}

function handleQueueEffect(state: State, effect: Effect): State {
  const { effectQueue } = state;
  return {
    ...state,
    effectQueue: [...effectQueue, effect],
  };
}

function handleSetEditorMode(state: State, newEditorMode: EditorMode): State {
  const { editorMode } = state;

  if (newEditorMode === EditorMode.Text && editorMode === EditorMode.Chunks) {
    const { chunks } = state;
    if (chunks.length === 0) {
      return {
        ...state,
        editorMode: EditorMode.Text,
        currentFileContents: '',
      };
    }
    return {
      ...state,
      currentFileContents: chunks.map((chunk) => chunk.text).join(CHUNKSEP),
    };
  }

  if (newEditorMode === EditorMode.Chunks && editorMode === EditorMode.Text) {
    const { currentFileContents } = state;
    if (currentFileContents === undefined) {
      return {
        ...state,
        editorMode: EditorMode.Chunks,
        chunks: [],
      };
    }

    let totalLines = 0;
    const chunks: Chunk[] = [];

    currentFileContents.split(CHUNKSEP).forEach((chunkString) => {
      chunks.push({
        text: chunkString,
        startLine: totalLines,
        id: newId(),
      });

      totalLines += chunkString.split('\n').length;
    });

    return {
      ...state,
      editorMode: EditorMode.Chunks,
      chunks,
    };
  }

  return state;
}

function handleSetCurrentRunner(state: State, runner: any): State {
  return {
    ...state,
    currentRunner: runner,
  };
}

function handleSetCurrentFileContents(state: State, contents: string): State {
  const { effectQueue } = state;
  return {
    ...state,
    currentFileContents: contents,
    effectQueue: [...effectQueue, 'saveFile'],
  };
}

function handleSetBrowsePath(state: State, path: string): State {
  return {
    ...state,
    browsePath: path,
  };
}

function handleSetCurrentFile(state: State, file: string): State {
  const { effectQueue } = state;
  return {
    ...state,
    currentFile: file,
    effectQueue: [...effectQueue, 'loadFile'],
  };
}

function handleSetChunks(state: State, chunks: Chunk[]): State {
  return { ...state, chunks };
}

function handleSetFocusedChunk(state: State, index: number): State {
  return { ...state, focusedChunk: index };
}

function handleSetEffectQueue(state: State, newEffectQueue: Effect[]): State {
  return { ...state, effectQueue: newEffectQueue };
}

function handleUpdate(
  state: State,
  key: any, // TODO: fix types
  value: any,
): State {
  switch (key) {
    case 'editorMode':
      return handleSetEditorMode(state, value);
    case 'currentRunner':
      return handleSetCurrentRunner(state, value);
    case 'currentFileContents':
      return handleSetCurrentFileContents(state, value);
    case 'browsePath':
      return handleSetBrowsePath(state, value);
    case 'currentFile':
      return handleSetCurrentFile(state, value);
    case 'chunks':
      return handleSetChunks(state, value);
    case 'focusedChunk':
      return handleSetFocusedChunk(state, value);
    case 'effectQueue':
      return handleSetEffectQueue(state, value);
    default:
      throw new Error(`handleUpdate: unknown key ${key}`);
  }
}

function rootReducer(state: State, action: Action): State {
  switch (action.type) {
    case 'setAsyncStatus':
      return handleSetAsyncStatus(state, action);
    case 'queueEffect':
      return handleQueueEffect(state, action.effect);
    case 'update':
      return handleUpdate(state, action.key, action.value);
    default:
      return state;
  }
}

export default function ideApp(state = initialState, action: Action): State {
  return rootReducer(state, action);
}
