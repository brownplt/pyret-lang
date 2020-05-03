import {
  EffectFailure,
  EffectSuccess,
  EffectEnded,
  FailureForEffect,
  Update,
  EffectStarted,
  Action,
  SuccessForEffect,
  EnqueueEffect,
} from './action';

import {
  EditorMode,
  State,
  initialState,
  CHUNKSEP,
  makeResult,
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

function handleEffectStarted(state: State, action: EffectStarted): State {
  const oldEffectQueue = state.effectQueue;

  if (oldEffectQueue[action.effect] === undefined) {
    const message = `handleEffectStarted: effect to remove is out of bounds${
      JSON.stringify(action)}`;
    throw new Error(message);
  }

  const effectQueue = [
    ...oldEffectQueue.slice(0, action.effect),
    ...oldEffectQueue.slice(action.effect + 1, oldEffectQueue.length),
  ];

  switch (oldEffectQueue[action.effect]) {
    case 'createRepl':
      return {
        ...state,
        creatingRepl: true,
        effectQueue,
      };
    case 'lint':
      return {
        ...state,
        linting: true,
        effectQueue,
      };
    case 'compile':
      return {
        ...state,
        compiling: true,
        effectQueue,
      };
    case 'run':
      return {
        ...state,
        running: true,
        effectQueue,
      };
    default:
      return {
        ...state,
        effectQueue,
      };
  }
}

function handleCreateReplSuccess(state: State): State {
  return {
    ...state,
    creatingRepl: false,
    isReplReady: true,
  };
}

function handleLintSuccess(state: State, action: SuccessForEffect<'lint'>): State {
  console.log('lint success', action);
  return {
    ...state,
    linting: false,
  };
}

function handleCompileSuccess(state: State): State {
  const { autoRun, effectQueue } = state;
  return {
    ...state,
    compiling: false,
    interactionErrors: [],
    definitionsHighlights: [],
    effectQueue: autoRun ? [...effectQueue, 'run'] : effectQueue,
  };
}

function handleRunSuccess(state: State, status: SuccessForEffect<'run'>): State {
  const results = makeResult(status.result.result, `file://${state.currentFile}`);
  return {
    ...state,
    running: false,
    interactions: results,
    checks: status.result.result.$checks,
  };
}

function handleSetupSuccess(state: State): State {
  return {
    ...state,
    isSetupFinished: true,
    settingUp: false,
  };
}

function handleStopSuccess(state: State, action: SuccessForEffect<'stop'>): State {
  console.log('stop successful, paused on line', action.line);
  return {
    ...state,
    running: false,
  };
}

function handleLoadFileSuccess(state: State): State {
  console.log('loaded a file successfully');
  return {
    ...state,
  };
}

function handleSaveFileSuccess(state: State): State {
  console.log('saved a file successfully');
  return {
    ...state,
  };
}

function handleSetupWorkerMessageHandlerSuccess(state: State): State {
  return {
    ...state,
    isMessageHandlerReady: true,
  };
}

function handleEffectSucceeded(state: State, action: EffectSuccess): State {
  switch (action.effect) {
    case 'createRepl':
      return handleCreateReplSuccess(state);
    case 'lint':
      return handleLintSuccess(state, action);
    case 'compile':
      return handleCompileSuccess(state);
    case 'run':
      return handleRunSuccess(state, action);
    case 'setup':
      return handleSetupSuccess(state);
    case 'stop':
      return handleStopSuccess(state, action);
    case 'loadFile':
      return handleLoadFileSuccess(state);
    case 'saveFile':
      return handleSaveFileSuccess(state);
    case 'setupWorkerMessageHandler':
      return handleSetupWorkerMessageHandlerSuccess(state);
    default:
      throw new Error(`handleAsyncSuccess: unknown process ${JSON.stringify(action)}`);
  }
}

function handleCreateReplFailure(): State {
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
  status: FailureForEffect<'compile'>,
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

function handleRunFailure(state: State, status: FailureForEffect<'run'>) {
  console.log('handleFailure', status);
  return {
    ...state,
    running: false,
    interactionErrors: [JSON.stringify(status.errors)],
  };
}

function handleEffectFailed(state: State, action: EffectFailure): State {
  switch (action.effect) {
    case 'createRepl':
      return handleCreateReplFailure();
    case 'lint':
      return handleLintFailure(state);
    case 'compile':
      return handleCompileFailure(state, action);
    case 'run':
      return handleRunFailure(state, action);
    default:
      throw new Error(`handleEffectFailed: unknown effect ${JSON.stringify(action)}`);
  }
}

function handleEffectEnded(state: State, action: EffectEnded): State {
  switch (action.status) {
    case 'succeeded':
      return handleEffectSucceeded(state, action);
    case 'failed':
      return handleEffectFailed(state, action);
    default:
      throw new Error(`handleEffectEnded: unknown action ${JSON.stringify(action)}`);
  }
}

function handleEnqueueEffect(state: State, action: EnqueueEffect): State {
  const { effectQueue } = state;
  return {
    ...state,
    effectQueue: [...effectQueue, action.effect],
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
  const contents = chunks.map((chunk) => chunk.text).join(CHUNKSEP);

  const {
    effectQueue,
    autoRun,
    compiling,
    running,
  } = state;

  function getNewEffectQueue(): Effect[] {
    if (autoRun && !compiling && !running) {
      return [...effectQueue, 'saveFile', 'compile'];
    }

    return [...effectQueue, 'saveFile'];
  }

  return {
    ...state,
    chunks,
    currentFileContents: contents,
    effectQueue: getNewEffectQueue(),
  };
}

function handleSetFocusedChunk(state: State, index: number): State {
  return { ...state, focusedChunk: index };
}

function handleUpdate(
  state: State,
  action: Update,
): State {
  switch (action.key) {
    case 'editorMode':
      return handleSetEditorMode(state, action.value);
    case 'currentRunner':
      return handleSetCurrentRunner(state, action.value);
    case 'currentFileContents':
      return handleSetCurrentFileContents(state, action.value);
    case 'browsePath':
      return handleSetBrowsePath(state, action.value);
    case 'currentFile':
      return handleSetCurrentFile(state, action.value);
    case 'chunks':
      return handleSetChunks(state, action.value);
    case 'focusedChunk':
      return handleSetFocusedChunk(state, action.value);
    default:
      throw new Error(`handleUpdate: unknown action ${action}`);
  }
}

function rootReducer(state: State, action: Action): State {
  switch (action.type) {
    case 'effectStarted':
      return handleEffectStarted(state, action);
    case 'effectEnded':
      return handleEffectEnded(state, action);
    case 'enqueueEffect':
      return handleEnqueueEffect(state, action);
    case 'update':
      return handleUpdate(state, action);
    default:
      return state;
  }
}

export default function ideApp(state = initialState, action: Action): State {
  return rootReducer(state, action);
}
