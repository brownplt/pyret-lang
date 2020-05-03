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

function handleStartEditTimerSuccess(
  state: State,
  action: SuccessForEffect<'startEditTimer'>,
): State {
  return {
    ...state,
    editTimer: action.timer,
  };
}

function handleEditTimerSuccess(state: State): State {
  const {
    effectQueue,
  } = state;

  return {
    ...state,
    effectQueue: [...effectQueue, 'saveFile'],
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
  const { compiling, autoRun, effectQueue } = state;

  if (compiling === 'out-of-date') {
    return {
      ...state,
      compiling: false,
      effectQueue: [...effectQueue, 'saveFile'],
    };
  }

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
  const {
    effectQueue,
    compiling,
    running,
    autoRun,
  } = state;

  function getNewEffectQueue(): Effect[] {
    if (autoRun && !compiling && !running) {
      return [...effectQueue, 'compile'];
    }

    return effectQueue;
  }

  return {
    ...state,
    isFileSaved: true,
    effectQueue: getNewEffectQueue(),
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
    case 'startEditTimer':
      return handleStartEditTimerSuccess(state, action);
    case 'editTimer':
      return handleEditTimerSuccess(state);
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
      throw new Error(`handleEffectSucceeded: unknown process ${JSON.stringify(action)}`);
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
  const { compiling } = state;
  if (compiling === 'out-of-date') {
    const { effectQueue } = state;
    return {
      ...state,
      compiling: false,
      effectQueue: [...effectQueue, 'saveFile'],
    };
  }

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
    // we already keep currentFileContents in sync with chunk contents while
    // in chunk mode, since we need it to save the file contents.
    return {
      ...state,
      editorMode: EditorMode.Text,
    };
  }

  if (newEditorMode === EditorMode.Chunks && editorMode === EditorMode.Text) {
    // in text mode currentFileContents can be more up-to-date than chunks, so we
    // need to recreate the chunks.

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
  const {
    effectQueue,
    compiling,
    editorMode,
  } = state;

  if (editorMode !== EditorMode.Text) {
    throw new Error('handleSetCurrentFileContents: not in text mode');
  }

  return {
    ...state,
    currentFileContents: contents,
    effectQueue: [...effectQueue, 'startEditTimer'],
    isFileSaved: false,
    compiling: compiling ? 'out-of-date' : false,
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
    compiling,
    editorMode,
  } = state;

  if (editorMode !== EditorMode.Chunks) {
    throw new Error('handleSetChunks: not in chunk mode');
  }

  return {
    ...state,
    chunks,
    currentFileContents: contents,
    effectQueue: [...effectQueue, 'startEditTimer'],
    isFileSaved: false,
    compiling: compiling ? 'out-of-date' : false,
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
