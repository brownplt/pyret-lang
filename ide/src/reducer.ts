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
  ChunksUpdate,
  isMultipleChunkUpdate,
  isSingleChunkUpdate,
} from './action';

import {
  EditorMode,
  State,
  initialState,
  CHUNKSEP,
} from './state';

import {
  Chunk,
  getStartLineForIndex,
  findChunkFromSrcloc,
  emptyChunk,
  lintSuccessState,
  notLintedState,
} from './chunk';

import {
  makeRHSObjects,
} from './rhsObject';

function handleEnter(state: State): State {
  const {
    focusedChunk,
    shouldAdvanceCursor,
    chunks,
  } = state;

  if (focusedChunk !== undefined
    && shouldAdvanceCursor
    && chunks[focusedChunk] !== undefined
    && chunks[focusedChunk].errorState.status !== 'failed') {
    if (focusedChunk + 1 === chunks.length) {
      const nextChunks: Chunk[] = [
        ...chunks,
        emptyChunk({
          startLine: getStartLineForIndex(chunks, focusedChunk + 1),
          errorState: lintSuccessState,
        }),
      ];
      return {
        ...state,
        chunks: nextChunks,
        focusedChunk: focusedChunk + 1,
        shouldAdvanceCursor: false,
      };
    }

    if (chunks[focusedChunk + 1].text.trim() !== '') {
      const nextChunks: Chunk[] = [
        ...chunks.slice(0, focusedChunk + 1),
        emptyChunk({
          startLine: getStartLineForIndex(chunks, focusedChunk + 1),
          errorState: lintSuccessState,
        }),
        ...chunks.slice(focusedChunk + 1),
      ];
      for (let i = focusedChunk + 1; i < nextChunks.length; i += 1) {
        nextChunks[i] = {
          ...nextChunks[i],
          startLine: getStartLineForIndex(nextChunks, i),
        };
      }
      return {
        ...state,
        chunks: nextChunks,
        focusedChunk: focusedChunk + 1,
        shouldAdvanceCursor: false,
      };
    }

    if (chunks[focusedChunk + 1].text.trim() === '') {
      return {
        ...state,
        focusedChunk: focusedChunk + 1,
        shouldAdvanceCursor: false,
        chunks,
      };
    }
  }

  return state;
}

function handleEffectStarted(state: State, action: EffectStarted): State {
  const oldEffectQueue = state.effectQueue;

  if (oldEffectQueue[action.effect] === undefined) {
    const message = `handleEffectStarted: effect to remove is out of bounds${JSON.stringify(action)}`;
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
  const { editorMode } = state;

  switch (editorMode) {
    case EditorMode.Text: {
      const { effectQueue } = state;
      return {
        ...state,
        linting: false,
        linted: true,
        effectQueue: [...effectQueue, 'compile'],
      };
    }
    case EditorMode.Chunks: {
      const {
        chunks,
        effectQueue,
        compiling,
        running,
      } = state;

      let allLinted = true;
      const newChunks: Chunk[] = chunks.map((chunk) => {
        if (chunk.id === action.name) {
          return {
            ...chunk,
            errorState: { status: 'succeeded', effect: 'lint' },
          };
        }

        if (chunk.errorState.status !== 'succeeded') {
          allLinted = false;
        }

        return chunk;
      });

      const shouldCompile = allLinted && !compiling && !running;

      return handleEnter({
        ...state,
        chunks: newChunks,
        linted: allLinted,
        linting: !allLinted,
        effectQueue: shouldCompile ? [...effectQueue, 'compile'] : effectQueue,
      });
    }
    default:
      throw new Error('handleLintSuccess: unknown editor mode');
  }
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
  console.log('run result', status);
  const rhs = makeRHSObjects(status.result, `file://${state.currentFile}`);

  const {
    chunks,
    currentFile,
  } = state;

  const newChunks = chunks.slice();
  const locations = status.result.result.$locations;
  const traces = status.result.result.$traces;

  locations.forEach((loc: any) => {
    const { name, srcloc } = loc;
    const chunk = findChunkFromSrcloc(chunks, srcloc, currentFile);
    if (chunk !== false) {
      newChunks[chunk].errorState = {
        status: 'succeeded',
        effect: 'run',
        result: status.result.result[name],
      };
    }
  });

  traces.forEach((loc: any) => {
    const { value, srcloc } = loc;
    const chunk = findChunkFromSrcloc(chunks, srcloc, currentFile);
    if (chunk !== false) {
      newChunks[chunk].errorState = {
        status: 'succeeded',
        effect: 'run',
        result: value,
      };
    }
  });

  return handleEnter({
    ...state,
    chunks: newChunks,
    running: false,
    rhs,
  });
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
    autoRun,
    compiling,
    running,
    chunks,
  } = state;

  let newEffectQueue = effectQueue;
  let needsLint = false;
  let shouldHandleEnter = false;

  for (let i = 0; i < chunks.length; i += 1) {
    if (chunks[i].errorState.status !== 'succeeded') {
      needsLint = true;
      break;
    }
  }

  if (autoRun && compiling !== true && !running) {
    if (needsLint) {
      newEffectQueue = [...effectQueue, 'lint'];
    } else if (autoRun) {
      // Chunks are inserted after a lint success. In this case, we aren't
      // linting, but we still would like to possibly create a new chunk.
      shouldHandleEnter = true;
      newEffectQueue = [...effectQueue, 'compile'];
    }
  }

  if (shouldHandleEnter) {
    // Make sure not to lint / compile if all of the following are true:
    // 1. enter was pressed
    // 2. the current chunk had no text in it
    // 3. the current chunk was the last chunk in the file
    const newState = handleEnter({
      ...state,
      isFileSaved: true,
      effectQueue: newEffectQueue,
      linted: true,
    });
    if (newState.chunks.length === state.chunks.length + 1
      && newState.focusedChunk === newState.chunks.length - 1
      && newState.chunks[newState.chunks.length - 2].text === '') {
      return { ...newState, effectQueue };
    }
    return newState;
  }

  return {
    ...state,
    isFileSaved: true,
    effectQueue: newEffectQueue,
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

function handleLintFailure(state: State, action: FailureForEffect<'lint'>): State {
  const { editorMode, focusedChunk, shouldAdvanceCursor } = state;

  switch (editorMode) {
    case EditorMode.Text:
      return {
        ...state,
        linted: true,
        linting: false,
        interactionErrors: action.errors,
      };
    case EditorMode.Chunks: {
      const { chunks } = state;

      let allLinted = true;
      let currentChunkFailed = false;
      const newChunks: Chunk[] = chunks.map((chunk, chunkIndex) => {
        if (chunk.id === action.name) {
          const highlights: number[][] = [];
          for (let i = 0; i < action.errors.length; i += 1) {
            const matches = action.errors[i].match(/:\d+:\d+-\d+:\d+/g);
            if (matches !== null) {
              matches.forEach((m: any) => {
                highlights.push(m.match(/\d+/g)!.map(Number));
              });
            }
          }

          if (chunkIndex === focusedChunk) {
            currentChunkFailed = true;
          }

          return {
            ...chunk,
            errorState: {
              status: 'failed',
              effect: 'lint',
              failures: action.errors,
              highlights,
            },
            needsJiggle: true,
          };
        }

        if (chunk.errorState.status === 'notLinted') {
          allLinted = false;
        }

        return chunk;
      });

      return handleEnter({
        ...state,
        chunks: newChunks,
        linted: allLinted,
        linting: !allLinted,
        shouldAdvanceCursor: shouldAdvanceCursor && !currentChunkFailed,
      });
    }
    default:
      throw new Error('handleLintFailure: unknown editor mode');
  }
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

  const { editorMode } = state;

  const places: number[][] = [];
  for (let i = 0; i < status.errors.length; i += 1) {
    const matches = status.errors[i].match(/:\d+:\d+-\d+:\d+/g);
    if (matches !== null) {
      matches.forEach((m: any) => {
        places.push(m.match(/\d+/g)!.map(Number));
      });
    }
  }

  function findChunkFromSrclocResult([l1] : number[]): number | false {
    const { chunks } = state;
    for (let i = 0; i < chunks.length; i += 1) {
      const end = chunks[i].startLine + chunks[i].text.split('\n').length;
      if (l1 >= chunks[i].startLine && l1 <= end) {
        return i;
      }
    }
    return false;
  }

  function getExistingHighlights(chunk : Chunk): number[][] | false {
    if (chunk.errorState.status === 'failed') {
      return chunk.errorState.highlights;
    }

    return false;
  }

  switch (editorMode) {
    case EditorMode.Text:
      return {
        ...state,
        compiling: false,
        interactionErrors: status.errors,
        definitionsHighlights: places,
      };
    case EditorMode.Chunks: {
      if (places.length > 0) {
        const { chunks } = state;
        const newChunks = [...chunks];
        for (let i = 0; i < places.length; i += 1) {
          const chunkIndex = findChunkFromSrclocResult(places[i]);
          if (chunkIndex) {
            const hl = getExistingHighlights(newChunks[chunkIndex]);
            newChunks[chunkIndex] = {
              ...newChunks[chunkIndex],
              errorState: {
                status: 'failed',
                effect: 'compile',
                failures: status.errors,
                highlights: hl ? [...hl, places[i]] : [places[i]],
              },
              needsJiggle: true,
            };
          }
        }
        return handleEnter({
          ...state,
          compiling: false,
          chunks: newChunks,
        });
      }
      return handleEnter({
        ...state,
        compiling: false,
        interactionErrors: status.errors,
        definitionsHighlights: places,
      });
    }
    default:
      throw new Error('handleCompileFailure: unknown editor mode');
  }
}

function handleRunFailure(state: State, status: FailureForEffect<'run'>) {
  console.log('handleFailure', status);
  return handleEnter({
    ...state,
    running: false,
    interactionErrors: [JSON.stringify(status.errors)],
  });
}

function handleEffectFailed(state: State, action: EffectFailure): State {
  switch (action.effect) {
    case 'createRepl':
      return handleCreateReplFailure();
    case 'lint':
      return handleLintFailure(state, action);
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
      chunks.push(emptyChunk({
        text: chunkString,
        startLine: totalLines,
        errorState: notLintedState,
      }));

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

function handleSetChunks(state: State, update: ChunksUpdate): State {
  const { editorMode, isFileSaved } = state;
  if (editorMode !== EditorMode.Chunks) {
    throw new Error('handleSetChunks: not in chunk mode');
  }

  const {
    compiling,
    currentFileContents,
  } = state;

  function nextCompilingState(): boolean | 'out-of-date' {
    if (compiling === true) {
      if (update.modifiesText) {
        return 'out-of-date';
      }

      return true;
    }

    return false;
  }

  if (isMultipleChunkUpdate(update)) {
    let contents = currentFileContents;

    if (update.modifiesText) {
      contents = update.chunks.map((chunk) => chunk.text).join(CHUNKSEP);
    }

    return {
      ...state,
      chunks: update.chunks,
      currentFileContents: contents,
      isFileSaved: isFileSaved && update.modifiesText === false,
      compiling: nextCompilingState(),
    };
  }

  if (isSingleChunkUpdate(update)) {
    const {
      chunks,
    } = state;

    const newChunks = [];
    for (let i = 0; i < chunks.length; i += 1) {
      if (chunks[i].id === update.chunk.id) {
        newChunks.push(update.chunk);
      } else {
        newChunks.push(chunks[i]);
      }
    }

    let contents = currentFileContents;

    if (update.modifiesText) {
      contents = newChunks.map((chunk) => chunk.text).join(CHUNKSEP);
    }

    return {
      ...state,
      chunks: newChunks,
      currentFileContents: contents,
      isFileSaved: isFileSaved && update.modifiesText === false,
      compiling: nextCompilingState(),
    };
  }

  throw new Error('handleSetChunks: unreachable point reached');
}

function handleSetFocusedChunk(state: State, index: number): State {
  const { effectQueue, isFileSaved, focusedChunk } = state;
  const shouldStartEditTimer = !isFileSaved && focusedChunk !== index;
  return {
    ...state,
    focusedChunk: index,
    effectQueue: shouldStartEditTimer ? [...effectQueue, 'startEditTimer'] : effectQueue,
  };
}

function handleSetFontSize(state: State, fontSize: number): State {
  return { ...state, fontSize };
}

function handleSetMenuTabVisible(state: State, tab: false | number) {
  const { menuTabVisible } = state;
  if (menuTabVisible !== false) {
    if (menuTabVisible !== tab) {
      return { ...state, menuTabVisible: tab };
    }

    // typescript tries to lift a literal `false` to the `boolean` type, but `false | number`
    // cannot be `true`.
    const myFalse: false = false;
    return { ...state, menuTabVisible: myFalse };
  }

  return { ...state, menuTabVisible: tab };
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
    case 'fontSize':
      return handleSetFontSize(state, action.value);
    case 'autoRun':
      return { ...state, autoRun: action.value };
    case 'runKind':
      return { ...state, runKind: action.value };
    case 'typeCheck':
      return { ...state, typeCheck: action.value };
    case 'dropdownVisible':
      return { ...state, dropdownVisible: action.value };
    case 'shouldAdvanceCursor':
      return { ...state, shouldAdvanceCursor: action.value };
    case 'menuTabVisible':
      return handleSetMenuTabVisible(state, action.value);
    case 'rhs':
      return { ...state, rhs: action.value };
    case 'firstSelectedChunkIndex':
      return { ...state, firstSelectedChunkIndex: action.value };
    case 'debugBorders':
      return { ...state, debugBorders: action.value };
    default:
      throw new Error(`handleUpdate: unknown action ${JSON.stringify(action)}`);
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
