/* Handles most of the logic of the IDE.

   The bottom of this file defines a root reducer. This is a function that is
   called in response to any Redux dispatch. The root reducer delegates to other
   reducers based off of what kind of action was passed in the dispatch.

   Important: none of the functions here can perform side effects. This means
   that they can't modify state directly---they have to return a modified copy.
   It also means that side effects such as file saving should not be performed
   here---those should be dealt with in store.ts. */

import { Store } from 'redux';
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
  UIChunksUpdate,
} from './action';

import {
  MessageTabIndex,
  EditorMode,
  State,
  initialState,
  Outdates,
} from './state';

import {
  Chunk,
  CHUNKSEP,
  emptyChunk,
} from './chunk';

import {
  makeRHSObjects,
} from './rhsObject';

import {
  CHATITOR_SESSION,
  cleanStopify,
  NeverError,
  TEXT_SESSION,
} from './utils';

import * as ideRt from './ide-rt-override';

import {
  RawRTMessage,
  makeRTMessage,
  RTMessages,
} from './rtMessages';

import { fs } from './browserfs-setup';
import * as path from './path';
import { bfsSetup, makeServerAPI, CompileAndRunResult } from './control';
import { getLocs, Srcloc } from './failure';
import { RunKind } from './backend';

// Dependency cycle between store and reducer because we dispatch from
// runSession. Our solution is to inject the store into this global variable
// after initializing it
let store: Store<State, Action>;
// Call once, immediately after initializing the store, from store.ts
export function setStore(theStore: Store<State, Action>) {
  store = theStore;
}

/* Tracks the state of side effects. This should only be called as a response to the
   dispatch in store.ts. */
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

  switch (oldEffectQueue[action.effect].effectKey) {
    default:
      return {
        ...state,
        effectQueue,
      };
  }
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
  // NOTE(luna): Yeah you know i'm working around Redux's "reducers shouldn't
  // dispatch" by wrapping it in a setTimeout, what's it to you, Redux is rude
  setTimeout(() => store.dispatch({ type: 'run', key: 'runProgram' }), 0);
  return state;
}

function handleStopSuccess(state: State, action: SuccessForEffect<'stop'>): State {
  console.log('stop successful, paused on line', action.line);
  return {
    ...state,
    running: { type: 'idle' },
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
    isFileSaved: true,
  };
}

function handleEffectSucceeded(state: State, action: EffectSuccess): State {
  switch (action.effectKey) {
    case 'startEditTimer':
      return handleStartEditTimerSuccess(state, action);
    case 'editTimer':
      return handleEditTimerSuccess(state);
    case 'stop':
      return handleStopSuccess(state, action);
    case 'loadFile':
      return handleLoadFileSuccess(state);
    case 'saveFile':
      return handleSaveFileSuccess(state);
    default:
      throw new Error(`handleEffectSucceeded: unknown process ${JSON.stringify(action)}`);
  }
}

function handleSaveFileFailure(state: State, action: FailureForEffect<'saveFile'>): State {
  // TODO(alex): Do something here?
  console.error(`Failed to save file: ${action.error}`);
  return state;
}

function handleEffectFailed(state: State, action: EffectFailure): State {
  switch (action.effectKey) {
    case 'saveFile':
      return handleSaveFileFailure(state, action);
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
  switch (newEditorMode) {
    case EditorMode.Embeditor:
    case EditorMode.Text: {
      // Ensure that currentFileContents is up-to-date with chunks
      const { chunks } = state;
      const currentFileContents = chunks.map((chunk) => chunk.editor.getValue()).join(CHUNKSEP);
      return {
        ...state,
        editorMode: newEditorMode,
        currentFileContents,
      };
    }
    case EditorMode.Chatitor: {
      // in text mode currentFileContents can be more up-to-date than chunks, so we
      // need to recreate the chunks.

      const { currentFileContents } = state;

      if (currentFileContents === undefined) {
        return {
          ...state,
          editorMode: newEditorMode,
          chunks: [],
        };
      }

      const chunks: Chunk[] = [];

      if (currentFileContents !== '') {
        currentFileContents.split(CHUNKSEP).forEach((chunkString) => {
          chunks.push(emptyChunk({
            editor: { getValue: () => chunkString },
          }));
        });
      }

      return {
        ...state,
        editorMode: newEditorMode,
        chunks,
      };
    }
    default:
      throw new NeverError(newEditorMode);
  }
}

function handleSetCurrentFileContents(state: State, contents: string): State {
  const {
    effectQueue,
    compiling,
  } = state;

  return {
    ...state,
    currentFileContents: contents,
    effectQueue: [...effectQueue, { effectKey: 'startEditTimer' }],
    isFileSaved: false,
    compiling: compiling ? 'out-of-date' : false,
  };
}

function handleSetBrowsePath(state: State, browsePath: string): State {
  return {
    ...state,
    browsePath,
  };
}

function handleSetCurrentFile(state: State, file: string): State {
  const { effectQueue } = state;

  return {
    ...state,
    currentFile: file,
    effectQueue: [...effectQueue, { effectKey: 'loadFile' }],
  };
}

function handleSetChunks(state: State, update: ChunksUpdate): State {
  const { editorMode, isFileSaved } = state;
  if (editorMode !== EditorMode.Chatitor) {
    throw new Error('handleSetChunks: not in chunk mode');
  }

  const {
    chunks,
    currentFileContents,
    firstOutdatedChunk,
  } = state;

  if (isMultipleChunkUpdate(update)) {
    let contents = currentFileContents;

    let newOutdate = firstOutdatedChunk;
    if (update.modifiesText) {
      const firstDiffering = update.chunks
        .findIndex((c, i) => chunks[i]?.editor.getValue() !== c.editor.getValue());
      newOutdate = firstDiffering === -1
        ? firstOutdatedChunk
        : Math.min(firstOutdatedChunk, firstDiffering);
    }

    if (update.modifiesText) {
      contents = update.chunks.map((chunk) => chunk.editor.getValue()).join(CHUNKSEP);
    }

    return {
      ...state,
      chunks: update.chunks,
      currentFileContents: contents,
      isFileSaved: isFileSaved && update.modifiesText === false,
      firstOutdatedChunk: newOutdate,
    };
  }

  if (isSingleChunkUpdate(update)) {
    const chunkIdOrNeg1 = chunks.findIndex((c) => c.id === update.chunk.id);
    const chunkId = chunkIdOrNeg1 === -1 ? firstOutdatedChunk : chunkIdOrNeg1;
    const newChunks = chunks.map((chunk) => (
      chunk.id === update.chunk.id ? update.chunk : chunk
    ));

    let contents = currentFileContents;

    if (update.modifiesText) {
      contents = newChunks.map((chunk) => chunk.editor.getValue()).join(CHUNKSEP);
    }

    return {
      ...state,
      chunks: newChunks,
      currentFileContents: contents,
      isFileSaved: isFileSaved && update.modifiesText === false,
      firstOutdatedChunk: Math.min(firstOutdatedChunk, chunkId),
    };
  }

  throw new NeverError(update);
}

function resolveOutdates(firstOutdatedChunk: number, outdates: Outdates): Outdates {
  if (outdates.type === 'initializes') {
    const { index } = outdates;
    return { type: 'initializes', index };
  } if (outdates.type === 'outdates') {
    return { type: 'outdates', index: Math.min(firstOutdatedChunk, outdates.index) };
  }
  throw new NeverError(outdates);
}

// TODO(luna): outdating is done wrong. for example, delete the last chunk and
// firstOutdatedChunk becomes 0, which is wrong(?)
function handleUIChunkUpdate(state: State, update: UIChunksUpdate): State {
  const { chunks, past, firstOutdatedChunk } = state;
  let newChunks: Chunk[];
  let outdates: Outdates;
  let nowOutdated;
  switch (update.key) {
    case 'clear':
      newChunks = [emptyChunk({
        editor: { getValue() { return 'include cpo'; } },
      })];
      outdates = { type: 'initializes', index: firstOutdatedChunk };
      nowOutdated = 0;
      break;
    case 'delete':
      newChunks = [
        ...chunks.slice(0, update.index),
        ...chunks.slice(update.index + 1, chunks.length),
      ];
      outdates = { type: 'outdates', index: update.index };
      // If it's the last chunk, the result goes away and that's it
      nowOutdated = update.index === chunks.length - 1 ? firstOutdatedChunk : update.index;
      break;
    case 'insert':
      newChunks = [
        ...chunks.slice(0, update.index),
        emptyChunk({
          editor: { getValue() { return update.text ?? ''; }, grabFocus: update.grabFocus },
        }),
        ...chunks.slice(update.index, chunks.length),
      ];
      outdates = { type: 'outdates', index: update.index };
      nowOutdated = update.text ? update.index : firstOutdatedChunk;
      break;
    default:
      throw new NeverError(update);
  }
  const undo = {
    chunks,
    outdates,
  };
  return {
    ...state,
    chunks: newChunks,
    past: [...past, undo],
    future: [],
    firstOutdatedChunk: Math.min(firstOutdatedChunk, nowOutdated),
  };
}

function handleUndo(state: State): State {
  const {
    chunks, past, future, firstOutdatedChunk,
  } = state;
  if (past.length === 0) {
    return state;
  }
  const event = past[past.length - 1];
  const outdates = resolveOutdates(firstOutdatedChunk, event.outdates);
  const undo = {
    chunks,
    outdates,
  };
  return {
    ...state,
    chunks: event.chunks,
    firstOutdatedChunk: outdates.index,
    past: past.slice(0, -1),
    future: [...future, undo],
  };
}

function handleRedo(state: State): State {
  const {
    chunks, past, future, firstOutdatedChunk,
  } = state;
  if (future.length === 0) {
    return state;
  }
  const event = future[future.length - 1];
  const outdates = resolveOutdates(firstOutdatedChunk, event.outdates);
  const entry = {
    chunks,
    outdates,
  };
  return {
    ...state,
    chunks: event.chunks,
    firstOutdatedChunk: outdates.index,
    past: [...past, entry],
    future: future.slice(0, -1),
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

function handleRTMessage(state: State, message: RawRTMessage): State {
  const {
    rtMessages,
  } = state;

  const newRTMessages: RTMessages = {
    ...rtMessages,
    messages: [...rtMessages.messages, makeRTMessage(message)],
  };

  return {
    ...state,
    rtMessages: newRTMessages,
  };
}

function handleMessageIndexUpdate(state: State, newIndex: MessageTabIndex) {
  return {
    ...state,
    messageTabIndex: newIndex,
  };
}

// TODO(alex): split editor UI updates to a separate function/file
function handleUpdate(
  state: State,
  action: Update,
): State {
  switch (action.key) {
    case 'editorMode':
      return handleSetEditorMode(state, action.value);
    case 'currentFileContents':
      return handleSetCurrentFileContents(state, action.value);
    case 'browsePath':
      return handleSetBrowsePath(state, action.value);
    case 'currentFile':
      return handleSetCurrentFile(state, action.value);
    case 'chunks':
      return handleSetChunks(state, action.value);
    case 'fontSize':
      return handleSetFontSize(state, action.value);
    case 'runKind':
      return { ...state, runKind: action.value };
    case 'typeCheck':
      return { ...state, typeCheck: action.value };
    case 'dropdownVisible':
      return { ...state, dropdownVisible: action.value };
    case 'menuTabVisible':
      return handleSetMenuTabVisible(state, action.value);
    case 'rt-message':
      return handleRTMessage(state, <RawRTMessage>action.value);
    case 'messageTabIndex':
      return handleMessageIndexUpdate(state, action.value);
    case 'editorResponseLoop':
      return { ...state, editorResponseLoop: action.value };
    case 'editorLoopDropdownVisible':
      return { ...state, editorLoopDropdownVisible: action.value };
    case 'updater':
      return action.value(state);
    default:
      throw new Error(`handleUpdate: unknown action ${JSON.stringify(action)}`);
  }
}

export const serverAPI = makeServerAPI(
  (msg) => console.log('Server: ', msg),
  () => console.log('Setup finished from server API'),
);

function segmentName(file: string, id: string): string {
  const { base } = bfsSetup.path.parse(file);
  return `/projects/${base}-${id}`;
}

function handleRunSessionSuccess(state: State, id: string, result: any): State {
  const {
    chunks,
    currentFile,
    firstOutdatedChunk,
  } = state;

  const rhs = makeRHSObjects(result, `file://${segmentName(currentFile, id)}`);

  // NOTE(alex): necessary b/c Stopify does not clean up top level infrastructure,
  //   resulting in a severe memory leak of 50+MB PER RUN
  cleanStopify();

  const newChunks = chunks.slice();
  const index = newChunks.findIndex((c) => c.id === id);

  newChunks[index] = {
    ...newChunks[index],
    results: {
      status: 'succeeded',
      objects: rhs,
    },
    outdated: false,
  };

  return {
    ...state,
    chunks: newChunks,
    firstOutdatedChunk: Math.max(firstOutdatedChunk, index + 1),
  };
}

function handleRunSessionFailure(state: State, id: string, error: string) {
  // NOTE(alex): necessary b/c Stopify does not clean up top level infrastructure,
  //   resulting in a severe memory leak of 50+MB PER RUN
  cleanStopify();
  const newChunks: Chunk[] = [...state.chunks];
  const index = newChunks.findIndex((c) => c.id === id);
  newChunks[index] = {
    ...newChunks[index],
    results: {
      status: 'failed', failures: [{ $name: 'text', str: error }],
    },
    outdated: false,
  };
  return {
    ...state,
    chunks: newChunks,
    firstOutdatedChunk: Math.max(state.firstOutdatedChunk, index + 1),
  };
}

function handleCompileSessionFailure(
  state: State,
  id: string,
  errors: string[],
): State {
  const failures = errors.map((e) => JSON.parse(e));
  const places: Srcloc[] = failures.flatMap(getLocs);

  const { chunks, currentFile, firstOutdatedChunk } = state;

  function findChunkFromSrclocResult(loc: Srcloc): number | null {
    for (let i = 0; i < chunks.length; i += 1) {
      if (loc.$name === 'srcloc' && loc.source === `file://${segmentName(currentFile, chunks[i].id)}`) {
        return i;
      }
    }
    return null;
  }

  const newChunks = [...chunks];
  if (places.length > 0) {
    let max = firstOutdatedChunk;
    places.forEach((place) => {
      const chunkIndex = findChunkFromSrclocResult(place);
      if (chunkIndex !== null) {
        max = Math.max(chunkIndex + 1, max);
        newChunks[chunkIndex] = {
          ...newChunks[chunkIndex],
          results: {
            status: 'failed',
            failures,
          },
          outdated: false,
        };
      }
    });
    return {
      ...state,
      chunks: newChunks,
      firstOutdatedChunk: max,
    };
  }
  const chunkIndex = newChunks.findIndex((c) => c.id === id);
  newChunks[chunkIndex] = {
    ...newChunks[chunkIndex],
    results: {
      status: 'failed',
      failures,
    },
    outdated: false,
  };
  return {
    ...state,
    chunks: newChunks,
    firstOutdatedChunk: Math.max(firstOutdatedChunk, chunkIndex + 1),
  };
}

const update = (value: (s: State) => State) => {
  store.dispatch({
    type: 'update',
    key: 'updater',
    value,
  });
};

function handleCompileProgramFailure(state: State, errors: string[]) : State {
  const failures = errors.map((e) => JSON.parse(e));
  const places: Srcloc[] = failures.flatMap(getLocs);
  const asHL = (place: Srcloc) => {
    if (place.$name !== 'srcloc') {
      throw new Error('how is a builtin a segment?');
    }
    // x:x-x:x (old data structure)
    return [place['start-line'], place['start-column'], place['end-line'], place['end-column']];
  };
  return {
    ...state,
    // compiling: false,
    interactionErrors: errors,
    definitionsHighlights: places.map(asHL),
    messageTabIndex: MessageTabIndex.ErrorMessages,
  };
}

function handleRunProgramFailure(state: State, error: string) {
  // TODO(joe): get source locations from dynamic errors (source map, etc)
  return {
    ...state,
    // compiling: false,
    interactionErrors: [error],
    definitionsHighlights: [],
    messageTabIndex: MessageTabIndex.ErrorMessages,
  };
}

function handleRunProgramSuccess(state : State, result : any) {
  const rhs = makeRHSObjects(result, `file://${state.currentFile}`);
  return {
    ...state,
    interactionErrors: [],
    definitionsHighlights: [],
    rhs: {
      objects: rhs,
      outdated: false,
    },
    messageTabIndex: MessageTabIndex.RuntimeMessages,
  };
}

async function runTextProgram(
  typeCheck: boolean, runKind: RunKind, saveFile: string, programText: string,
) : Promise<CompileAndRunResult> {
  fs.writeFileSync(saveFile, programText);
  const sessionId = TEXT_SESSION;
  const { dir, base } = bfsSetup.path.parse(saveFile);
  await serverAPI.filterSession(sessionId, 'builtin://');
  const result = await serverAPI.compileAndRun({
    baseDir: dir,
    program: base,
    builtinJSDir: path.compileBuiltinJS,
    checks: 'none',
    typeCheck,
    recompileBuiltins: false,
    session: sessionId,
  }, runKind, {
    spyMessgeHandler: ideRt.defaultSpyMessage,
    spyExprHandler: ideRt.defaultSpyExpr,
    imgUrlProxy: ideRt.defaultImageUrlProxy,
    checkBlockFilter: ideRt.checkBlockFilter,
  });
  return result;
}

async function runProgramAsync(state: State) : Promise<void> {
  const {
    typeCheck, runKind, currentFile, currentFileContents,
  } = state;
  setTimeout(() => (update((s: State) => ({ ...s, running: { type: 'text' } }))), 0);
  const result = await runTextProgram(typeCheck, runKind, currentFile, currentFileContents ?? '');
  if (result.type === 'compile-failure') {
    update((s: State) => handleCompileProgramFailure(s, result.errors));
  } else if (result.type === 'run-failure') {
    update((s: State) => handleRunProgramFailure(s, result.error));
  } else {
    update((s: State) => handleRunProgramSuccess(s, result.result));
  }
}

let stopFlag = false;
async function runSegmentsAsync(state : State) : Promise<any> {
  stopFlag = false;
  const { typeCheck, chunks, firstOutdatedChunk } = state;
  const onlyLastSegmentChanged = firstOutdatedChunk === chunks.length - 1
      // If any non-outdated segment was an error, the final segment would not
      // be non-outdated, with the exception of the last segment itself. We
      // don't want to run this one in such a case because it would have been
      // skipped by the "break on error" logic. Are we ever going to reconsider
      // that logic?
      && chunks[chunks.length - 2] !== undefined
      && chunks[chunks.length - 2].results.status === 'succeeded';
  const filenames: string[] = [];
  console.log('RUNNING THESE CHUNKS:');
  chunks.forEach((c, i) => {
    const isLastSegment = (i === chunks.length - 1);
    if (!onlyLastSegmentChanged || isLastSegment) {
      const filename = segmentName(state.currentFile, c.id);
      filenames.push(filename);
      const value = c.editor.getValue();
      fs.writeFileSync(filename, value);
      console.log(value);
    }
  });
  setTimeout(() => (update((s: State) => ({ ...s, running: { type: 'segments', total: filenames.length, done: 0 } }))), 0);
  console.log('Chunks were saved in:', JSON.stringify(filenames));
  fs.writeFileSync(
    state.currentFile,
    chunks.map((chunk) => chunk.editor.getValue()).join(CHUNKSEP),
  );

  const sessionId = CHATITOR_SESSION;
  if (!onlyLastSegmentChanged) {
    await serverAPI.filterSession(sessionId, 'builtin://');
  }

  for (let i = 0; i < chunks.length; i += 1) {
    const isLastSegment = (i === chunks.length - 1);
    // eslint-disable-next-line
    if (onlyLastSegmentChanged && !isLastSegment) { continue; }
    const c = chunks[i];
    const filename = segmentName(state.currentFile, c.id);
    const { dir, base } = bfsSetup.path.parse(filename);
    // eslint-disable-next-line
    const result = await serverAPI.compileAndRun({
      baseDir: dir,
      program: base,
      builtinJSDir: path.compileBuiltinJS,
      checks: 'none',
      typeCheck,
      recompileBuiltins: false,
      session: sessionId,
    }, state.runKind, {
      spyMessgeHandler: ideRt.defaultSpyMessage,
      spyExprHandler: ideRt.defaultSpyExpr,
      imgUrlProxy: ideRt.defaultImageUrlProxy,
      checkBlockFilter: ideRt.checkBlockFilter,
    });
    console.log('Result from running: ', JSON.stringify(result));
    if (result.type === 'compile-failure') {
      update((s: State) => handleCompileSessionFailure(s, c.id, result.errors));
      break;
    } else if (result.type === 'run-failure') {
      update((s: State) => handleRunSessionFailure(s, c.id, result.error));
      break;
    }
    if (stopFlag) {
      // NOTE(luna): Arguably, there should be no state change. If this chat was
      // outdated, it will still be outdated, if it was
      // "outdated-by-other-chat-edit" then it's still that is the only
      // difference, which there's currently no UI change for (there maybe
      // should be a super subtle one for a couple reasons)
      update((s: State) => handleRunSessionFailure(s, c.id, 'Compile was canceled'));
      stopFlag = false;
      break;
    }
    update((s: State) => ({
      ...handleRunSessionSuccess(s, c.id, result.result),
      running: { ...s.running, done: i + 1 },
    }));
  }
  filenames.forEach((f) => {
    fs.unlinkSync(f);
  });
}

// runner is responsible for setting running!
function runProgramOrSegments(state : State, runner : (s : State) => Promise<any>) : State {
  // TODO(luna): reset rt messages?
  if (state.running.type !== 'idle') { return state; }
  const result : Promise<any> = runner(state);
  result.then(() => {
    store.dispatch(
      { type: 'update', key: 'updater', value: (s) => ({ ...s, running: { type: 'idle' } }) },
    );
  }).catch((e) => {
    store.dispatch(
      { type: 'update', key: 'updater', value: (s) => ({ ...s, running: { type: 'idle' } }) },
    );
    console.log('Running segments failed', e);
  });
  return state;
}
function stopSession(state: State): State {
  console.log('stopSession');
  console.assert(state.running);
  serverAPI.stop().then((wasRunning) => {
    console.log('serverAPI.stop().then(console.log)', wasRunning);
    if (!wasRunning) {
      stopFlag = true;
    }
    update((s: State) => ({ ...s, running: { type: 'idle' } }));
  });
  return state;
}

function rootReducer(state: State, action: Action): State {
  switch (action.type) {
    case 'effectStarted':
      return handleEffectStarted(state, action);
    case 'effectEnded':
      return handleEffectEnded(state, action);
    case 'enqueueEffect':
      return handleEnqueueEffect(state, action);
    case 'run':
      if (action.key === 'runProgram') {
        return runProgramOrSegments(state, runProgramAsync);
      }
      if (action.key === 'runSegments') {
        return runProgramOrSegments(state, runSegmentsAsync);
      }
      throw new NeverError(action);
    case 'stopSession':
      return stopSession(state);
    case 'update':
      return handleUpdate(state, action);
    case 'chunk':
      return handleUIChunkUpdate(state, action);
    case 'undo':
      return handleUndo(state);
    case 'redo':
      return handleRedo(state);
    default:
      return state;
  }
}

export default function ideApp(state = initialState, action: Action): State {
  return rootReducer(state, action);
}
