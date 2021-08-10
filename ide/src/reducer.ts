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
} from './action';

import {
  MessageTabIndex,
  EditorMode,
  State,
  initialState,
  BackendCmd,
} from './state';

import backendCmdFromState from './editor_loop';

import {
  Chunk,
  CHUNKSEP,
  emptyChunk,
  notLintedState,
} from './chunk';

import {
  makeRHSObjects,
  RHSObjects,
} from './rhsObject';

import {
  cleanStopify,
} from './ide-rt-helpers';

import * as ideRt from './ide-rt-override';

import {
  RawRTMessage,
  makeRTMessage,
  RTMessages,
} from './rtMessages';
import { NeverError } from './utils';
import { fs } from './browserfs-setup';
import * as path from './path';
import { bfsSetup, makeServerAPI } from './control';
import { getLocs, Srcloc } from './failure';

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
  const {
    editorResponseLoop,
    effectQueue,
  } = state;

  const cmd = backendCmdFromState(editorResponseLoop);

  if (cmd === BackendCmd.None) {
    console.log('[EDITOR LOOP]: None');
    return {
      ...state,
      effectQueue: [...effectQueue, { effectKey: 'saveFile' }],
    };
  }

  console.log(`[EDITOR LOOP]: ${cmd}`);
  return {
    ...state,
    effectQueue: [...effectQueue, { effectKey: 'initCmd', cmd }],
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
    isFileSaved: true,
  };
}

function handleInitCmdSuccess(state: State): State {
  return state;
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
    case 'initCmd':
      return handleInitCmdSuccess(state);
    default:
      throw new Error(`handleEffectSucceeded: unknown process ${JSON.stringify(action)}`);
  }
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function handleInitCmdFailure(state: State, action: FailureForEffect<'initCmd'>): State {
  // TODO(alex): Do something here?
  return state;
}

function handleSaveFileFailure(state: State, action: FailureForEffect<'saveFile'>): State {
  // TODO(alex): Do something here?
  console.error(`Failed to save file: ${action.error}`);
  return state;
}

function handleEffectFailed(state: State, action: EffectFailure): State {
  switch (action.effectKey) {
    case 'initCmd':
      return handleInitCmdFailure(state, action);
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
      // we already keep currentFileContents in sync with chunk contents while
      // in chunk mode, since we need it to save the file contents.
      return {
        ...state,
        editorMode: newEditorMode,
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

      let totalLines = 0;
      const chunks: Chunk[] = [];

      if (currentFileContents !== '') {
        currentFileContents.split(CHUNKSEP).forEach((chunkString) => {
          chunks.push(emptyChunk({
            // TODO(luna): CHUNKSTEXT this is where the fun happens
            editor: { getValue: () => chunkString },
            startLine: totalLines,
            errorState: notLintedState,
          }));

          totalLines += chunkString.split('\n').length;
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
      contents = update.chunks.map((chunk) => chunk.editor.getValue()).join(CHUNKSEP);
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
    const { chunks } = state;

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
      compiling: nextCompilingState(),
    };
  }

  throw new NeverError(update);
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
    case 'chunkToRHS':
      return { ...state, chunkToRHS: action.value };
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
    case 'backendCmd':
      return { ...state, backendCmd: action.value };
    case 'updater':
      return action.value(state);
    default:
      throw new Error(`handleUpdate: unknown action ${JSON.stringify(action)}`);
  }
}

const serverAPI = makeServerAPI(
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
  } = state;

  const rhs = makeRHSObjects(result, `file://${segmentName(currentFile, id)}`);

  // Associate rhs to chunks *now* before they're outdated. Then only chunk
  // deletion / insertion needs to be tracked to correspond outdated RHSs
  // through edits
  const chunkToRHS: Map<string, RHSObjects> = new Map(state.chunkToRHS);
  chunkToRHS.set(id, rhs);

  // NOTE(alex): necessary b/c Stopify does not clean up top level infrastructure,
  //   resulting in a severe memory leak of 50+MB PER RUN
  cleanStopify();

  const newChunks = chunks.slice();
  const locations = result.result.$locations;
  const traces = result.result.$traces;

  const index = newChunks.findIndex((c) => c.id === id);
  if (locations.length > 0 || traces.length > 0) {
    newChunks[index] = {
      ...newChunks[index],
      errorState: {
        status: 'succeeded',
        effect: 'run',
        result: 'TODO(luna): is this ever read?',
      },
    };
  }

  return {
    ...state,
    backendCmd: BackendCmd.None,
    chunks: newChunks,
    rhs: {
      objects: rhs.objects,
      outdated: rhs.outdated,
    },
    chunkToRHS,
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
    errorState: {
      // TODO(luna): obviously the effect is... run, not compile
      status: 'failed', effect: 'compile', failures: [{ $name: 'text', str: error }], highlights: [],
    },
  };
  return {
    ...state,
    chunks: newChunks,
  };
}

function handleCompileSessionFailure(
  state: State,
  id: string,
  errors: string[],
): State {
  const failures = errors.map((e) => JSON.parse(e));
  const places: Srcloc[] = failures.flatMap(getLocs);

  const { chunks, currentFile } = state;

  function findChunkFromSrclocResult(loc: Srcloc): number | null {
    for (let i = 0; i < chunks.length; i += 1) {
      if (loc.$name === 'srcloc' && loc.source === `file://${segmentName(currentFile, chunks[i].id)}`) {
        return i;
      }
    }
    return null;
  }

  function getExistingHighlights(chunk : Chunk): number[][] | null {
    if (chunk.errorState.status === 'failed') {
      return chunk.errorState.highlights;
    }
    return null;
  }

  const asHL = (place: Srcloc) => {
    if (place.$name !== 'srcloc') {
      throw new Error('how is a builtin a segment?');
    }
    // x:x-x:x (old data structure)
    return [place['start-line'], place['start-column'], place['end-line'], place['end-column']];
  };

  const newChunks = [...chunks];
  if (places.length > 0) {
    places.forEach((place) => {
      const chunkIndex = findChunkFromSrclocResult(place);
      if (chunkIndex) {
        const hl = getExistingHighlights(newChunks[chunkIndex]);
        newChunks[chunkIndex] = {
          ...newChunks[chunkIndex],
          errorState: {
            status: 'failed',
            failures,
            // These might not be used in chatitor atm
            effect: 'compile',
            highlights: hl ? [...hl, asHL(place)] : [asHL(place)],
          },
          needsJiggle: true,
        };
      }
    });
    return {
      ...state,
      chunks: newChunks,
    };
  }
  const chunkIndex = newChunks.findIndex((c) => c.id === id);
  newChunks[chunkIndex] = {
    ...newChunks[chunkIndex],
    errorState: {
      status: 'failed',
      failures,
      // These might not be used in chatitor atm
      effect: 'compile',
      highlights: [asHL({
        $name: 'srcloc',
        source: 'dummy',
        'start-line': 1,
        'start-column': 0,
        'end-line': 999999,
        'end-column': 9999,
        'start-char': 0,
        'end-char': 99999,
        asString: 'dummy',
      })],
    },
    needsJiggle: true,
  };
  return {
    ...state,
    chunks: newChunks,
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
    // backendCmd: BackendCmd.None,
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
    // backendCmd: BackendCmd.None,
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
      objects: rhs.objects,
      outdated: rhs.outdated,
    },
    messageTabIndex: MessageTabIndex.RuntimeMessages,
  };
}

async function runProgramAsync(state : State) : Promise<any> {
  const { typeCheck, currentFile, currentFileContents } = state;
  fs.writeFileSync(currentFile, currentFileContents);
  const sessionId = 'text-session';
  const { dir, base } = bfsSetup.path.parse(currentFile);
  await serverAPI.filterSession(sessionId, 'builtin://');
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
  const { typeCheck, chunks } = state;
  const filenames: string[] = [];
  console.log('RUNNING THESE CHUNKS:');
  chunks.forEach((c) => {
    const filename = segmentName(state.currentFile, c.id);
    filenames.push(filename);
    const value = c.editor.getValue();
    fs.writeFileSync(filename, value);
    console.log(value);
  });
  console.log('Chunks were saved in:', JSON.stringify(filenames));
  fs.writeFileSync(
    state.currentFile,
    chunks.map((chunk) => chunk.editor.getValue()).join(CHUNKSEP),
  );

  const sessionId = 'chatidor-session';
  await serverAPI.filterSession(sessionId, 'builtin://');

  for (let i = 0; i < chunks.length; i += 1) {
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
    update((s: State) => handleRunSessionSuccess(s, c.id, result.result));
  }
  filenames.forEach((f) => {
    fs.unlinkSync(f);
  });
  return 'runSessionAsyncFinished';
}

function runProgramOrSegments(state : State, runner : (s : State) => Promise<any>) : State {
  // TODO(luna): reset rt messages?
  const result : Promise<any> = runner(state);
  result.then(() => {
    store.dispatch(
      { type: 'update', key: 'updater', value: (s) => ({ ...s, running: false }) },
    );
  }).catch((e) => {
    console.log('Running segments failed', e);
  });
  return { ...state, running: true };
}
function stopSession(state: State): State {
  console.assert(state.running);
  stopFlag = true;
  serverAPI.stop();
  return {
    ...state,
    running: false,
  };
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
    default:
      return state;
  }
}

export default function ideApp(state = initialState, action: Action): State {
  return rootReducer(state, action);
}
