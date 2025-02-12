/* Handles most of the logic of the IDE.

   The bottom of this file defines a root reducer. This is a function that is
   called in response to any Redux dispatch. The root reducer delegates to other
   reducers based off of what kind of action was passed in the dispatch.

 */

import { Store } from 'redux';
import { fs } from './browserfs-setup';
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
  GoogleDriveProjectStructure,
  GoogleDriveFile,
  getCurrentFileContents,
} from './state';

import {
  Chunk,
  CHUNKSEP,
  emptyChunk,
  isInitializedEditor,
  makeChunksFromString,
  UninitializedEditor,
} from './chunk';

import {
  makeRHSObjects,
  Location,
  ExamplarReport,
  ExamplarResult
} from './rhsObject';

import {
  CHATITOR_SESSION,
  cleanStopify,
  CMEditor,
  NeverError,
  Srcloc,
  TEXT_SESSION,
} from './utils';

import * as ideRt from './ide-rt-override';

import {
  RawRTMessage,
  makeRTMessage,
  RTMessages,
} from './rtMessages';

import * as path from './path';
import { bfsSetup, makeServerAPI, CompileAndRunResult } from './control';
import { Failure, getLocs } from './failure';
import { RunKind } from './backend';
import GoogleAPI from './Drive';
import { resetAsyncCacheToBuiltins } from './runner';
import CodeMirror from 'codemirror';

// Dependency cycle between store and reducer because we dispatch from
// runSession. Our solution is to inject the store into this global variable
// after initializing it
let store: Store<State, Action>;
// Call once, immediately after initializing the store, from store.ts
export function setStore(theStore: Store<State, Action>) {
  store = theStore;
}

function update(value: (s: State) => State) {
  store.dispatch({
    type: 'update',
    key: 'updater',
    value,
  });
}

function segmentName(file: string, id: string): string {
  const { dir, base } = bfsSetup.path.parse(file);
  return `${dir}/${base}-${id}-segment`;
}

function isSegmentName(file : string) {
  return file.endsWith('-segment');
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

function handleStopSuccess(state: State, action: SuccessForEffect<'stop'>): State {
  console.log('stop successful, paused on line', action.line);
  return {
    ...state,
    running: { type: 'idle' },
  };
}

function correctEditorOption(state: State) {
  const { currentFile } = state;
  const { dir } = bfsSetup.path.parse(currentFile);
  // eslint-disable-next-line
  const dirWheats = dir + '/wheats';
  // eslint-disable-next-line
  const dirChaffs = dir + '/chaffs';
  // eslint-disable-next-line
  const testFile = dir + '/test.arr';
  let state2 = state;
  if (fs.existsSync(testFile) && fs.existsSync(dirWheats) && fs.existsSync(dirChaffs)) {
    console.log('setting to Examplaritor');
    return handleSetEditorMode(state, EditorMode.Examplaritor);
  } else if (state.editorMode === EditorMode.Examplaritor) {
    return handleSetEditorMode(state, EditorMode.Chatitor);
  }
  return state2;
}

function handleLoadFileSuccess(state: State): State {
  console.log('loaded a file successfully');
  const state2 = correctEditorOption(state);
  return {
    ...state2,
  };
}

function handleSaveFileSuccess(state: State): State {
  // console.log('saved a file successfully', state, getCurrentFileContents(state));

  return {
    ...state,
    isFileSaved: true,
  };
}

function handleEffectSucceeded(state: State, action: EffectSuccess): State {
  switch (action.effectKey) {
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
  if (state.editorMode === newEditorMode) { return state; }
  const allChunks = makeChunksFromString(getCurrentFileContents(state));
  switch (newEditorMode) {
    case EditorMode.Examplaritor:
    case EditorMode.Text: {
      // Ensure that currentFileContents is up-to-date with chunks
      const defs = allChunks.length > 0 ? allChunks[0].editor.getValue() : "";
      const newDefsEditor = {
        getValue: () => defs
      };
      return {
        ...state,
        definitionsEditor: newDefsEditor,
        chunks: allChunks.slice(1),
        editorMode: newEditorMode,
        past: [],
        future: []
      };
    }
    case EditorMode.Chatitor: {
      return {
        ...state,
        editorMode: newEditorMode,
        chunks: allChunks,
        topChunk: undefined,
        past: [],
        future: []
      };
    }
    default:
      throw new NeverError(newEditorMode);
  }
}

function handleSetBrowsePath(state: State, browsePath: string): State {
  return {
    ...state,
    browsePath,
  };
}

function handleSetCurrentFile(state: State, file: string): State {
  // console.log('tk doing handleSetCurrentFile', file);
  const { effectQueue } = state;

  return {
    ...state,
    currentFile: file,
    effectQueue: [...effectQueue, { effectKey: 'loadFile' }],
  };
}

function handleSetChunks(state: State, chunksUpdate: ChunksUpdate): State {
  const { editorMode, isFileSaved } = state;
  const {
    chunks,
    firstOutdatedChunk,
  } = state;
  
  let chunksString;
  let newOutdate;
  let newChunks : Chunk[] = chunks;
  
  if (isMultipleChunkUpdate(chunksUpdate)) {
    newOutdate = firstOutdatedChunk;
    if (chunksUpdate.modifiesText) {
      const firstDiffering = chunksUpdate.chunks
        .findIndex((c, i) => chunks[i]?.editor.getValue() !== c.editor.getValue());
      newOutdate = firstDiffering === -1
        ? firstOutdatedChunk
        : Math.min(firstOutdatedChunk, firstDiffering);
    }

    if (chunksUpdate.modifiesText) {
      chunksString = chunksUpdate.chunks.map((chunk) => chunk.editor.getValue()).join(CHUNKSEP);
    }
    newChunks = chunksUpdate.chunks;
  }
  else if (isSingleChunkUpdate(chunksUpdate)) {
    const chunkIdOrNeg1 = chunks.findIndex((c) => c.id === chunksUpdate.chunk.id);
    const chunkId = chunkIdOrNeg1 === -1 ? firstOutdatedChunk : chunkIdOrNeg1;
    newChunks = chunks.map((chunk) => (
      chunk.id === chunksUpdate.chunk.id ? chunksUpdate.chunk : chunk
    ));

    if (chunksUpdate.modifiesText) {
      chunksString = newChunks.map((chunk) => chunk.editor.getValue()).join(CHUNKSEP);
    }
    newOutdate = Math.min(firstOutdatedChunk, chunkId);
  }
  else {
    throw new NeverError(chunksUpdate);
  }

  switch(editorMode) {
    case EditorMode.Examplaritor: 
    case EditorMode.Text: {
      return {
        ...state,
        chunks: newChunks,
        isFileSaved: isFileSaved && chunksUpdate.modifiesText === false,
        firstOutdatedChunk: newOutdate,
        effectQueue: [ ...state.effectQueue, { effectKey: 'saveFile' }]
      };
    }
    case EditorMode.Chatitor: {
      return {
        ...state,
        chunks: newChunks,
        isFileSaved: isFileSaved && chunksUpdate.modifiesText === false,
        firstOutdatedChunk: newOutdate,
        effectQueue: [ ...state.effectQueue, { effectKey: 'saveFile' }]
      };
    }
    default: {
      throw new NeverError(editorMode);  
    }
  }

  

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
function handleUIChunkUpdate(state: State, chunksUpdate: UIChunksUpdate): State {
  const { chunks, past, firstOutdatedChunk, editorMode, definitionsEditor } = state;
  let newChunks: Chunk[];
  let outdates: Outdates;
  let nowOutdated;
  let newDefs: UninitializedEditor | CMEditor = definitionsEditor;
  const oldDefs = definitionsEditor.getValue();
  switch (chunksUpdate.key) {
    case 'clear':
      newChunks = [emptyChunk({
        editor: { getValue() { return 'include cpo'; } },
      })];
      outdates = { type: 'initializes', index: firstOutdatedChunk };
      nowOutdated = 0;
      break;
    case 'delete':
      newChunks = [
        ...chunks.slice(0, chunksUpdate.index),
        ...chunks.slice(chunksUpdate.index + 1, chunks.length),
      ];
      outdates = { type: 'outdates', index: chunksUpdate.index };
      // If it's the last chunk, the result goes away and that's it
      nowOutdated = chunksUpdate.index === chunks.length - 1 ? firstOutdatedChunk : chunksUpdate.index;
      break;
    case 'merge':
      const above = chunks[chunksUpdate.top];
      const below = chunks[chunksUpdate.bottom];
      const newChunkText = `${above.editor.getValue()}\n${below.editor.getValue()}`;
      const newChunk = emptyChunk({ 
        editor: {
          getValue() { return newChunkText; },
          grabFocus: true
        }
      });
      newChunks = [
        ...chunks.slice(0, chunksUpdate.top),
        newChunk,
        ...chunks.slice(chunksUpdate.bottom + 1, chunks.length)
      ];
      outdates = { type: 'outdates', index: chunksUpdate.top };
      nowOutdated = chunksUpdate.top;
      break;
    case 'insert':
      newChunks = [
        ...chunks.slice(0, chunksUpdate.index),
        emptyChunk({
          editor: { getValue() { return chunksUpdate.text ?? ''; }, grabFocus: chunksUpdate.grabFocus },
        }),
        ...chunks.slice(chunksUpdate.index, chunks.length),
      ];
      outdates = { type: 'outdates', index: chunksUpdate.index };
      nowOutdated = chunksUpdate.text ? chunksUpdate.index : firstOutdatedChunk;
      break;
    case 'appendToDefinitions':
      if(editorMode === EditorMode.Chatitor) {
        return state;
      }
      newChunks = [
        ...chunks.slice(0, chunksUpdate.index),
        ...chunks.slice(chunksUpdate.index + 1, chunks.length),
      ];
      outdates = { type: 'outdates', index: chunksUpdate.index };
      nowOutdated = 0;
      const currentDefs = definitionsEditor.getValue();
      newDefs = { getValue() { return currentDefs + "\n\n" + chunks[chunksUpdate.index].editor.getValue() } };
      break;
    default:
      throw new NeverError(chunksUpdate);
  }
  const undo = {
    chunks,
    outdates,
    definitions: oldDefs,
  };
  const rerunAllChunks = ['clear', 'delete', 'merge', 'appendToDefinitions'].indexOf(chunksUpdate.key) >= 0;
  return {
    ...state,
    chunks: newChunks,
    definitionsEditor: newDefs,
    past: [...past, undo],
    future: [],
    rerunAllChunks,
    firstOutdatedChunk: Math.min(firstOutdatedChunk, nowOutdated),
    effectQueue: [ ...state.effectQueue, { effectKey: 'saveFile' }]
  };
}

function handleUndo(state: State): State {
  const {
    definitionsEditor, chunks, past, future, firstOutdatedChunk,
  } = state;
  if (past.length === 0) {
    return state;
  }
  const event = past[past.length - 1];
  const outdates = resolveOutdates(firstOutdatedChunk, event.outdates);
  const undo = {
    definitions: definitionsEditor.getValue(),
    chunks,
    outdates,
  };
  return {
    ...state,
    chunks: event.chunks,
    firstOutdatedChunk: outdates.index,
    definitionsEditor: { getValue() { return event.definitions; } },
    past: past.slice(0, -1),
    future: [...future, undo],
    effectQueue: [ ...state.effectQueue, { effectKey: 'saveFile' }]
  };
}

function handleRedo(state: State): State {
  const {
    definitionsEditor, chunks, past, future, firstOutdatedChunk,
  } = state;
  if (future.length === 0) {
    return state;
  }
  const event = future[future.length - 1];
  const outdates = resolveOutdates(firstOutdatedChunk, event.outdates);
  const entry = {
    definitions: definitionsEditor.getValue(),
    chunks,
    outdates,
  };
  return {
    ...state,
    chunks: event.chunks,
    firstOutdatedChunk: outdates.index,
    definitionsEditor: { getValue() { return event.definitions } },
    past: [...past, entry],
    future: future.slice(0, -1),
    effectQueue: [ ...state.effectQueue, { effectKey: 'saveFile' }]
  };
}

function handleReady(state: State) : State {
  const { readyCallbacks } = state;
  readyCallbacks.forEach((cb) => cb());
  return { ...state, readyCallbacks: [], ready: true };
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

export function populateFromDrive(structure : GoogleDriveProjectStructure) {
  function copyFile(base : string, file : GoogleDriveFile) {
    const filePath = `${base}/${file.name}`;
    if (!fs.existsSync(filePath)) {
      fs.writeFileSync(filePath, file.body);
    }
  }
  function recursiveCopy(base : string, dir: GoogleDriveProjectStructure) {
    const dirPath = `${base}/${dir.name}/`;
    if (!fs.existsSync(dirPath)) {
      fs.mkdirSync(dirPath);
    }
    dir.files.forEach((f) => copyFile(dirPath, f));
    dir.folders.forEach((f) => recursiveCopy(dirPath, f));
  }
  const projectPath = `/google-drive/${structure.id}/`;
  if (!fs.existsSync(projectPath)) {
    fs.mkdirSync(projectPath, { recursive: true });
  }
  recursiveCopy(projectPath, structure);
}

function handleFileSync(state: State) : State {
  const google = new GoogleAPI();
  if (state.projectState.type !== 'gdrive') { return state; }

  async function checkAndSaveFile(fsPath : string, filename : string, googDir : GoogleDriveProjectStructure) : Promise<GoogleDriveFile> {
    const existingGoogleFile = googDir.files.filter((d) => d.name === filename);
    console.log('Looking for google files: ', existingGoogleFile, fsPath, filename, googDir);
    if (existingGoogleFile.length > 0) {
      const filePath = `${fsPath}/${filename}`;
      const stats = fs.statSync(filePath);
      const [file] = existingGoogleFile;
      if (new Date(stats.mtime) > new Date(file.modifiedTime)) {
        const contents = String(fs.readFileSync(`${fsPath}/${filename}`));
        // eslint-disable-next-line
        await google.saveFile(file, contents);
        return { ...file, modifiedTime: stats.mtime, body: contents };
      }
      console.log('No change to ', fsPath, filename);
      return file;
    } else {
      const contents = String(fs.readFileSync(`${fsPath}/${filename}`));
      return google.createFile(filename, googDir.id, contents);
    }
  }

  async function recursiveCheckAndSave(fsPath : string, googDir : GoogleDriveProjectStructure) : Promise<GoogleDriveProjectStructure> {
    console.log('Visiting ', fsPath, googDir);
    const inDirectory : string[] = fs.readdirSync(fsPath);
    const updatedDirs = [];
    const updatedFiles = [];
    for (let i = 0; i < inDirectory.length; i += 1) {
      const dirOrFile = inDirectory[i];
      // eslint-disable-next-line
      if (isSegmentName(dirOrFile)) { continue; }
      const fullPath = `${fsPath}/${dirOrFile}`;
      const stats = fs.statSync(fullPath);
      if (stats.isDirectory()) {
        const existingGoogleDir = googDir.folders.filter((d) => d.name === dirOrFile);
        console.log('Syncing a directory', existingGoogleDir);
        let dirToVisit;
        if (existingGoogleDir.length > 0) {
          [dirToVisit] = existingGoogleDir;
        } else {
          // eslint-disable-next-line
          const newDir = await google.createDir(dirOrFile, googDir.id);
          dirToVisit = { ...newDir, files: [], folders: [] };
        }
        // eslint-disable-next-line
        const updatedDir = await recursiveCheckAndSave(fullPath, dirToVisit);
        updatedDirs.push(updatedDir);
      } else {
        console.log('Syncing a file', fsPath, dirOrFile);
        // eslint-disable-next-line
        const updatedFile = await checkAndSaveFile(fsPath, dirOrFile, googDir);
        updatedFiles.push(updatedFile);
      }
    }

    return { ...googDir, files: updatedFiles, folders: updatedDirs };
  }

  const { structure } = state.projectState;
  const projectPath = `/google-drive/${structure.id}/${structure.name}`;
  const newStructure = recursiveCheckAndSave(projectPath, structure);
  setTimeout(() => {
    update((s : State) => ({ ...s, headerMessage: 'Saving...' }));
    newStructure.then((updatedStructure) => {
      update((s : State) => ({ ...s, projectState: { type: 'gdrive', structure: updatedStructure } }));
      update((s : State) => ({ ...s, headerMessage: 'Saved.' }));
      // Then, if this was the first-created file, make sure it pops up
      if (structure.files.length === 0 && updatedStructure.files.length > 0) {
        const filePath = `${projectPath}/${updatedStructure.files[0].name}`;
        update((s : State) => ({ ...s, currentFile: filePath }));
        store.dispatch({ type: 'enqueueEffect', effect: { effectKey: 'loadFile' } });
      }
    })
      .catch((err) => {
        update((s : State) => ({ ...s, headerMessage: 'Could not save.' }));
        console.error("Couldn't sync: ", err);
      });
  }, 0);
  return state;
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

function handleAddReadyCallback(state : State, cb: () => void) {
  if (state.ready) {
    setTimeout(cb, 0);
    return state;
  } else {
    return {
      ...state,
      readyCallbacks: [cb].concat(state.readyCallbacks),
    };
  }
}

// TODO(alex): split editor UI updates to a separate function/file
function handleUpdate(
  state: State,
  action: Update,
): State {
  switch (action.key) {
    case 'editorMode':
      return handleSetEditorMode(state, action.value);
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
    case 'addReadyCallback':
      return handleAddReadyCallback(state, action.value);
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

// Yeah... this is like O(n_chunks*n_references) and runs on every run result,
// *and* it reconstructs every chunk, but it's very important to not mutate
// because of undo/redo and we do have to invalidate each one. And JS Set
// doesn't have immutable remove! If it becomes a problem we can rethink
function removeReferencesFrom(chunks: Chunk[], id: string): Chunk[] {
  return chunks.map((c) => ({
    ...c,
    referencedFrom: c.referencedFrom.filter((from) => from !== id),
  }));
}

function handleRunSessionStart(state: State, id: string, doc: CodeMirror.Doc): State {
  const start = Date.now();
  const newChunks = [...state.chunks];
  const index = state.chunks.findIndex((c) => c.id === id);
  newChunks[index] = {
    ...newChunks[index],
    results: {
      ...newChunks[index].results,
      start,
      end: undefined,
      editorAtLastRun: doc
    },
  };
  return {
    ...state,
    chunks: newChunks
  }
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

  const newChunks = removeReferencesFrom(chunks, id);
  const index = newChunks.findIndex((c) => c.id === id);
  const end = Date.now();
  newChunks[index] = {
    ...newChunks[index],
    results: {
      ...newChunks[index].results,
      end,
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

function handleRunSessionFailure(state: State, id: string, error: string, errorVal: any) : State {
  // NOTE(alex): necessary b/c Stopify does not clean up top level infrastructure,
  //   resulting in a severe memory leak of 50+MB PER RUN
  cleanStopify();
  const newChunks = removeReferencesFrom(state.chunks, id);
  const index = newChunks.findIndex((c) => c.id === id);
  const failureAsED : Failure = errorVal.errorDisplay ?? { $name: 'text', str: error };
  const end = Date.now();
  newChunks[index] = {
    ...newChunks[index],
    results: {
      ...newChunks[index].results,
      end,
      status: 'failed', failures: [failureAsED],
    },
    outdated: false,
  };
  return {
    ...state,
    chunks: newChunks,
    firstOutdatedChunk: Math.max(state.firstOutdatedChunk, index + 1),
  };
}

function handleRunExamplarFailure(state: State, error: string) : State {
  // console.log('** handleRunExamplarFailure', error);
  const oldTop = state.topChunk!.results;
  const end = Date.now();

  const failureObj : any = { $name: 'text', str: error };
  return {
    ...state,
    messageTabIndex: MessageTabIndex.ErrorMessages,
    topChunk: {
      editor: state.definitionsEditor,
      results: {
        ...oldTop,
        end,
        status: 'failed',
        failures: [failureObj],
      },
      id: "topChunk",
      outdated: false,
      referencedFrom: []
    }
  };
}

function handleCompileSessionFailure(
  state: State,
  id: string,
  errors: string[],
): State {
  const failures = errors.map((e) => JSON.parse(e));
  const places = failures.flatMap(getLocs);

  const { chunks, currentFile, firstOutdatedChunk } = state;

  function findChunkFromSrclocResult(loc: Srcloc): number | null {
    for (let i = 0; i < chunks.length; i += 1) {
      if (loc.$name === 'srcloc' && loc.source === `file://${segmentName(currentFile, chunks[i].id)}`) {
        return i;
      }
    }
    return null;
  }

  // Because it's not a set, we don't want to add ourselves to it twice
  const newChunks = removeReferencesFrom(chunks, id);
  const chunkIndex = newChunks.findIndex((c) => c.id === id);
  places.forEach((place) => {
    const referencing = findChunkFromSrclocResult(place);
    if (referencing !== null && referencing !== chunkIndex) {
      const refs = newChunks[referencing].referencedFrom;
      // This is okay despite other refs because these would be ones *just added
      // by us*
      if (refs[refs.length - 1] !== id) {
        newChunks[referencing] = {
          ...newChunks[referencing],
          referencedFrom: [
            ...newChunks[referencing].referencedFrom,
            id,
          ],
        };
      }
    }
  });
  const end = Date.now();
  newChunks[chunkIndex] = {
    ...newChunks[chunkIndex],
    results: {
      ...newChunks[chunkIndex].results,
      end,
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

function handleCompileProgramFailure(state: State, errors: string[]) : State {
  const oldTop = state.topChunk!.results;
  const end = Date.now();
  const failures = errors.map((e) => JSON.parse(e));
  const places = failures.flatMap(getLocs);
  return {
    ...state,
    // compiling: false,
    messageTabIndex: MessageTabIndex.ErrorMessages,
    topChunk: {
      id: "topChunk",
      editor: state.definitionsEditor,
      results: {
        ...oldTop,
        end,
        status: 'failed',
        failures,
      },
      outdated: false,
      referencedFrom: []
    }
  };
}

function handleCompileExamplarFailure(state: State, errors: string[]) : State {
  const oldTop = state.topChunk!.results;
  const end = Date.now();
  return {
    ...state,
    messageTabIndex: MessageTabIndex.ErrorMessages,
    topChunk: {
      id: "topChunk",
      editor: state.definitionsEditor,
      results: {
        ...oldTop,
        end,
        status: 'failed',
        failures: errors.map(e => JSON.parse(e)),
      },
      outdated: false,
      referencedFrom: []
    }
  };
}

function handleRunProgramStart(state: State, doc: CodeMirror.Doc) : State {
  const start = Date.now();
  const oldTop = state.topChunk ?? {
    editor: state.definitionsEditor,
    results: {
      status: 'running',
      start,
      end: undefined,
      editorAtLastRun: doc,
    },
    id: "topChunk",
    outdated: false,
    referencedFrom: [] 
  };
  return {
    ...state,
    topChunk: {
      ...oldTop,
      results: {
        ...oldTop.results,
        start,
        end: undefined,
        editorAtLastRun: doc,
      }
    },
  };
}

function handleRunProgramFailure(state: State, error: string) : State {
  // TODO(joe): get source locations from dynamic errors (source map, etc)
  const oldTop = state.topChunk!.results;
  
  const end = Date.now();
  return {
    ...state,
    // compiling: false,
    messageTabIndex: MessageTabIndex.ErrorMessages,
    topChunk: {
      editor: state.definitionsEditor,
      results: {
        ...oldTop,
        end,
        status: 'failed',
        failures: [JSON.parse(error)],
      },
      id: "topChunk",
      outdated: false,
      referencedFrom: []
    }
  };
}

function handleRunProgramSuccess(state : State, result : any) : State {
  const rhs = makeRHSObjects(result, `file://${state.currentFile}`);
  const end = Date.now();
  const oldTop = state.topChunk!.results;
  return {
    ...state,
    topChunk: {
      editor: state.definitionsEditor,
      results: {
        ...oldTop,
        end,
        status: 'succeeded',
        objects: rhs
      },
      outdated: false,
      id: "topChunk",
      referencedFrom: []
    },
    messageTabIndex: MessageTabIndex.RuntimeMessages,
  };
}

function numFailures(resultArray: ExamplarResult[]) {
  const fails = resultArray.filter((result) => !(result.success));
  return fails.length;
}

function handleRunExamplarSuccess(state: State, wheatResultArray: any[], chaffResultArray: any[], hintMessage: string, qtmVariations: number, sampleResult: any, reprFile: string) : State {
  if (!sampleResult) {
    console.log('examplar all wheats and chaffs were ill-formed');
  }
  const rhs = makeRHSObjects(sampleResult, `file://${reprFile}`);
  const rhs0 = (rhs.slice(0, 1))[0];
  // eslint-disable-next-line
  const modifiedResult : ExamplarReport = {
    key: (<Location>rhs0).key,
    tag: 'examplar',
    srcloc: (<Location>rhs0).srcloc,
    wheatResults: wheatResultArray,
    chaffResults: chaffResultArray,
    hintMessage: hintMessage,
    qtmVariations: qtmVariations
  };
  const doc = isInitializedEditor(state.definitionsEditor)
    ? state.definitionsEditor.getDoc().copy(false)
    : CodeMirror.Doc(state.definitionsEditor.getValue());
  return {
    ...state,
    running: { type: 'idle' },
    topChunk: {
      id: "topChunk",
      editor: state.definitionsEditor,
      results: { status: 'succeeded', objects: [modifiedResult], editorAtLastRun: doc },
      outdated: false,
      referencedFrom: []
    },
    messageTabIndex: MessageTabIndex.RuntimeMessages,
  };
}

async function runTextProgram(
  typeCheck: boolean, runKind: RunKind, saveFile: string, programText: string,
) : Promise<CompileAndRunResult> {
  // console.log('tk doing runTextProgram', saveFile);
  if (fs.existsSync(saveFile)) {
    fs.unlinkSync(saveFile);
  }
  fs.writeFileSync(saveFile, programText);
  const sessionId = TEXT_SESSION;
  const { dir, base } = bfsSetup.path.parse(saveFile);

  await (await serverAPI).filterSession(sessionId, 'builtin://');
  const result = await (await serverAPI).compileAndRun({
    baseDir: dir,
    program: base,
    builtinJSDir: path.compileBuiltinJS,
    checks: 'all',
    typeCheck,
    recompileBuiltins: false,
    session: sessionId,
  }, runKind, {
    cwd: dir,
    spyMessgeHandler: ideRt.defaultSpyMessage,
    spyExprHandler: ideRt.defaultSpyExpr,
    imgUrlProxy: ideRt.defaultImageUrlProxy,
    checkBlockFilter: ideRt.checkBlockFilter,
  });
  return result;
}

async function upwait(f : (s : State) => State) {
  return new Promise<State>((resolve) => {
    setTimeout(() => {
      update((s : State) => {
        const newState = f(s);
        resolve(newState);
        return newState;
      });
    }, 0);
  });
}

async function runProgramAsync(state: State) : Promise<void> {
  const {
    typeCheck, runKind, currentFile, definitionsEditor, topChunk, rerunAllChunks
  } = state;
  if( !topChunk || topChunk.outdated || topChunk.results.status === 'failed' || rerunAllChunks ) {
    const currentFileContents = definitionsEditor.getValue();
    const doc = isInitializedEditor(definitionsEditor) ? definitionsEditor.getDoc().copy(false) : CodeMirror.Doc(definitionsEditor.getValue());
    cleanStopify();
    await upwait((s: State) => handleRunProgramStart(s, doc));
    const result = await runTextProgram(typeCheck, runKind, segmentName(currentFile, "definitions"), currentFileContents ?? '');
    // console.log('tk result=', result);
    if (result.type === 'compile-failure') {
      update((s: State) => handleCompileProgramFailure(s, result.errors));
    } else if (result.type === 'run-failure') {
      update((s: State) => handleRunProgramFailure(s, result.error));
    } else {
      update((s: State) => handleRunProgramSuccess(s, result.result));
      await runSegmentsAsyncWithSession(state, TEXT_SESSION, true);
    }  
  }
  else {
    await runSegmentsAsyncWithSession(state, TEXT_SESSION, true); 
  }
}

function setupRunProgramAsync(state: State) : State {
  return { ...state, running: { type: 'text' } };
}

let stopFlag = false;
async function runSegmentsAsyncWithSession(state : State, sessionId : string, alwaysKeepCache : boolean) : Promise<any> {
  stopFlag = false;
  const { typeCheck, chunks, firstOutdatedChunk, rerunAllChunks } = state;
  const onlyLastSegmentChanged = firstOutdatedChunk === chunks.length - 1
      
  // If any non-outdated segment was an error, the final segment would not
      // be non-outdated, with the exception of the last segment itself. We
      // don't want to run this one in such a case because it would have been
      // skipped by the "break on error" logic. Are we ever going to reconsider
      // that logic?
      && chunks[chunks.length - 2] !== undefined
      && chunks[chunks.length - 2].results.status === 'succeeded'

      // If we did some merging or deleting that might have made bindings go away,
      // but still left the last chunk as the only one outdated, we still need
      // to re-run all, because we don't have per-chunk control over the environment,
      // etc. So rerunAllChunks makes sure we don't e.g. get a shadowing error from
      // the bindings of a chunk that was deleted.
      && !rerunAllChunks;
  const filenames: string[] = [];
  // If we are re-running the whole program, make sure to also re-run anything
  // that's imported, etc. This reset function removes *everything* but the
  // builtins from the cache, which is also a good eager GC thing to do.
  if (!onlyLastSegmentChanged && !alwaysKeepCache) {
    resetAsyncCacheToBuiltins();
  }
  console.log('RUNNING THESE CHUNKS:');
  // gather all segment filenames
  const { dir } = bfsSetup.path.parse(state.currentFile);
  const outdatedSegments = new Set<string>();
  fs.readdirSync(dir).forEach((p: string) => {
    if(p.endsWith("-segment")) {
      const maybeSegmentPath = bfsSetup.path.join(dir, p);
      outdatedSegments.add(maybeSegmentPath);
    }
  });
  console.log("outdatedSegments before looking at chiunks:", outdatedSegments);
  const docs = chunks.map((c, i) => {
    const filename = segmentName(state.currentFile, c.id);
    outdatedSegments.delete(filename);
    const isLastSegment = (i === chunks.length - 1);
    if (!onlyLastSegmentChanged || isLastSegment) {
      filenames.push(filename);
      const value = c.editor.getValue();
      fs.writeFileSync(filename, value);
    }
    return isInitializedEditor(c.editor) ? c.editor.getDoc().copy(false) : CodeMirror.Doc(c.editor.getValue());
  });
  console.log("after:" , outdatedSegments);
  outdatedSegments.forEach(p => {
    fs.unlinkSync(p);
  });

  if (!onlyLastSegmentChanged && !alwaysKeepCache) {
    await (await serverAPI).filterSession(sessionId, 'builtin://');
  }

  for (let i = 0; i < chunks.length; i += 1) {
    const isLastSegment = (i === chunks.length - 1);
    // eslint-disable-next-line
    if (onlyLastSegmentChanged && !isLastSegment) { continue; }
    const c = chunks[i];
    const filename = segmentName(state.currentFile, c.id);
    const { dir, base } = bfsSetup.path.parse(filename);
    await upwait((s: State) => handleRunSessionStart(s, c.id, docs[i]));
    // eslint-disable-next-line
    const result = await (await serverAPI).compileAndRun({
      baseDir: dir,
      program: base,
      builtinJSDir: path.compileBuiltinJS,
      checks: 'all',
      typeCheck,
      recompileBuiltins: false,
      session: sessionId,
    }, state.runKind, {
      cwd: dir,
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
      update((s: State) => handleRunSessionFailure(s, c.id, result.error, result.errorVal));
      break;
    }
    if (stopFlag) {
      // NOTE(luna): Arguably, there should be no state change. If this chat was
      // outdated, it will still be outdated, if it was
      // "outdated-by-other-chat-edit" then it's still that is the only
      // difference, which there's currently no UI change for (there maybe
      // should be a super subtle one for a couple reasons)
      update((s: State) => handleRunSessionFailure(s, c.id, 'Compile was canceled', {}));
      stopFlag = false;
      break;
    }
    update((s: State) => ({
      ...handleRunSessionSuccess(s, c.id, result.result),
      running: { ...s.running, done: i + 1 },
    }));
  }
  if(!state.developerMode) {
    filenames.forEach((f) => {
      fs.unlinkSync(f);
    });
  }

}

async function runSegmentsAsync(state : State) : Promise<any> {
  const session = state.editorMode === EditorMode.Text ? TEXT_SESSION : CHATITOR_SESSION;
  return runSegmentsAsyncWithSession(state, session, state.editorMode === EditorMode.Text);
}

function setupRunSegmentsAsync(s : State) : State {
  const { chunks, firstOutdatedChunk } = s;
  const onlyLastSegmentChanged = firstOutdatedChunk === chunks.length - 1
      // If any non-outdated segment was an error, the final segment would not
      // be non-outdated, with the exception of the last segment itself. We
      // don't want to run this one in such a case because it would have been
      // skipped by the "break on error" logic. Are we ever going to reconsider
      // that logic?
      && chunks[chunks.length - 2] !== undefined
      && chunks[chunks.length - 2].results.status === 'succeeded';
  const count = onlyLastSegmentChanged ? 1 : chunks.length;
  return { ...s, running: { type: 'segments', total: count, done: 0 } };
}

// runner is responsible for setting running!
function runProgramOrSegments(
  state : State,
  runner : (s : State) => Promise<any>,
  updater : (s : State) => State,
) : State {
    // console.log('tk doing runProgramOrSegments', state);
  // TODO(luna): reset rt messages?
  if (state.running.type !== 'idle') { return state; }
  const result : Promise<any> = runner(state);
  result.finally(() => {
    update((s) => ({ ...s, running: { type: 'idle' } }));
  });
  return updater(state);
}

function handleRunExamplarSuccessFull(state: State, wheatResultArray: any[], chaffResultArray: any[], hintMessage: string, qtmVariations: number, sampleResult: any, reprFile: string) : State {
  // console.log('tk doing handleRunExamplarSuccessFull', state, wheatResultArray, chaffResultArray);
  const state2 = handleRunExamplarSuccess(state, wheatResultArray, chaffResultArray, hintMessage, qtmVariations, sampleResult, reprFile);
  // const state3 = runProgramOrSegments(state2, runSegmentsAsync, setupRunSegmentsAsync);
  return state2;
}

function insertUserImpl(s: string, newFile: string) {
  //console.log('### insertUserImpl', s, newFile);
  const ll = s.split('\n');
  const llOut = ll.map((x) => x);
  for (let i = 0; i < ll.length; i += 1) {
    const line = ll[i];
    if (/^ *(include|import) +file\(".*?-code.arr"\)/.test(line)) {
      // eslint-disable-next-line
      llOut[i] = line.replace(/file\(".*?-code.arr"\)/, 'file("' + newFile + '")');
      break;
    }
  }
  return llOut.join('\n');
}

function extractRelevantCode(str: string, loc: any[]) {
  // console.log('tk doing extractRelevantCode');
  const l1 : number = loc[1] - 1;
  const c1 : number = loc[2];
  const l2 : number = loc[4] - 1;
  const c2 : number = loc[5];
  const ss = str.split('\n');
  // console.log('tk l1', l1, 'c1', c1, 'l2', l2, 'c2', c2);
  // console.log('tk ss-len', ss.length);
  let x : string;
  if (l1 === l2) {
    x = ss[l1].substring(c1, c2 + 1);
  } else {
    x = ss[l1].substring(c1);
    for (let j = l1 + 1; j < l2; j+= 1) {
      x +=  ss[j];
    }
    x += ss[l2].substring(0, c2 + 1);
  }
  return x;
}

function addCodeToCheckArray(checkArray: any[], fileString: string, numOfChecks: number): any[] {
  // console.log('tk doing addCodeToCheckArray', checkArray);
  // console.log('tk checkarraylen =', checkArray.length, 'pick=', numOfChecks);
  let testResultArray = [];
  let n = checkArray.length;
  if (numOfChecks > -1) {
    checkArray = checkArray.slice(n - numOfChecks, n);
    n = numOfChecks;
  }
  // console.log('tk n=', n);
  for (let i = 0; i < n; i += 1) {
    const check = checkArray[i];
    const testResults = check.testResults;
    const m = testResults.length;
    // console.log('tk m=', m);
    for (let j = 0; j < m; j += 1) {
      const testResult = testResults[j];
      // console.log('tk inner loop', j, testResult);
      // console.log('tk loc=', testResult.metadata.loc);
      const relevantCode = extractRelevantCode(fileString, testResult.metadata.loc);
      // console.log('tk relevantCode=', relevantCode);
      const qtm = ((/-in-ok/.test(relevantCode) || /-out-ok/.test(relevantCode)) && (/satisfies/.test(relevantCode) || /violates/.test(relevantCode)));
      testResultArray.push({
        success: (testResult.$name === 'success'),
        exception: false,
        code: relevantCode,
        qtm: qtm
      });
    }
  }
  console.log('tk testResultArray =', testResultArray);
  return testResultArray;
}

async function runExamplarAsync(state: State) : Promise<any> {
  // currentFile is just the standard program.arr, we'll use it to get at
  // our relevant files
  const { typeCheck, runKind, currentFile } = state;
  // console.log('tk doing runExamplarAsync on', currentFile);
  const { dir } = bfsSetup.path.parse(currentFile);
  let hintMessage: string = '';
  let qtmVariationsArr: any[] = [];

  function getQtmVariations(checkArrayWithCode: any[]) {
    checkArrayWithCode.filter((check: any) => check.qtm).map((check) => check.success || check.exception).forEach(function(x) {
      if (!qtmVariationsArr.includes(x)) {
        qtmVariationsArr.push(x);
      }
    });
    //console.log('***** qtmvars array = ', qtmVariationsArr);
  }

  // eslint-disable-next-line
  const dirWheats: string = dir + '/wheats';
  // eslint-disable-next-line
  const dirChaffs: string = dir + '/chaffs';

  // eslint-disable-next-line
  const testFile: string = dir + '/test.arr';
  // console.log('tk testFile', testFile);

  // eslint-disable-next-line
  const hintsFile: string = dir + '/hints.json';
  let hintsFileContents: string = (fs.existsSync(hintsFile) ? fs.readFileSync(hintsFile, { encoding: 'utf8' }) : "{}");
  hintsFileContents = hintsFileContents.replace(/#\.CHUNK#/g, '');
  // console.log('hintsFileContents is', hintsFileContents);
  const hints =  JSON.parse(hintsFileContents);

  // eslint-disable-next-line
  const testWheatFile = dir + '/testwheat.arr';
  // eslint-disable-next-line
  const testChaffFile = dir + '/testchaff.arr';
  // eslint-disable-next-line
  const testMutantFile = dir + '/testmutant.arr';

  if (fs.existsSync(testWheatFile)) {
    fs.unlinkSync(testWheatFile);
  }
  if (fs.existsSync(testChaffFile)) {
    fs.unlinkSync(testChaffFile);
  }
  if (fs.existsSync(testMutantFile)) {
    fs.unlinkSync(testMutantFile);
  }

  // eslint-disable-next-line
  const wheatFileBasenames: string[] = (fs.existsSync(dirWheats) ? fs.readdirSync(dirWheats) : []).filter((f: string) => !(fs.lstatSync(dirWheats + '/' + f).isDirectory()));
  // eslint-disable-next-line
  const chaffFileBasenames: string[] = (fs.existsSync(dirChaffs) ? fs.readdirSync(dirChaffs) : []).filter((f: string) => !(fs.lstatSync(dirChaffs + '/' + f).isDirectory()));
  // eslint-disable-next-line
  const mutantFileBasenames: string[] = Object.keys(hints);

  const numWheats = wheatFileBasenames.length;
  const numChaffs = chaffFileBasenames.length;
  const numMutants = mutantFileBasenames.length;

  const wheatFiles: string[] = [];
  const chaffFiles: string[] = [];
  const mutantFiles: string[] = [];

  for (let i = 0; i < numWheats; i += 1) {
    // eslint-disable-next-line
    wheatFiles.push('wheats/' + wheatFileBasenames[i]);
  }
  for (let i = 0; i < numChaffs; i += 1) {
    // eslint-disable-next-line
    chaffFiles.push('chaffs/' + chaffFileBasenames[i]);
  }
  for (let i = 0; i < numMutants; i += 1) {
    // eslint-disable-next-line
    mutantFiles.push('chaffs/' + mutantFileBasenames[i] + '.arr');
  }

  // test if numPrograms > 0 and testFile exists
  const wheatResultArray: ExamplarResult[] = [];
  const chaffResultArray: ExamplarResult[] = [];

  let result: any;
  const testTemplate = String(fs.readFileSync(testFile));

  let firstUsableResult: any = false;
  let correspondingFile: string;
  let firstFailureResult: any = false;
  let numOfChecks: number = -1;

  const definitionsEditor = state.definitionsEditor;
  const doc = isInitializedEditor(definitionsEditor) ? definitionsEditor.getDoc().copy(true) : CodeMirror.Doc(definitionsEditor.getValue());
  await upwait((s: State) => handleRunProgramStart(s, doc));

  // console.log('tk START WHEATS ETC!!!');

  for (let i = 0; i < numWheats; i += 1) {
    const wheatFile = wheatFiles[i];
    const testProgram = insertUserImpl(testTemplate, wheatFile);
    // eslint-disable-next-line
    const testProgramFile = segmentName(testWheatFile, Number(i).toString())
    // eslint-disable-next-line
    result = await runTextProgram(typeCheck, runKind, testProgramFile, testProgram);
    // console.log('tk result=', result);
    if (result.type === 'compile-failure') {
      console.log('examplar wheat failure compiling', wheatFile, result);
      if (!firstFailureResult) {
        firstFailureResult = result;
      }
      wheatResultArray.push({
        success: false,
        result
      });
    } else if (result.type === 'run-failure') {
      console.log('examplar wheat failure runnning', wheatFile, result);
      if (!firstFailureResult) {
        firstFailureResult = result;
      }
      wheatResultArray.push({
        success: false,
        result
      });
    } else {
      if (!firstUsableResult) {
        firstUsableResult = result.result;
        numOfChecks = result.result.result.$checks.length;
        correspondingFile = testProgramFile;
      }
      const checkArray = result.result.result.$checks;
      // console.log('tk checkArray=', checkArray);
      const checkArrayWithCode = addCodeToCheckArray(checkArray, testProgram, numOfChecks);
      // console.log('tk checkArrayWithCode=', checkArrayWithCode);
      // getQtmVariations(checkArrayWithCode);
      // console.log('checkArrayWithCode=', checkArrayWithCode);
      const failureArray = checkArrayWithCode.filter((check: any) => (check.success === false));
      const success = (failureArray.length === 0);
      wheatResultArray.push({ success, result });
    }
  }

  const wheatFails = numFailures(wheatResultArray);

  // console.log('### numWheats=', numWheats, '; wheatFails=', wheatFails);

  // if wheatFails == 0, do chaffs, else do mutants

  if (numWheats > 0) {
    if (wheatFails === 0) {
      console.log('all wheats succeeded');
      for (let i = 0; i < numChaffs; i += 1) {
        const chaffFile = chaffFiles[i];
        const testProgram = insertUserImpl(testTemplate, chaffFile);
        // eslint-disable-next-line
        const testProgramFile = segmentName(testChaffFile, Number(i).toString())
        // eslint-disable-next-line
        result = await runTextProgram(typeCheck, runKind, testProgramFile, testProgram);
        // console.log('tk result=', result);
        if (result.type === 'compile-failure') {
          console.log('examplar chaff failure compiling', chaffFile, result);
          if (!firstFailureResult) {
            firstFailureResult = result;
          }
          chaffResultArray.push({
            success: false,
            result
          });
        } else if (result.type === 'run-failure') {
          console.log('examplar chaff failure running', chaffFile, result);
          if (!firstFailureResult) {
            firstFailureResult = result;
          }
          chaffResultArray.push({
            success: false,
            result
          });
        } else {
          if (!firstUsableResult) {
            firstUsableResult = result.result;
            numOfChecks = result.result.result.$checks.length;
            correspondingFile = testProgramFile;
          }
          const checkArray = result.result.result.$checks;
          // console.log('tk checkArray=', checkArray);
          const checkArrayWithCode = addCodeToCheckArray(checkArray, testProgram, numOfChecks);
          // console.log('tk checkArrayWithCode=', checkArrayWithCode);
          getQtmVariations(checkArrayWithCode);
          const failureArray = checkArrayWithCode.filter((check: any) => (check.success === false));
          const success = (failureArray.length === 0);
          chaffResultArray.push({ success, result });
        }
      }
    } else { // i.e., at least one wheat failed
      console.log('at least one wheat failed');
      //
      for (let i = 0; i < numMutants; i += 1) {
        // console.log('TRYING MUTANTS NOW');
        const mutantFile = mutantFiles[i];
        const testProgram = insertUserImpl(testTemplate, mutantFile);
        // eslint-disable-next-line
        const testProgramFile = segmentName(testMutantFile, Number(i).toString())
        // eslint-disable-next-line
        result = await runTextProgram(typeCheck, runKind, testProgramFile, testProgram);
        // console.log('tk result=', result);
        if (result.type === 'compile-failure') {
          console.log('examplar compile failure in mutant', mutantFile, result);
          if (!firstFailureResult) {
            firstFailureResult = result;
          }
        } else if (result.type === 'run-failure') {
          console.log('examplar run failure in mutant', mutantFile, result);
          if (!firstFailureResult) {
            firstFailureResult = result;
          }
        } else {
          if (!firstUsableResult) {
            firstUsableResult = result.result;
            numOfChecks = result.result.result.$checks.length;
            correspondingFile = testProgramFile;
          }
          const checkArray = result.result.result.$checks;
          // console.log('tk checkArray=', checkArray);
          const checkArrayWithCode = addCodeToCheckArray(checkArray, testProgram, numOfChecks);
          // console.log('tk checkArrayWithCode=', checkArrayWithCode);
          // getQtmVariations(checkArrayWithCode);
          const failureArray = checkArrayWithCode.filter((check: any) => (check.success === false));
          const mutantSucceeded = (failureArray.length === 0);
          // console.log('mutantSucceeded is', mutantSucceeded);
          if (mutantSucceeded) {
            // console.log('hintsfile is', hintsFile);
            // console.log('hints are', hints);
            // console.log('found succeeding mutant in', mutantFile);
            const mutantFileBasename = mutantFile.replace(/^chaffs\/(.+?)\.arr$/, '$1');
            // console.log('mutantFileBasename is', mutantFileBasename);
            // eslint-disable-next-line
            const mutantHint = hints[mutantFileBasename];
            if (mutantHint) {
              // eslint-disable-next-line
              hintMessage = "Hint: " + mutantHint.hint + '\n';
            } else {
              hintMessage = `${mutantFileBasename} has no hint\n`;
            }
            // console.log('hintMessage is', hintMessage);
            break;
          }
        }
      }
    }
  }

  if (firstUsableResult) {
    update((s: State) => handleRunExamplarSuccessFull(s, wheatResultArray, chaffResultArray, hintMessage, qtmVariationsArr.length, firstUsableResult, correspondingFile));
  } else if (firstFailureResult) {
    if (firstFailureResult.type === 'compile-failure') {
      update((s: State) => handleCompileExamplarFailure(s, firstFailureResult.errors));
    } else if (firstFailureResult.type === 'run-failure') {
      update((s: State) => handleRunExamplarFailure(s, firstFailureResult.error));
    }
  }
}

function stopSession(state: State): State {
  console.log('stopSession');
  console.assert(state.running);
  serverAPI.then(serverAPI => {
    serverAPI.stop().then((wasRunning) => {
      console.log('serverAPI.stop().then(console.log)', wasRunning);
      if (!wasRunning) {
        stopFlag = true;
      }
      update((s: State) => ({ ...s, running: { type: 'idle' } }));
    });
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
    case 'fileSync':
      return handleFileSync(state);
    case 'run':
      if (state.editorMode === EditorMode.Text) {
        return runProgramOrSegments(state, runProgramAsync, setupRunProgramAsync);
      }
      else if (state.editorMode === EditorMode.Chatitor) {
        return runProgramOrSegments(state, runSegmentsAsync, setupRunSegmentsAsync);
      }
      else if (state.editorMode === EditorMode.Examplaritor) {
        return runProgramOrSegments(state, runExamplarAsync, setupRunProgramAsync);
      }

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
    case 'ready':
      return handleReady(state);
    default:
      return state;
  }
}

export default function ideApp(state = initialState, action: Action): State {
  return rootReducer(state, action);
}
