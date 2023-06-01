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
import { getLocs } from './failure';
import { RunKind } from './backend';
import GoogleAPI from './Drive';
import { resetAsyncCacheToBuiltins } from './runner';

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
    case EditorMode.Examplaritor:
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

function handleSetChunks(state: State, chunksUpdate: ChunksUpdate): State {
  const { editorMode, isFileSaved } = state;
  if (editorMode !== EditorMode.Chatitor && editorMode !== EditorMode.Examplaritor) {
    throw new Error('handleSetChunks: not in chunk mode');
  }

  const {
    chunks,
    currentFileContents,
    firstOutdatedChunk,
  } = state;

  if (isMultipleChunkUpdate(chunksUpdate)) {
    let contents = currentFileContents;

    let newOutdate = firstOutdatedChunk;
    if (chunksUpdate.modifiesText) {
      const firstDiffering = chunksUpdate.chunks
        .findIndex((c, i) => chunks[i]?.editor.getValue() !== c.editor.getValue());
      newOutdate = firstDiffering === -1
        ? firstOutdatedChunk
        : Math.min(firstOutdatedChunk, firstDiffering);
    }

    if (chunksUpdate.modifiesText) {
      contents = chunksUpdate.chunks.map((chunk) => chunk.editor.getValue()).join(CHUNKSEP);
    }

    return {
      ...state,
      chunks: chunksUpdate.chunks,
      currentFileContents: contents,
      isFileSaved: isFileSaved && chunksUpdate.modifiesText === false,
      firstOutdatedChunk: newOutdate,
    };
  }

  if (isSingleChunkUpdate(chunksUpdate)) {
    const chunkIdOrNeg1 = chunks.findIndex((c) => c.id === chunksUpdate.chunk.id);
    const chunkId = chunkIdOrNeg1 === -1 ? firstOutdatedChunk : chunkIdOrNeg1;
    const newChunks = chunks.map((chunk) => (
      chunk.id === chunksUpdate.chunk.id ? chunksUpdate.chunk : chunk
    ));

    let contents = currentFileContents;

    if (chunksUpdate.modifiesText) {
      contents = newChunks.map((chunk) => chunk.editor.getValue()).join(CHUNKSEP);
    }

    return {
      ...state,
      chunks: newChunks,
      currentFileContents: contents,
      isFileSaved: isFileSaved && chunksUpdate.modifiesText === false,
      firstOutdatedChunk: Math.min(firstOutdatedChunk, chunkId),
    };
  }

  throw new NeverError(chunksUpdate);
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
  const { chunks, past, firstOutdatedChunk } = state;
  let newChunks: Chunk[];
  let outdates: Outdates;
  let nowOutdated;
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
    default:
      throw new NeverError(chunksUpdate);
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
  setImmediate(() => {
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
  });
  return state;
}
/*
function handleFileSync(state: State) : State {
  const google = new GoogleAPI();
  if (state.projectState.type !== 'gdrive') { return state; }

  function checkAndSave(base : string, file : GoogleDriveFile) : GoogleDriveFile {
    const filePath = `${base}/${file.name}`;
    const stats = fs.statSync(filePath);
    if (new Date(stats.mtime) > new Date(file.modifiedTime)) {
      console.log('Saving file', file.name, file.id);
      const contents = String(fs.readFileSync(filePath));
      google.saveFile(file, contents);
      return { ...file, modifiedTime: stats.mtime, body: contents };
    }
    return file;
  }
  function recursiveCheckAndSave(base : string, dir: GoogleDriveProjectStructure) : GoogleDriveProjectStructure {
    const dirPath = `${base}/${dir.name}/`;
    const files = dir.files.map((f) => checkAndSave(dirPath, f));
    const folders = dir.folders.map((f) => recursiveCheckAndSave(dirPath, f));
    return { ...dir, files, folders };
  }
  const { structure } = state.projectState;

  const projectPath = `/google-drive/${structure.id}/`;
  const newStructure = recursiveCheckAndSave(projectPath, structure);

  console.log('syncing files...', state.projectState);
  return { ...state, projectState: { type: 'gdrive', structure: newStructure } };
}
*/

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
    setImmediate(cb);
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

function handleRunSessionFailure(state: State, id: string, error: string, errorVal: any) : State {
  // NOTE(alex): necessary b/c Stopify does not clean up top level infrastructure,
  //   resulting in a severe memory leak of 50+MB PER RUN
  cleanStopify();
  const newChunks = removeReferencesFrom(state.chunks, id);
  const index = newChunks.findIndex((c) => c.id === id);
  const failureAsED : any = errorVal.errorDisplay ?? { $name: 'text', str: error };
  newChunks[index] = {
    ...newChunks[index],
    results: {
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
  return {
    ...state,
    interactionErrors: [error],
    definitionsHighlights: [],
    messageTabIndex: MessageTabIndex.ErrorMessages,
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

function handleCompileProgramFailure(state: State, errors: string[]) : State {
  const failures = errors.map((e) => JSON.parse(e));
  const places = failures.flatMap(getLocs);
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

function handleCompileExamplarFailure(state: State, errors: string[]) : State {
  return {
    ...state,
    interactionErrors: errors,
    definitionsHighlights: [],
    messageTabIndex: MessageTabIndex.ErrorMessages,
  };
}

function handleRunProgramFailure(state: State, error: string) : State {
  // TODO(joe): get source locations from dynamic errors (source map, etc)
  return {
    ...state,
    // compiling: false,
    interactionErrors: [error],
    definitionsHighlights: [],
    messageTabIndex: MessageTabIndex.ErrorMessages,
  };
}

function handleRunProgramSuccess(state : State, result : any) : State {
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

function handleRunExamplarSuccess(state: State, result: any) : State {
  // console.log('doing handleRunExamplarSuccess', state.currentFile);
  const rhs = makeRHSObjects(result, `file://${state.currentFile}`);
  cleanStopify();
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
  const result = await runTextProgram(typeCheck, runKind, currentFile, currentFileContents ?? '');
  if (result.type === 'compile-failure') {
    update((s: State) => handleCompileProgramFailure(s, result.errors));
  } else if (result.type === 'run-failure') {
    update((s: State) => handleRunProgramFailure(s, result.error));
  } else {
    update((s: State) => handleRunProgramSuccess(s, result.result));
  }
}

async function runExamplarAsync(state: State) : Promise<any> {
  // currentFile is just the standard program.arr, we'll use it to get at
  // our relevant files
  const { typeCheck, runKind, currentFile } = state;
  const { dir } = bfsSetup.path.parse(currentFile);
  // eslint-disable-next-line
  const dirChaffs: string = dir + '/chaffs';
  // eslint-disable-next-line
  const dirWheats: string = dir + '/wheats';
  // eslint-disable-next-line
  const testFile: string = dir + '/test.arr';
  // eslint-disable-next-line
  const testbedFile: string = dir + '/testbed.arr';
  if (fs.existsSync(testbedFile)) {
    fs.unlinkSync(testbedFile);
  }
  const wheatFiles: string[] = fs.existsSync(dirWheats) ? fs.readdirSync(dirWheats) : [];
  const chaffFiles: string[] = fs.existsSync(dirChaffs) ? fs.readdirSync(dirChaffs) : [];

  const implFiles: string[] = [];
  for (let i: number = 0; i < wheatFiles.length; i += 1) {
    // eslint-disable-next-line
    implFiles.push(dirWheats + '/' + wheatFiles[i]);
  }
  for (let i: number = 0; i < chaffFiles.length; i += 1) {
    // eslint-disable-next-line
    implFiles.push(dirChaffs + '/' + chaffFiles[i]);
  }

  const numPrograms: number = implFiles.length;

  // test if numPrograms > 0 and testFile exists

  const resultArray: any[] = [];
  let result: any;
  let failed: boolean = false;
  const checkBlock = String(fs.readFileSync(testFile));
  for (let i = 0; i < numPrograms; i += 1) {
    const sampleImpl = String(fs.readFileSync(implFiles[i]));
    // eslint-disable-next-line
    const testProgram = sampleImpl + '\n' + checkBlock + '\n';
    // eslint-disable-next-line
    const testProgramFile = segmentName(testbedFile, Number(i).toString()) + '.arr';
    // eslint-disable-next-line
    result = await runTextProgram(typeCheck, runKind, testProgramFile, testProgram);
    if (result.type === 'compile-failure') {
      failed = true;
      // eslint-disable-next-line
      update((s: State) => handleCompileExamplarFailure(s, result.errors));
      break;
    } else if (result.type === 'run-failure') {
      failed = true;
      // eslint-disable-next-line
      update((s: State) => handleRunExamplarFailure(s, result.error));
      break;
    } else {
      resultArray.push(result);
    }
  }
  if (!failed) {
    update((s: State) => handleRunExamplarSuccess(s, resultArray.length > 0 ? resultArray[0].result : null));
  }
}

function setupRunProgramAsync(state: State) : State {
  return { ...state, running: { type: 'text' } };
}

function setupRunExamplarAsync(s: State) : State {
  const { chunks } = s;
  const count = chunks.length;
  return { ...s, running: { type: 'examplar', total: count, done: 0 } };
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
  // If we are re-running the whole program, make sure to also re-run anything
  // that's imported, etc. This reset function removes *everything* but the
  // builtins from the cache, which is also a good eager GC thing to do.
  if (!onlyLastSegmentChanged) {
    resetAsyncCacheToBuiltins();
  }
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
  filenames.forEach((f) => {
    fs.unlinkSync(f);
    console.log(f);
  });
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
  // TODO(luna): reset rt messages?
  if (state.running.type !== 'idle') { return state; }
  const result : Promise<any> = runner(state);
  result.finally(() => {
    store.dispatch(
      { type: 'update', key: 'updater', value: (s) => ({ ...s, running: { type: 'idle' } }) },
    );
  });
  return updater(state);
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
    case 'fileSync':
      return handleFileSync(state);
    case 'run':
      if (action.key === 'runProgram') {
        return runProgramOrSegments(state, runProgramAsync, setupRunProgramAsync);
      }
      if (action.key === 'runSegments') {
        return runProgramOrSegments(state, runSegmentsAsync, setupRunSegmentsAsync);
      }
      if (action.key === 'runExamplar') {
        return runProgramOrSegments(state, runExamplarAsync, setupRunExamplarAsync);
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
    case 'ready':
      return handleReady(state);
    default:
      return state;
  }
}

export default function ideApp(state = initialState, action: Action): State {
  return rootReducer(state, action);
}
