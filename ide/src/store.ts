/* Handles side effects. */

import { createStore } from 'redux';
import ideApp, { setStore, serverAPI, populateFromDrive } from './reducer';
import { IDE } from './ide';
import {
  EditorMode,
  getCurrentFileContents,
  initialState,
  State,
} from './state';
import {
  Chunk,
  makeChunksFromString,
  CHUNKSEP,
  UninitializedEditor,
  emptyChunk,
} from './chunk';
import { Action } from './action';
import { Effect } from './effect';
import * as control from './control';
import { CHATITOR_SESSION, CMEditor, NeverError, TEXT_SESSION } from './utils';
import { bfsSetup, fs } from './control';
import * as ideRt from './ide-rt-override';
import GoogleDrive from './Drive';

type Dispatch = (action: Action) => void;

/* Tracks the current runner of a running program that was compiled with
   Stopify. Used for stopping the program when the user hits the "stop" button. */
let currentRunner: any;

function handleLoadFile(
  dispatch: Dispatch,
  currentFile: string,
  editorMode: EditorMode,
) {
  const contents = control.openOrCreateFile(currentFile);
  const chunks = makeChunksFromString(contents);
  console.log("loadFile chunks", chunks);

  switch (editorMode) {
    case EditorMode.Text:
    case EditorMode.Examplaritor:
      const defs = chunks.length > 0 ? chunks[0].editor.getValue() : "";
      dispatch({ type: 'update', key: 'updater', value: (state : State) => ({...state,
          definitionsEditor: { getValue: () => defs },
          chunks: chunks.slice(1),
          topChunk: undefined
        })
      })
      break;
    case EditorMode.Chatitor: {
      dispatch({
        type: 'update',
        key: 'chunks',
        value: {
          chunks,
          modifiesText: true,
        },
      });
      break;
    }
    default:
      throw new NeverError(editorMode);
  }

  dispatch({
    type: 'effectEnded',
    status: 'succeeded',
    effectKey: 'loadFile',
  });
}

function handleSaveFile(
  dispatch: Dispatch,
  state: State
) {
  const saveCallback = (error: Error) => {
    if (error) {
      dispatch({
        type: 'effectEnded',
        status: 'failed',
        effectKey: 'saveFile',
        error,
      });
    } else {
      dispatch({
        type: 'effectEnded',
        status: 'succeeded',
        effectKey: 'saveFile',
      });
    }
  };
  const { currentFile } = state;
  const contents = getCurrentFileContents(state);
  control.fs.writeFile(currentFile, contents, saveCallback);
}

function handleStop(dispatch: Dispatch) {
  currentRunner.pause((line: number) => {
    dispatch({
      type: 'effectEnded',
      status: 'succeeded',
      effectKey: 'stop',
      line,
    });
  });
}

// Removes consecutive, duplicate effects, so we don't do extra work.
function collapseEffectQueue(effectQueue: Effect[]): Effect[] {
  const collapsedEffectQueue: Effect[] = [];
  let lastElement: undefined | Effect;

  effectQueue.forEach((effect) => {
    if (lastElement !== effect) {
      collapsedEffectQueue.push(effect);
      lastElement = effect;
    }
  });

  return collapsedEffectQueue;
}

/* Picks the first effect in the effect queue that can be applied. Returns an
   object containing the selected index and a function that applies the side
   effect. */
function handleFirstActionableEffect(
  state: State,
  dispatch: Dispatch,
): false | { effect: number, applyEffect: () => void } {
  const { effectQueue } = state;

  for (let i = 0; i < effectQueue.length; i += 1) {
    const effect = effectQueue[i];

    // console.log('doing effect.effectKey ', effect.effectKey);
    // unfortunately, 'startEditTimer' shows up on random runs, so deal with it
    switch (effect.effectKey) {
      case 'startEditTimer':
        break;
      case 'loadFile':
        {
          const { currentFile, editorMode } = state;
          // console.log('loadFile', currentFile, editorMode);
          if (currentFile !== undefined) {
            return {
              effect: i,
              applyEffect: () => handleLoadFile(dispatch, currentFile, editorMode),
            };
          }
        }
        break;
      case 'saveFile':
        {
          const {
            currentFile,
          } = state;
          if (currentFile !== undefined) {
            return {
              effect: i,
              applyEffect: () => handleSaveFile(dispatch, state)
            };
          }
        }
        break;
      case 'stop': {
        const { running } = state;
        if (running && currentRunner !== undefined) {
          return {
            effect: i,
            applyEffect: () => handleStop(dispatch),
          };
        }

        return {
          effect: i,
          applyEffect: () => { },
        };
      }
      default:
        // console.log('effect.effectKey is ', effect.effectKey);
        throw new Error('getFirstActionableEffect: unknown effect');
    }
  }

  return false;
}

const LOCALSTORAGE_STATE_NAME = 'anchor-settings';
const serializedState = localStorage.getItem(LOCALSTORAGE_STATE_NAME);
let loadedState = initialState;
if (serializedState) {
  loadedState = {
    ...initialState,
    ...JSON.parse(serializedState),
  };
}

const store = createStore(
  ideApp,
  loadedState,
  (window as any).__REDUX_DEVTOOLS_EXTENSION__
    && (window as any).__REDUX_DEVTOOLS_EXTENSION__({ trace: true, traceLimit: 25 }),
);
type SerializedOptions = {[key: string]: (string | number | boolean)};
let lastStorage: SerializedOptions;
store.subscribe(() => {
  const s = store.getState();
  const storage: SerializedOptions = {
    // effectQueue: shouldn't even exist
    browsePath: s.browsePath,
    browseRoot: s.browseRoot,
    currentFile: s.currentFile,
    typeCheck: s.typeCheck,
    // rhs / rtMessages / interactionErrors / definitionsHighlights: can/should
    // be gotten by running
    runKind: s.runKind,
    // editTimer: ~infinity between pageloads
    // *dropdownVisible / menuTabVisible / menuTabIndex: should be false on
    // load, visually
    editorMode: s.editorMode,
    fontSize: s.fontSize,
    // chunks: serialized into current file with #.CHUNK#
    // past/future: not serializable, and hard to remember across loads
    // isFileSaved: always true on load
    // compiling / running: always false on load
    // menuItems: shouldn't be in state probably
    enterNewline: s.enterNewline,
    editorLayout: s.editorLayout,
  };
  const keys = Object.keys(storage);
  for (let i = 0; i < keys.length; i += 1) {
    const key = keys[i];
    if (lastStorage === undefined || storage[key] !== lastStorage[key]) {
      localStorage.setItem(LOCALSTORAGE_STATE_NAME, JSON.stringify(storage));
      lastStorage = storage;
      return;
    }
  }
});




const ide: IDE = {
  dispatchSpyMessage: (loc: string, message: string | undefined) => {
    store.dispatch({
      type: 'update',
      key: 'rt-message',
      value: {
        tag: 'spy-message',
        message: true,
        value: message,
        loc,
      },
    });
  },
  dispatchSpyValue: (loc: string, key: string, value: any) => {
    store.dispatch({
      type: 'update',
      key: 'rt-message',
      value: {
        tag: 'spy-value',
        value: {
          key,
          value,
        },
        loc,
      },
    });
  },
};

// @ts-ignore
window.ide = ide;

/* This callback is triggered after a reducer is run. It looks though the effect
   queue and applies the first effect it can apply.

   This is somewhat of a hack to get around the fact that Redux doesn't have a
   built-in way of handling side effects. The logic of how this works goes
   something like this:
   - An action (see action.ts) is dispatched of type enqueueEffect
   - A reducer (see reducer.ts) adds the corresponding effect to state's effect queue
     (see state.ts)
   - This callback is triggered as a result of the reducer running
   - This callback selects a side effect to apply from the state's effect queue
   - This callback dispatches an action of type effectStarted
   - A reducer marks that action as active, by setting either compiling, linting, etc.
     to true. It also removes the effect from the effect queue.
   - This callback calls a function that does the side effect.
   - That function, when the side effect is finished, dispatches an effectEnded action.
   - A reducer marks that action as inactive, by setting either compiling, linting, etc.
     to false. */
store.subscribe(() => {
  const state = store.getState();

  const { dispatch } = store;
  const { effectQueue } = state;

  const collapsedState = {
    ...state,
    effectQueue: collapseEffectQueue(effectQueue),
  };

  const maybeEffect = handleFirstActionableEffect(collapsedState, dispatch);

  if (!maybeEffect) {
    return;
  }

  const { effect, applyEffect } = maybeEffect;

  dispatch({ type: 'effectStarted', effect });
  applyEffect();
});

/* Try to load a Chunk mode program from the URI component ?program=uri-encoded-program */

control.installFileSystem().then(() => {
  control.loadBuiltins();
  setStore(store);
  setup();
}).catch((e) =>{
  console.error("Failed to load fs: ", e);
})

function setup() {
  const params = new URLSearchParams(window.location.search);

  const drive = new GoogleDrive();
  const folderId = params.get('folder');
  
  function update(kv : Partial<State>) {
    store.dispatch({ type: 'update', key: 'updater', value: (s : State) => ({ ...s, ...kv }) });
  }
  
  if (folderId !== null) {
    update({
      projectState: { type: 'gdrive-pending' },
    });
    drive.getFileStructureFor(folderId)
      .then((structure) => {
        populateFromDrive(structure);
        const browsePath = `/google-drive/${folderId}/${structure.name}`;
        update({
          projectState: { type: 'gdrive', structure },
          browsePath,
          browseRoot: browsePath,
        });
        if (structure.files.length > 0) {
          update({
            currentFile: `${browsePath}/${structure.files[0].name}`,
          });
          store.dispatch({ type: 'enqueueEffect', effect: { effectKey: 'loadFile' } });
        } else {
          update({
            menuTabVisible: 0, // Need to make this a better API (this is the files menu)
          });
        }
        console.log('Structure is: ', structure);
      });
  } else {
    update({
      menuTabVisible: 0, // Need to make this a better API (this is the files menu)
    });
    store.dispatch({ type: 'enqueueEffect', effect: { effectKey: 'loadFile' } });
  }
  
  const maybeEncodedProgram: null | string = params.get('program');
  if (maybeEncodedProgram !== null) {
    const decodedProgram = decodeURIComponent(maybeEncodedProgram);
    const chunks: Chunk[] = makeChunksFromString(decodedProgram);
    store.dispatch({ type: 'update', key: 'chunks', value: { chunks, modifiesText: true } });
    store.dispatch({ type: 'enqueueEffect', effect: { effectKey: 'saveFile' } });
  }
  // Run `import cpo` at the very beginning to make first-time run more bearable
  {
    const saveFile = '/tmp/include-cpo.arr';
    const programText = 'import essentials2021 as __UNUSED_NAME';
    const { runKind } = store.getState();
    // We aren't going to lock running, because we don't want any interface
    // changes that may continue to be associated with it as this happens "in the
    // background" - this *is* safe, though it might not seem it because we are
    // saving to a special file in /tmp which runSegments won't unlink
    fs.writeFileSync(saveFile, programText);
    const sessionId = store.getState().editorMode === 'Chatitor' ? CHATITOR_SESSION : TEXT_SESSION;
    const { dir, base } = bfsSetup.path.parse(saveFile);
    serverAPI.then(serverAPI => {
      serverAPI.compileAndRun({
        baseDir: dir,
        program: base,
        builtinJSDir: control.path.compileBuiltinJS,
        checks: 'none',
        typeCheck: false,
        recompileBuiltins: false,
        session: sessionId,
      }, runKind, {
        cwd: dir,
        spyMessgeHandler: ideRt.defaultSpyMessage,
        spyExprHandler: ideRt.defaultSpyExpr,
        imgUrlProxy: ideRt.defaultImageUrlProxy,
        checkBlockFilter: ideRt.checkBlockFilter,
      }).then(() => {
        console.log('compiled and run `include cpo`');
        store.dispatch({ type: 'update', key: 'updater', value: (s: State) => ({ ...s, footerMessage: '' }) });
        store.dispatch({ type: 'ready' });
      });
    });
  }
  
}

export default store;
