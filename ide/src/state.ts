/* Exports both the type of state in the Redux store as well as its default value. */

import { CHUNKSEP, Chunk, UninitializedEditor } from './chunk';
import { Effect } from './effect';
import { MenuItems } from './menu-types';
import { RTMessages } from './rtMessages';
import * as control from './control';
import { program } from './path';
import { CMEditor, NeverError } from './utils';
import { Srcloc } from '../../src/runtime-arr/srcloc.arr';

const DEVELOPER_MODE = false;

export type Outdates =
  // Applying *or undoing* this change technically invalidates this chunk. If a
  // lower-index chunk has been edited, firstOutdatedChunk may not change
  // When undefined
  | { type: 'outdates', index: number }
  // Undoing this change sets firstOutdatedChunk to undoValidates
  // (clear)
  | { type: 'initializes', index: number };

// Includes the actual state as-of-the-event, but *also* additional information
// about how to reconstruct it as-needed
// NOTE(luna): right now, only the firstOutdatedChunk, which makes
// me wonder if that should be a computed property! The trouble is if we, say,
// delete a chunk, who holds the invalidation?)
type UndoState = {
  definitions: string,
  chunks: Chunk[],
  outdates: Outdates,
};

export type RunningState =
  | { type: 'idle' }
  | { type: 'text' }
  | { type: 'segments', total: number, done: number };

export type GoogleDriveFile = {
  id: string,
  name: string,
  body: string,
  mimeType: string,
  modifiedTime: string,
};
export type GoogleDriveProjectStructure = {
  id: string,
  name: string,
  files: GoogleDriveFile[],
  folders: GoogleDriveProjectStructure[],
};
export type ProjectState =
  | { type: 'scratch' }
  | { type: 'gdrive-pending' }
  | { type: 'gdrive', structure: GoogleDriveProjectStructure };

export type State = {

  /* Since Redux doesn't handle side effects for us we need to keep a queue of
     them inside the state. It needs to be a queue because we sometimes need to
     queue multiple side effects at a time. Side effects are not always
     processed in the same order. */
  effectQueue: Effect[];

  /* The file path that the file system browser considers the "root", usually
     "/". This can, however, be any valid directory. The file system will not
     traverse above its root. */
  browseRoot: string,

  /* The current directory in the file system browser. */
  browsePath: string,

  /* The path to the current file. */
  currentFile: string,

  /* If we are loading a project */
  projectState: ProjectState,

  /* `true` if the compiler should be run with type checking, `false` otherwise. */
  typeCheck: boolean,

  /* Parsed messages from an executed/executing Pyret program */
  rtMessages: RTMessages,

  /* The type of run (whether we should run with Stopify [async mode] or not [sync mode]) */
  runKind: control.backend.RunKind,

  /* Text mode only. This timer is started when an edit is made to the text of a
     program. When the timer finishes and auto run is on, the program is run. If
     the user edits the program while the timer is running it is reset back to
     0. This ensures that we don't send too many run requests which would be
     quite slow. */
  editTimer: NodeJS.Timer | false,

  // TODO(alex): split out editor UI state into its own type
  /* True if the dropdown to the right of the run button is visible, false otherwise. */
  dropdownVisible: boolean,

  /* True if the nested dropdown within the dropdown is visible */
  editorLoopDropdownVisible: boolean,

  /* Determines which kind of editor interface is shown, such as Text or Chatitor */
  editorMode: EditorMode,

  /* The current font size of the left hand side and right hand side of the
     page. Does not affect the size of text in the header and in menus. */
  fontSize: number,

  /* A chunk, intended to have results only, that appears above all other chunks.
     Usually it has an `editor` that refers to the definitions editor. */
  topChunk: Chunk | undefined,

  /* The list of chunks. */
  chunks: Chunk[],

  /* The index of the Chunk that was most recently focused, for showing its menu.
   * false indicates nothing focused */
  focusedChunk: number | false;

  /* Chunk undo history */
  past: UndoState[],
  future: UndoState[],

  /* True if the current file has been saved since the last edit, false otherwise. */
  /* Technically unused, but it actually looks pretty useful. Will keep around,
   * but will need to updated to runSession. */
  isFileSaved: boolean,

  /* True if the program is compiling, false if it is not, and 'out-of-date' if
     the program has been edited during a compile. 'out-of-date' is used to
     trigger a recompile before the program is run. */
  /* Rendered unused by the move to the promise-based API, but may be useful in
    * the future, even if not by Chatitor mode */
  compiling: boolean | 'out-of-date',

  /* Reflects whether anything is being run, AND its progress */
  running: RunningState,

  /* Message to display at bottom of page */
  footerMessage: string,

  /* Starts false, true once the user can click "Run" and have it start immediately */
  ready: boolean,

  /* List of callbacks for when the page is ready */
  readyCallbacks: (() => void)[],

  /* Message to display next to the file/options menu */
  headerMessage: string,

  /* Names of the menus. These appear in the top left of the page. */
  menuItems: MenuItems,

  /* The menu tab that is open, or false of no tab is open. */
  menuTabVisible: false | number,

  /* Chatitor only. Enter always inserts a newline when true. Otherwise enter
   * sends one-line chats and complete multi-line blocks. */
  enterNewline: boolean,

  /* Current tab index of the messages tab panel */
  messageTabIndex: MessageTabIndex,

  editorLayout: EditorLayout,

  /* In Chatitor mode, when an edit has been made to any chunk, including
   * deletion or insertion, all subsequent chunks are "partially outdated" or
   * "technically outdated" - their values can mostly be believed, but may change
   * due to changes in definitions in previous chats */
  firstOutdatedChunk: number,

  /* Because of limitations in tracking the environment per-chunk, we cannot
   * reliably re-run faithfully from firstOutdatedChunk. So some edits, like merging
   * cells and other multi-cell edits, simply require a re-run.
   * 
   * Starts true, then switches to false after each run
   * (then it's OK to append and just run the last one), then switches back on some edits */
  rerunAllChunks: boolean,

  /* Whether to show advanced options like Stopify, parent directory in
   * filesystem, modes */
  developerMode: boolean,

  /* The main definitions editor */
  definitionsEditor: UninitializedEditor | CMEditor
};

export enum MessageTabIndex {
  RuntimeMessages = 0,
  ErrorMessages = 1,
}

export enum EditorMode {
  Text = 'Text',
  Chatitor = 'Chatitor',
  Examplaritor = 'Examplaritor',
}

export enum EditorLayout {
  Compact = 'Compact',
  Normal = 'Normal',
}

export const initialState: State = {
  browseRoot: '/',
  browsePath: '/projects',
  currentFile: program,
  projectState: { type: 'scratch' },
  typeCheck: true,
  rtMessages: {
    messages: [],
    outdated: false,
  },
  runKind: control.backend.RunKind.Async,
  editTimer: false,
  dropdownVisible: false,
  editorLoopDropdownVisible: false,
  editorMode: EditorMode.Chatitor,
  fontSize: 14,
  effectQueue: [],
  isFileSaved: false,
  compiling: false,
  running: { type: 'idle' },
  ready: false,
  readyCallbacks: [],
  footerMessage: 'Setting up (run may be slow)',
  headerMessage: '',
  topChunk: undefined,
  chunks: [],
  focusedChunk: false,
  past: [],
  future: [],
  menuTabVisible: false,
  menuItems: [
    {
      name: 'Files',
      id: 'filesButton',
      icon: 'folderIcon.svg',
    },
    {
      name: 'Options',
      id: 'optionsButton',
      icon: 'gearIcon.svg',
    },
  ],
  enterNewline: true,
  messageTabIndex: MessageTabIndex.RuntimeMessages,
  firstOutdatedChunk: 0,
  rerunAllChunks: true,
  editorLayout: EditorLayout.Normal,
  developerMode: DEVELOPER_MODE,
  definitionsEditor: {
    getValue() { return ""; }
  },
};

// TODO(alex): Chunk file saving works by concating chunks together into a single buffer
//   and writing it out.
// If performance becomes bottlenecked here, consider:
//   * Storing chunks in a single string buffer and performing edits on that buffer
//   * Using fs.WriteStream to stream the chunk contents into the file
export function getCurrentFileContents(state : State) {
  const { editorMode, definitionsEditor, chunks } = state;
  const chunkContents = chunks.map((chunk) => chunk.editor.getValue()).join(CHUNKSEP);
  switch (editorMode) {
    case EditorMode.Text:
    case EditorMode.Examplaritor:
      const fullContents = `${definitionsEditor.getValue()}${CHUNKSEP}${chunkContents}`
      return fullContents
    case EditorMode.Chatitor:
      return chunkContents;
    default:
      throw new NeverError(editorMode);
  }
}