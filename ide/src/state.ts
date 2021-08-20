/* Exports both the type of state in the Redux store as well as its default value. */

import { Chunk } from './chunk';
import { Effect } from './effect';
import { MenuItems } from './menu-types';
import { RHSObjects } from './rhsObject';
import { RTMessages } from './rtMessages';
import * as control from './control';
import { program } from './path';

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
  chunks: Chunk[],
  outdates: Outdates,
};

export type RunningState =
  | { type: 'idle' }
  | { type: 'text' }
  | { type: 'segments', total: number, done: number };

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

  /* The contents of the current file, or `undefined` if the current file has
     not yet been loaded. */
  currentFileContents: string | undefined,

  /* `true` if the compiler should be run with type checking, `false` otherwise. */
  typeCheck: boolean,

  /* The parsed module results of a Pyret program. Not read by Chatitor, which
   * prefers chunkToRHS */
  rhs: RHSObjects,

  /* Parsed messages from an executed/executing Pyret program */
  rtMessages: RTMessages,

  /* In text mode: the errors (if any) */
  interactionErrors: string[],

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

  /* Text mode only. Tracks error highlight source location spans (if any). */
  definitionsHighlights: number[][],

  /* The list of chunks. */
  chunks: Chunk[],

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

  /* Names of the menus. These appear in the top left of the page. */
  menuItems: MenuItems,

  /* The menu tab that is open, or false of no tab is open. */
  menuTabVisible: false | number,

  /* Chatitor only. Enter always inserts a newline when true. Otherwise enter
   * sends one-line chats and complete multi-line blocks. */
  enterNewline: boolean,

  /* Current tab index of the messages tab panel */
  messageTabIndex: MessageTabIndex,

  /* In text mode, whether the program is compiled/run after pauses to editing */
  /* TODO(luna): Do we really want this? */
  editorResponseLoop: EditorResponseLoop,

  editorLayout: EditorLayout,

  /* In Chatitor mode, when an edit has been made to any chunk, including
   * deletion or insertion, all subsequent chunks are "partially outdated" or
   * "technically outdated" - their values can mostly be believed, but may change
   * due to changes in definitions in previous chats */
  firstOutdatedChunk: number,
};

export enum EditorResponseLoop {
  AutoCompile,
  AutoCompileRun,
  Manual,
}

export enum MessageTabIndex {
  RuntimeMessages = 0,
  ErrorMessages = 1,
}

export enum EditorMode {
  Text = 'Text',
  Embeditor = 'Embeditor',
  Chatitor = 'Chatitor',
}

export enum EditorLayout {
  Compact = 'Compact',
  Normal = 'Normal',
}

export const initialState: State = {
  browseRoot: '/',
  browsePath: '/projects',
  currentFile: program,
  currentFileContents: undefined,
  typeCheck: true,
  rhs: {
    objects: [],
    outdated: false,
  },
  rtMessages: {
    messages: [],
    outdated: false,
  },
  interactionErrors: [],
  runKind: control.backend.RunKind.Async,
  editTimer: false,
  dropdownVisible: false,
  editorLoopDropdownVisible: false,
  editorMode: EditorMode.Chatitor,
  fontSize: 14,
  definitionsHighlights: [],
  effectQueue: [],
  isFileSaved: false,
  compiling: false,
  running: { type: 'idle' },
  footerMessage: 'Setting up (run may be slow)',
  chunks: [],
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
  enterNewline: false,
  messageTabIndex: MessageTabIndex.RuntimeMessages,
  editorResponseLoop: EditorResponseLoop.Manual,
  firstOutdatedChunk: 0,
  editorLayout: EditorLayout.Normal,
};
