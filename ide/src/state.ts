/* Exports both the type of state in the Redux store as well as its default value. */

import { Chunk } from './chunk';
import { BackendCmd, Effect } from './effect';
import { MenuItems } from './menu-types';
import { RHSObjects } from './rhsObject';
import { RTMessages } from './rtMessages';
import * as control from './control';

export { BackendCmd } from './effect';

export type State = {

  backendCmd: BackendCmd,

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

  /* The parsed module results of a Pyret program. */
  rhs: RHSObjects,

  /* Parsed messages from an executed/executing Pyret program */
  rtMessages: RTMessages,

  /* In text mode: the errors (if any). In chunk mode: the runtime errors (if
     any); lint and compile errors are tracked in the `chunks` array. */
  interactionErrors: string[],

  /* The type of run (whether we should run with Stopify [async mode] or not [sync mode]) */
  runKind: control.backend.RunKind,

  /* `true` if the program should auto run upon edit in text mode. In chunk mode
     this must always be `true` for the program to properly run.

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

  /* Determines whether the editor is in Chunk mode or Text mode. */
  editorMode: EditorMode,

  /* The current font size of the left hand side and right hand side of the
     page. Does not affect the size of text in the header and in menus. */
  fontSize: number,

  /* Text mode only. Tracks error highlight source location spans (if any). */
  definitionsHighlights: number[][],

  /* Unused. Replaced with a file-local runner in store.ts. TODO(michael):
     remove this. */
  currentRunner: any,

  /* The list of chunks. In Chunk mode, these are parsed into HTML in DefChunk.ts. */
  chunks: Chunk[],

  /* Chunk mode only. The currently focused chunk, i.e., the chunk that has a
     cursor blinking inside of it. */
  focusedChunk: number | undefined,

  /* Chunk mode only. Used for tracking whether or not to move the cursor into
     the subsequent chunk after Enter is pressed. */
  shouldAdvanceCursor: boolean,

  /* True if the current file has been saved since the last edit, false otherwise. */
  isFileSaved: boolean,

  /* True if the worker message handlers have been set up. See store.ts. */
  isSetupFinished: boolean,

  /* This seems to be a redundant version of `isSetupFinished`. TODO(michael): remove this. */
  isMessageHandlerReady: boolean,

  /* Unused */
  isReplReady: boolean,

  /* True if we are waiting for message handlers to be setup, false otherwise */
  settingUp: boolean,

  /* Unused */
  creatingRepl: boolean,

  /* True if the program is linting, false otherwise. */
  linting: boolean,

  /* True if the program has been linted since the last edit, false otherwise. */
  linted: boolean,

  /* True if the program is compiling, false if it is not, and 'out-of-date' if
     the program has been edited during a compile. 'out-of-date' is used to
     trigger a recompile before the program is run. */
  compiling: boolean | 'out-of-date',

  /* True if the program is running, false otherwise. */
  running: boolean,

  /* Names of the menus. These appear in the top left of the page. */
  menuItems: MenuItems,

  /* The menu tab that is open, or false of no tab is open. */
  menuTabVisible: false | number,

  /* Chunk mode only. The chunk that was selected first in a click-and-drag
     highlighting operation. This is used for determining how to highlight
     chunks properly. See DefChunk.tsx. */
  firstSelectedChunkIndex: false | number,

  /* Chunk mode only. True if the drag handles should be yellow / orange if
     their corresponding chunks have not been linted yet, false otherwise. This
     is useful for debugging issues where chunks are not linting properly. */
  debugBorders: boolean,

  /* Chunk mode only. True if results of a program should also be displayed
     below each chunk, false otherwise. */
  displayResultsInline: boolean,

  /* Current tab index of the messages tab panel */
  messageTabIndex: MessageTabIndex,

  editorResponseLoop: EditorResponseLoop,
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
  Chunks = 'Chunks',
  Text = 'Text',
  Embeditor = 'Embeditor',
  Chatitor = 'Chatitor',
}

export type LintFailure = {
  name: string,
  errors: string[]
};

export type LintFailures = {
  [name : string]: LintFailure
};

export const initialState: State = {
  backendCmd: BackendCmd.None,
  browseRoot: '/',
  browsePath: '/projects',
  currentFile: '/projects/program.arr',
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
  editorMode: EditorMode.Chunks,
  fontSize: 18,
  definitionsHighlights: [],
  currentRunner: undefined,
  shouldAdvanceCursor: false,
  effectQueue: [],
  isFileSaved: false,
  isMessageHandlerReady: false,
  isReplReady: false,
  isSetupFinished: false,
  settingUp: true,
  creatingRepl: false,
  linting: false,
  linted: false,
  compiling: false,
  running: false,
  chunks: [],
  focusedChunk: undefined,
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
  firstSelectedChunkIndex: false,
  debugBorders: false,
  displayResultsInline: false,
  messageTabIndex: MessageTabIndex.RuntimeMessages,
  editorResponseLoop: EditorResponseLoop.Manual,
};
