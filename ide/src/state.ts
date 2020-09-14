import { Chunk } from './chunk';
import { Effect } from './effect';
import { MenuItems } from './menu-types';
import { RHSObjects } from './rhsObject';
import * as control from './control';

export type State = {
  effectQueue: Effect[];
  browseRoot: string,
  browsePath: string,
  currentFile: string,
  currentFileContents: string | undefined,
  typeCheck: boolean,
  rhs: RHSObjects,
  interactionErrors: string[],
  runKind: control.backend.RunKind,
  autoRun: boolean,
  editTimer: NodeJS.Timer | false,
  dropdownVisible: boolean,
  editorMode: EditorMode,
  fontSize: number,
  definitionsHighlights: number[][],
  fsBrowserVisible: boolean,
  currentRunner: any,
  currentChunk: number,
  firstUpdatableChunk: number | undefined,
  chunks: Chunk[],
  focusedChunk: number | undefined,
  shouldAdvanceCursor: boolean,
  isFileSaved: boolean,
  isSetupFinished: boolean,
  isMessageHandlerReady: boolean,
  isReplReady: boolean,
  settingUp: boolean,
  creatingRepl: boolean,
  linting: boolean,
  linted: boolean,
  compiling: boolean | 'out-of-date',
  running: boolean,
  compiledSinceLastEdit: boolean,
  menuItems: MenuItems,
  menuTabVisible: false | number,
  firstSelectedChunkIndex: false | number,
  debugBorders: boolean,
  displayResultsInline: boolean,
};

export enum EditorMode {
  Chunks,
  Text,
}

export type LintFailure = {
  name: string,
  errors: string[]
};

export type LintFailures = {
  [name : string]: LintFailure
};

export const initialState: State = {
  browseRoot: '/',
  browsePath: '/projects',
  currentFile: '/projects/program.arr',
  currentFileContents: undefined,
  typeCheck: true,
  rhs: {
    objects: [],
    spyData: [],
    outdated: false,
  },
  interactionErrors: [],
  runKind: control.backend.RunKind.Async,
  autoRun: true,
  editTimer: false,
  dropdownVisible: false,
  editorMode: EditorMode.Chunks,
  fontSize: 12,
  definitionsHighlights: [],
  fsBrowserVisible: false,
  currentRunner: undefined,
  currentChunk: 0,
  firstUpdatableChunk: 0,
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
  compiledSinceLastEdit: false,
  chunks: [],
  focusedChunk: undefined,
  menuTabVisible: false,
  menuItems: [
    {
      name: 'Files',
      icon: 'folderIcon.svg',
    },
    {
      name: 'Options',
      icon: 'gearIcon.svg',
    },
  ],
  firstSelectedChunkIndex: false,
  debugBorders: false,
  displayResultsInline: false,
};
