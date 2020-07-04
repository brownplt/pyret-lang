import { Check } from './Check';
import { Chunk } from './chunk';
import { Effect } from './effect';
import { MenuItems } from './menu';
import { RHS } from './rhsObject';
import * as control from './control';

export type State = {
  effectQueue: Effect[];
  browseRoot: string,
  browsePath: string,
  currentFile: string,
  currentFileContents: string | undefined,
  typeCheck: boolean,
  checks: Check[],
  interactions: { key: any, name: any, value: any }[],
  rhs: RHS,
  interactionErrors: string[],
  lintFailures: LintFailures,
  runKind: control.backend.RunKind,
  autoRun: boolean,
  editTimer: NodeJS.Timer | false,
  dropdownVisible: boolean,
  editorMode: EditorMode,
  fontSize: number,
  message: string,
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
  checks: [],
  interactions: [{
    key: 'Note',
    name: 'Note',
    value: 'Press Run to compile and run',
  }],
  rhs: { objects: [] },
  interactionErrors: [],
  lintFailures: {},
  runKind: control.backend.RunKind.Async,
  autoRun: true,
  editTimer: false,
  dropdownVisible: false,
  editorMode: EditorMode.Chunks,
  fontSize: 12,
  message: 'Ready to rock',
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
};

export const CHUNKSEP = '#.CHUNK#\n';

export const makeResult = (
  result: any,
  moduleUri: string,
): { key: string, name: string, value: any }[] => {
  const compareLocations = (a: any, b: any): number => a.srcloc[1] - b.srcloc[1];

  // There may be toplevel expressions in many modules, but we only want to
  // show the ones from the main module we're working on
  const mainTraces = result.$traces.filter((t : any) => t.srcloc[0] === moduleUri);

  const allWithLocs = result.$locations.concat(mainTraces);

  // We combine and then sort to get the traces interleaved correctly with named values
  const allSorted = allWithLocs.sort(compareLocations);
  return allSorted.map((key: any) => {
    if ('name' in key) {
      return {
        name: key.name,
        key: key.name,
        line: key.srcloc[1],
        value: result[key.name],
      };
    }

    return {
      name: '',
      key: String(key.srcloc[1]),
      line: key.srcloc[1],
      value: key.value,
    };
  });
};
