import { Controlled as CodeMirror } from 'react-codemirror2';
import { v4 as uuidv4 } from 'uuid';

export type ErrorState =
  ({ status: 'failed', effect: 'lint' | 'compile', failures: string[], highlights: number[][] }
  | { status: 'succeeded', effect: 'lint' | 'compile' }
  | { status: 'succeeded', effect: 'run', result: any }
  | { status: 'notLinted' });

export const lintSuccessState: ErrorState = {
  status: 'succeeded',
  effect: 'lint',
};

export const notLintedState: ErrorState = {
  status: 'notLinted',
};

type LineAndCh = { line: number, ch: number };
type Selection = { anchor: LineAndCh, head: LineAndCh };

const emptySelection = {
  anchor: { line: 0, ch: 0 },
  head: { line: 0, ch: 0 },
};

export type Chunk = {
  startLine: number,
  text: string,
  id: string,
  errorState: ErrorState,
  editor: false | CodeMirror.Editor;
  needsJiggle: boolean,
  selection: false | Selection,
};

export function getStartLineForIndex(chunks : Chunk[], index : number) {
  if (index === 0) { return 0; }

  return chunks[index - 1].startLine + chunks[index - 1].text.split('\n').length;
}

export function newId() {
  return uuidv4();
}

export function findChunkFromSrcloc(
  chunks: Chunk[],
  [file, l1] : [string, number],
  currentFile: string,
): number | false {
  if (file !== `file://${currentFile}`) {
    return false;
  }

  for (let i = 0; i < chunks.length; i += 1) {
    const end = chunks[i].startLine + chunks[i].text.split('\n').length;
    if (l1 >= chunks[i].startLine && l1 <= end) {
      return i;
    }
  }

  return false;
}

export function emptyChunk(options?: Partial<Chunk>): Chunk {
  return {
    startLine: 0,
    text: '',
    id: newId(),
    errorState: { status: 'notLinted' },
    editor: false,
    needsJiggle: false,
    selection: false,
    ...options,
  };
}

export function removeSelection(chunk: Chunk): Chunk {
  return {
    ...chunk,
    selection: emptySelection,
  };
}

export function removeAllSelections(chunks: Chunk[]): Chunk[] {
  return chunks.map(removeSelection);
}
