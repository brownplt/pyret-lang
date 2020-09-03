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

export const emptySelection = {
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
  selection: Selection,
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
    selection: emptySelection,
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

export function selectAll(chunk: Chunk): Chunk {
  const anchor = { line: 0, ch: 0 };

  const lines = chunk.text.split('\n');

  // TODO: Are lines, characters zero-indexed? There might be an off-by-one
  // error here.
  const head = {
    line: lines.length - 1,
    ch: lines[lines.length - 1].length,
  };

  return {
    ...chunk,
    selection: { anchor, head },
  };
}

// Returns the number of characters from the start of `text` to the line and character
// location, `lineAndCh`, or `false` if `lineAndCh` isn't inside the text.
function getLineAndChIndex(text: string, lineAndCh: LineAndCh): false | number {
  const lines = text.split('\n');

  let characters = 0;

  for (let i = 0; i < lines.length; i += 1) {
    if (i === lineAndCh.line) {
      if (lines[i].length >= lineAndCh.ch) {
        return characters + lineAndCh.ch;
      }

      return false;
    }

    characters += lines[i].length + 1; // 1 for the newline character
  }

  return false;
}

function getSelectedText(text: string, selection: Selection): false | string {
  const anchorIndex = getLineAndChIndex(text, selection.anchor);
  const headIndex = getLineAndChIndex(text, selection.head);

  if (anchorIndex === false || headIndex === false) {
    return false;
  }

  return text.substring(anchorIndex, headIndex);
}

export function getChunkSelectedText(chunk: Chunk): string {
  const {
    text,
    selection,
  } = chunk;

  const selectedText = getSelectedText(text, selection);

  if (selectedText === false) {
    throw new Error(`Selection '${selection}' out of bounds for text '${text}'`);
  }

  return selectedText;
}

export function isEmptySelection(selection: Selection): boolean {
  return selection.anchor.line === 0
      && selection.anchor.ch === 0
      && selection.head.line === 0
      && selection.head.ch === 0;
}

export function removeSelectedText(chunk: Chunk): Chunk {
  const {
    selection,
    text,
  } = chunk;

  if (isEmptySelection(selection) === false) {
    const anchorIndex = getLineAndChIndex(text, selection.anchor);
    const headIndex = getLineAndChIndex(text, selection.head);

    if (anchorIndex === false || headIndex === false) {
      throw new Error(`Selection '${selection}' out of bounds for text '${text}'`);
    }

    const newText = text.substring(0, anchorIndex) + text.substring(headIndex + 1, text.length);

    return {
      ...chunk,
      text: newText,
      selection: emptySelection,
      errorState: notLintedState,
    };
  }

  return chunk;
}
