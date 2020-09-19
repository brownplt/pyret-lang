/* Exports types and functions for managing chunks. */

import { Controlled as CodeMirror } from 'react-codemirror2';
import { v4 as uuidv4 } from 'uuid';

export const CHUNKSEP = '#.CHUNK#\n';

/* Represents the current state of the chunk, whether it (1) has not yet been
   linted; (2) was successfully linted, but not compiled (3) was successfully
   compiled but not run; (4) was successfully run; (5...) failed at any of those
   steps.

   TODO(michael): this should probably just be called 'state' */
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

export type LineAndCh = { line: number, ch: number };
export type Selection = { anchor: LineAndCh, head: LineAndCh };

export const emptySelection = {
  anchor: { line: 0, ch: 0 },
  head: { line: 0, ch: 0 },
};

export type Chunk = {
  /* the line number of the first line of this chunk */
  startLine: number,

  /* the text in the chunk, not including the chunk separator, #.CHUNK# */
  text: string,

  /* a unique id */
  id: string,

  /* the current state of this chunk */
  errorState: ErrorState,

  /* the underlying CodeMirror instance. Can be false if the chunk has not yet
     been rendered. */
  editor: false | CodeMirror.Editor;

  /* Chunks used to jiggle when they had errors, but they don't anymore.
     TODO(michael): remove this field. */
  needsJiggle: boolean,

  /* The highlighted text in this chunk. We manage this ourselves (instead of
     letting CodeMirror do it) because it helps us re-render chunks properly; see
     shouldComponentUpdate() in DefChunk.tsx. */
  selection: Selection,
};

export function getStartLineForIndex(chunks : Chunk[], index : number) {
  if (index === 0) { return 0; }

  return chunks[index - 1].startLine + chunks[index - 1].text.split('\n').length;
}

export function newId() {
  return uuidv4();
}

/* Returns the chunk index of the chunk containing the specified source
   location, or false if none could be found. */
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

/* The function for creating chunks.

   Arguments:
     options: A partial Chunk. Any keys not provide here will be
              filled with defaults. */
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

/* Parses a string containing chunk separators (#.CHUNK#) into a list of Chunks. */
export function makeChunksFromString(s: string): Chunk[] {
  const chunkStrings = s.split(CHUNKSEP);
  let totalLines = 0;
  const chunks = chunkStrings.map((chunkString) => {
    const chunk: Chunk = emptyChunk({
      text: chunkString,
      startLine: totalLines,
      errorState: notLintedState,
    });

    totalLines += chunkString.split('\n').length;

    return chunk;
  });
  return chunks;
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
function getLineAndChIndex(text: string, lineAndCh: LineAndCh): number | false {
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

function getSelectedText(text: string, selection: Selection): string {
  const anchorIndex = getLineAndChIndex(text, selection.anchor);
  const headIndex = getLineAndChIndex(text, selection.head);

  if (anchorIndex !== false && headIndex !== false) {
    return text.substring(anchorIndex, headIndex);
  }

  return '';
}

export function getChunkSelectedText(chunk: Chunk): string {
  const {
    text,
    selection,
  } = chunk;

  const selectedText = getSelectedText(text, selection);

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

    if (anchorIndex !== false && headIndex !== false) {
      const newText = text.substring(0, anchorIndex) + text.substring(headIndex + 1, text.length);

      return {
        ...chunk,
        text: newText,
        selection: emptySelection,
        errorState: notLintedState,
      };
    }

    return {
      ...chunk,
      selection: emptySelection,
    };
  }

  return chunk;
}

// Returns 1 when `a` is at a larger index than `b`, -1 when the opposite is
// true, and 0 when they are at the same index.
export function compareLineAndCh(text: string, a: LineAndCh, b: LineAndCh): number {
  const aIndex = getLineAndChIndex(text, a);
  const bIndex = getLineAndChIndex(text, b);

  if (aIndex > bIndex) {
    return 1;
  }

  if (aIndex === bIndex) {
    return 0;
  }

  if (aIndex < bIndex) {
    return -1;
  }

  throw new Error('Error comparing srclocs');
}
