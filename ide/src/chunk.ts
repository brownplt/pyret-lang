/* Exports types and functions for managing chunks. */

import { v4 as uuidv4 } from 'uuid';
import { Failure } from './failure';
import { RHSObject } from './rhsObject';

export const CHUNKSEP = '#.CHUNK#\n';

/* Represents the current state of the chunk, whether it (1) has not yet been
   linted; (2) was successfully linted, but not compiled (3) was successfully
   compiled but not run; (4) was successfully run; (5...) failed at any of those
   steps.

   TODO(michael): this should probably just be called 'state' */
export type ErrorState =
  { status: 'failed', failures: Failure[], highlights: number[][] };

export type ChunkResults = ErrorState | ({ status: 'succeeded', objects: RHSObject[] });

export type LineAndCh = { line: number, ch: number };
export type Selection = { anchor: LineAndCh, head: LineAndCh };

export const emptySelection = {
  anchor: { line: 0, ch: 0 },
  head: { line: 0, ch: 0 },
};

export type UninitializedEditor = {
  getValue: () => string,
  grabFocus?: boolean,
};

export type Chunk = {
  /* the line number of the first line of this chunk */
  startLine: number,

  /* a unique id */
  id: string,

  /* the underlying CodeMirror instance. Can be false if the chunk has not yet
     been rendered. */
  editor: UninitializedEditor | (CodeMirror.Editor & CodeMirror.Doc);

  /* Results for this chunk */
  results: ChunkResults,

  /* Whether this chunk has been changed since last run */
  outdated: boolean,
};

export function getStartLineForIndex(chunks : Chunk[], index : number) {
  if (index === 0) { return 0; }

  return chunks[index - 1].startLine + chunks[index - 1].editor.getValue().split('\n').length;
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
    const end = chunks[i].startLine + chunks[i].editor.getValue().split('\n').length;
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
    id: newId(),
    results: { status: 'succeeded', objects: [] },
    editor: { getValue: () => '' },
    outdated: true,
    ...options,
  };
}

/* Parses a string containing chunk separators (#.CHUNK#) into a list of Chunks. */
export function makeChunksFromString(s: string): Chunk[] {
  if (s === '') {
    return [];
  }
  const chunkStrings = s.split(CHUNKSEP);
  let totalLines = 0;
  const chunks = chunkStrings.map((chunkString) => {
    const chunk: Chunk = emptyChunk({
      editor: { getValue: () => chunkString },
      startLine: totalLines,
      results: { status: 'succeeded', objects: [] },
      outdated: true,
    });

    totalLines += chunkString.split('\n').length;

    return chunk;
  });
  return chunks;
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
