/* Exports types and functions for managing chunks. */

import { v4 as uuidv4 } from 'uuid';
import { Failure } from './failure';
import { RHSObject } from './rhsObject';
import { CMEditor } from './utils';

export const CHUNKSEP = '\n#---#\n';

/* Represents the current state of the chunk, whether it (1) has not yet been
   linted; (2) was successfully linted, but not compiled (3) was successfully
   compiled but not run; (4) was successfully run; (5...) failed at any of those
   steps.

   TODO(michael): this should probably just be called 'state' */
export type ErrorState =
  { status: 'failed', failures: Failure[] };

export type ChunkResults = ErrorState | ({ status: 'succeeded', objects: RHSObject[] });

export type UninitializedEditor = {
  getValue: () => string,
  grabFocus?: boolean,
};

export function isInitializedEditor(editor: CMEditor | UninitializedEditor): editor is CMEditor {
  return 'getDoc' in editor;
}

export type Chunk = {
  /* a unique id */
  id: string,

  /* the underlying CodeMirror instance. Can be false if the chunk has not yet
     been rendered. */
  editor: UninitializedEditor | CMEditor;

  /* Results for this chunk */
  results: ChunkResults,

  /* Whether this chunk has been changed since last run */
  outdated: boolean,

  /* Errors might reference prior chunks. This error might have results we want
   * to keep (or not), but we might want to link back to them */
  referencedFrom: string[],
};

function newId() {
  return uuidv4();
}

/* The function for creating chunks.

   Arguments:
     options: A partial Chunk. Any keys not provide here will be
              filled with defaults. */
export function emptyChunk(options?: Partial<Chunk>, initialText: string = ''): Chunk {
  return {
    id: newId(),
    results: { status: 'succeeded', objects: [] },
    editor: { getValue: () => initialText },
    outdated: true,
    referencedFrom: [],
    ...options,
  };
}

/* Parses a string containing chunk separators (#.CHUNK#) into a list of Chunks. */
export function makeChunksFromString(s: string): Chunk[] {
  if (s === '') {
    return [];
  }
  const chunkStrings = s.split(CHUNKSEP);
  const chunks = chunkStrings.map((chunkString) => {
    const chunk: Chunk = emptyChunk({
      editor: { getValue: () => chunkString },
      results: { status: 'succeeded', objects: [] },
      outdated: true,
    });

    return chunk;
  });
  return chunks;
}
