/* Exports types and functions for managing chunks. */

import { v4 as uuidv4 } from 'uuid';
import { Failure } from './failure';
import { RHSObject } from './rhsObject';
import { CMEditor } from './utils';
import CodeMirror from 'codemirror';

export const CHUNKSEP = '#.CHUNK#\n';
export type ChunkSnapshot = { start?: number, end?: number, editorAtLastRun: CodeMirror.Doc}
export type ChunkFail = { status: 'failed', failures: Failure[] } & ChunkSnapshot;
export type ChunkSuccess = { status: 'succeeded', objects: RHSObject[] } & ChunkSnapshot;
export type ChunkFirstRun = { status: 'running' } & ChunkSnapshot;

export type ChunkResults = ChunkFail | ChunkSuccess | ChunkFirstRun;

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
    results: { status: 'succeeded', objects: [], editorAtLastRun: CodeMirror.Doc(initialText) },
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
      results: { status: 'succeeded', objects: [], editorAtLastRun: CodeMirror.Doc(chunkString) },
      outdated: true,
    });

    return chunk;
  });
  return chunks;
}
