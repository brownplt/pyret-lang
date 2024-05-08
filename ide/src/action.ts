/* Exports all of the possible types of Redux actions that can be dispatched.
   The `Action` type is our type of Redux actions. */

import { Chunk } from './chunk';
import {
  EditorMode,
  MessageTabIndex,
  State,
} from './state';
import { Effect, EffectKey } from './effect';
import { RawRTMessage } from './rtMessages';
import * as control from './control';

export type EffectFailure =
  | { effectKey: 'stop' }
  | { effectKey: 'loadFile' }
  | { effectKey: 'saveFile', error: Error };

export type EffectSuccess =
  | { effectKey: 'stop', line: number }
  | { effectKey: 'loadFile' }
  | { effectKey: 'saveFile' };

// An undoable chunk update
export type UIChunksUpdate =
  | { key: 'insert', index: number, grabFocus?: boolean, text?: string }
  | { key: 'delete', index: number }
  | { key: 'merge', top: number, bottom: number }
  | { key: 'appendToDefinitions', index: number }
  | { key: 'clear' };

export type SuccessForEffect<E extends EffectKey> =
  Extract<EffectSuccess, { effectKey: E }>;

export type FailureForEffect<E extends EffectKey> =
  Extract<EffectFailure, { effectKey: E }>;

export type EffectSucceeded = { status: 'succeeded' } & SuccessForEffect<EffectKey>;

export type EffectFailed = { status: 'failed' } & FailureForEffect<EffectKey>;

export type EffectEnded = EffectSucceeded | EffectFailed;

// We don't need to recompile every time we change the chunks. Doing so would be
// needlessly inefficient; we only need to recompile when the text of the program
// changes, hence the `modifiesText` key.
export type MultipleChunkUpdate = { chunks: Chunk[], modifiesText: boolean };
export type SingleChunkUpdate = { chunk: Chunk, modifiesText: boolean };
export type ChunksUpdate = SingleChunkUpdate | MultipleChunkUpdate;

export function isMultipleChunkUpdate(update: ChunksUpdate): update is MultipleChunkUpdate {
  return (update as any).chunks !== undefined;
}

export function isSingleChunkUpdate(update: ChunksUpdate): update is SingleChunkUpdate {
  return (update as any).chunk !== undefined;
}

// TODO(alex): Split editor updates into a separate type
export type Update =
  | { key: 'editorMode', value: EditorMode }
  | { key: 'browsePath', value: string }
  | { key: 'currentFile', value: string }
  | { key: 'chunks', value: ChunksUpdate }
  | { key: 'fontSize', value: number }
  | { key: 'runKind', value: control.backend.RunKind }
  | { key: 'typeCheck', value: boolean }
  | { key: 'dropdownVisible', value: boolean }
  | { key: 'menuTabVisible', value: false | number }
  // store.ts: ide
  | { key: 'rt-message', value: RawRTMessage }
  // Editor.tsx
  | { key: 'messageTabIndex', value: MessageTabIndex }
  | { key: 'addReadyCallback', value: () => void }
  | { key: 'updater', value: (state : State) => State };

export type EffectStarted = { effect: number };

export type EnqueueEffect = { effect: Effect };

export type FileSync = { };

export type Action =
  | { type: 'effectStarted' } & EffectStarted
  | { type: 'effectEnded' } & EffectEnded
  | { type: 'enqueueEffect' } & EnqueueEffect
  | { type: 'fileSync' } & FileSync
  | { type: 'update' } & Update
  | { type: 'chunk' } & UIChunksUpdate
  | { type: 'undo' }
  | { type: 'redo' }
  | { type: 'ready' }
  | { type: 'run' }
  | { type: 'stopSession' };
