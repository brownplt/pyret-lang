/* Exports all of the possible types of Redux actions that can be dispatched.
   The `Action` type is our type of Redux actions. */

import { Chunk } from './chunk';
import {
  EditorMode,
  MessageTabIndex,
  EditorResponseLoop,
} from './state';
import { Effect, EffectKey } from './effect';
import { RawRTMessage } from './rtMessages';
import * as control from './control';

export type EffectFailure =
  (| { effectKey: 'createRepl' }
  | { effectKey: 'startEditTimer' }
  | { effectKey: 'editTimer' }
  | { effectKey: 'setup' }
  | { effectKey: 'stop' }
  | { effectKey: 'loadFile' }
  | { effectKey: 'saveFile' }
  | { effectKey: 'setupWorkerMessageHandler' }
  | BackendEffectFailure);

export type EffectSuccess =
  (| { effectKey: 'createRepl' }
  | { effectKey: 'startEditTimer', timer: NodeJS.Timer }
  | { effectKey: 'editTimer' }
  | { effectKey: 'setup' }
  | { effectKey: 'stop', line: number }
  | { effectKey: 'loadFile' }
  | { effectKey: 'saveFile' }
  | { effectKey: 'setupWorkerMessageHandler' }
  | BackendEffectSuccess);

export type BackendEffectFailure =
  (| { effectKey: 'lint', name: string, errors: string[] } // TODO: check errors type
  | { effectKey: 'compile', errors: string[] }
  | { effectKey: 'run', errors: any });

export type BackendEffectSuccess =
  (| { effectKey: 'lint', name: string }
  | { effectKey: 'compile' }
  | { effectKey: 'run', result: any });

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
  (| { key: 'editorMode', value: EditorMode }
  | { key: 'currentRunner', value: any }
  | { key: 'currentFileContents', value: string }
  | { key: 'browsePath', value: string }
  | { key: 'currentFile', value: string }
  | { key: 'chunks', value: ChunksUpdate }
  | { key: 'focusedChunk', value: number | undefined }
  | { key: 'fontSize', value: number }
  | { key: 'runKind', value: control.backend.RunKind }
  | { key: 'typeCheck', value: boolean }
  | { key: 'shouldAdvanceCursor', value: boolean }
  | { key: 'dropdownVisible', value: boolean }
  | { key: 'menuTabVisible', value: false | number }
  | { key: 'firstSelectedChunkIndex', value: false | number }
  | { key: 'debugBorders', value: boolean }
  | { key: 'displayResultsInline', value: boolean }
  | { key: 'rhs', value: 'make-outdated' | 'reset-rt-messages' }
  | { key: 'rt-message', value: RawRTMessage }
  | { key: 'messageTabIndex', value: MessageTabIndex }
  | { key: 'editorResponseLoop', value: EditorResponseLoop }
  | { key: 'editorLoopDropdownVisible', value: boolean });

export type UpdateKey = Update['key'];

export type UpdateOfKey<K extends UpdateKey> = Extract<Update, { key: K }>;

export type UpdateOfKeyValue<K extends UpdateKey> = UpdateOfKey<K>['value'];

export type EffectStarted = { effect: number };

export type EnqueueEffect = { effect: Effect };

export type Action =
  (| { type: 'effectStarted' } & EffectStarted
  | { type: 'effectEnded' } & EffectEnded
  | { type: 'enqueueEffect' } & EnqueueEffect
  | { type: 'update' } & Update);

export type ActionType = Action['type'];

export type ActionOfType<T extends ActionType> = Extract<Action, { type: T }>;

export function isActionType<K extends ActionType>(
  actionType: K,
  action: Action,
): action is ActionOfType<K> {
  return actionType === action.type;
}
