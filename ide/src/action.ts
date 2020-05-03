import { Chunk } from './chunk';
import { EditorMode } from './state';
import { Effect } from './effect';

export type EffectFailure =
  (| { effect: 'createRepl' }
  | { effect: 'startEditTimer' }
  | { effect: 'editTimer' }
  | { effect: 'lint', name: string, errors: string[] } // TODO: check errors type
  | { effect: 'compile', errors: string[] }
  | { effect: 'run', errors: any }
  | { effect: 'setup' }
  | { effect: 'stop' }
  | { effect: 'loadFile' }
  | { effect: 'saveFile' }
  | { effect: 'setupWorkerMessageHandler' });

export type EffectSuccess =
  (| { effect: 'createRepl' }
  | { effect: 'startEditTimer', timer: NodeJS.Timer }
  | { effect: 'editTimer' }
  | { effect: 'lint', name: string }
  | { effect: 'compile' }
  | { effect: 'run', result: any }
  | { effect: 'setup' }
  | { effect: 'stop', line: number }
  | { effect: 'loadFile' }
  | { effect: 'saveFile' }
  | { effect: 'setupWorkerMessageHandler' });

export type SuccessForEffect<E extends Effect> =
  Extract<EffectSuccess, { effect: E }>;

export type FailureForEffect<E extends Effect> =
  Extract<EffectFailure, { effect: E }>;

export type EffectSucceeded = { status: 'succeeded' } & SuccessForEffect<Effect>;

export type EffectFailed = { status: 'failed' } & FailureForEffect<Effect>;

export type EffectEnded = EffectSucceeded | EffectFailed;

export type Update =
  (| { key: 'editorMode', value: EditorMode }
  | { key: 'currentRunner', value: any }
  | { key: 'currentFileContents', value: string }
  | { key: 'browsePath', value: string }
  | { key: 'currentFile', value: string }
  | { key: 'chunks', value: Chunk[] }
  | { key: 'focusedChunk', value: number });

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
