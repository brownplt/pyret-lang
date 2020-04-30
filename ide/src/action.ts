import { Chunk } from './chunk';
import { EditorMode } from './state';
import { Effect } from './effect';

export type AsyncProcess = 'createRepl' | 'lint' | 'compile' | 'run';

export type AsyncFailure =
  ({ process: 'createRepl' }
  | { process: 'setupWorkerMessageHandler' }
  | { process: 'setup' }
  | { process: 'lint', name: string, errors: string[] } // TODO: check errors type
  | { process: 'compile', errors: string[] }
  | { process: 'run', errors: any });

export type AsyncSuccess =
  ({ process: 'createRepl' }
  | { process: 'setupWorkerMessageHandler' }
  | { process: 'setup' }
  | { process: 'lint', name: string }
  | { process: 'compile' }
  | { process: 'run', result: any });

export type AsyncSuccessForProcess<P extends AsyncProcess>
  = Extract<AsyncSuccess, { process: P }>;

export type AsyncFailureForProcess<P extends AsyncProcess>
  = Extract<AsyncFailure, { process: P }>;

export type AsyncStatus<P extends AsyncProcess> =
  ({ status: 'started', process: P }
  | { status: 'succeeded', process: P } & AsyncSuccessForProcess<P>
  | { status: 'failed', process: P } & AsyncFailureForProcess<P>);

export type Update =
  ({ key: 'editorMode', value: EditorMode }
  | { key: 'isMessageHandlerReady', value: boolean }
  | { key: 'effectQueue', value: Effect[] }
  | { key: 'currentRunner', value: any }
  | { key: 'currentFileContents', value: string }
  | { key: 'browsePath', value: string }
  | { key: 'currentFile', value: string }
  | { key: 'chunks', value: Chunk[] }
  | { key: 'focusedChunk', value: number });

export type UpdateKey = Update['key'];

export type UpdateOfKey<K extends UpdateKey> = Extract<Update, { key: K }>;

export type UpdateOfKeyValue<K extends UpdateKey> = UpdateOfKey<K>['value'];

export type Action =
  ({ type: 'setAsyncStatus' } & AsyncStatus<AsyncProcess>
  | { type: 'queueEffect', effect: Effect }
  | { type: 'update' } & Update);

export type ActionType = Action['type'];

export type ActionOfType<T extends ActionType> = Extract<Action, { type: T }>;

export function isActionType<K extends ActionType>(
  actionType: K,
  action: Action,
): action is ActionOfType<K> {
  return actionType === action.type;
}
