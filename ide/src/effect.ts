export type Effect =
  'loadFile'
  | 'saveFile'
  | 'setupWorkerMessageHandler'
  | 'createRepl'
  | 'lint'
  | 'compile'
  | 'run'
  | 'stop';
