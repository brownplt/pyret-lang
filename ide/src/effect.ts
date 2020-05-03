export type Effect =
  (| 'createRepl'
   | 'lint'
   | 'compile'
   | 'run'
   | 'setup'
   | 'stop'
   | 'loadFile'
   | 'saveFile'
   | 'setupWorkerMessageHandler');
