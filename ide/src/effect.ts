export type Effect =
  (| 'createRepl'
   | 'startEditTimer'
   | 'editTimer'
   | 'lint'
   | 'compile'
   | 'run'
   | 'setup'
   | 'stop'
   | 'loadFile'
   | 'saveFile'
   | 'setupWorkerMessageHandler');
