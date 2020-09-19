/* Exports the Effect type; a type for representing side-effects. This type is
   used when dispatching actions that enqueue effects. */

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
