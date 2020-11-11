/* Exports the Effect type; a type for representing side-effects. This type is
   used when dispatching actions that enqueue effects. */

export type Effect =
  (| 'createRepl'
   | 'startEditTimer'
   | 'editTimer'
   | 'setup'
   | 'stop'
   | 'loadFile'
   | 'saveFile'
   | 'setupWorkerMessageHandler'
   | BackendEffect);

export type BackendEffect =
  (| 'lint'
   | 'compile'
   | 'run'
   | 'stop');
