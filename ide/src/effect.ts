/* Exports the Effect type; a type for representing side-effects. This type is
   used when dispatching actions that enqueue effects. */

export type EffectKey =
  (| 'createRepl'
   | 'startEditTimer'
   | 'editTimer'
   | 'setup'
   | 'stop'
   | 'loadFile'
   | 'saveFile'
   | 'setupWorkerMessageHandler'
   | BackendEffectKey);

export type BackendEffectKey =
  (| 'lint'
   | 'compile'
   | 'run'
   | 'stop');

export type Effect =
  (| { effectKey: 'createRepl' }
    | { effectKey: 'startEditTimer' }
    | { effectKey: 'editTimer' }
    | { effectKey: 'setup' }
    | { effectKey: 'stop' }
    | { effectKey: 'loadFile' }
    | { effectKey: 'saveFile' }
    | { effectKey: 'setupWorkerMessageHandler' }
    | BackendEffect);

export type BackendEffect =
  (| { effectKey: 'lint' }
    | { effectKey: 'compile' }
    | { effectKey: 'run' }
    | { effectKey: 'stop' });

