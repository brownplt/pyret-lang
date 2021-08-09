/* Exports the Effect type; a type for representing side-effects. This type is
   used when dispatching actions that enqueue effects. */

// Comments indicate who causes this effect and therefore cannot be deleted (yet)
export type EffectKey =
  // handleSetCurrentFileContents
  | 'startEditTimer'
  // handleStartEditTimer
  | 'editTimer'
  // Run.tsx
  | 'stop'
  // handleSetCurrentFile
  | 'loadFile'
  // initCmd
  | 'saveFile'
  // handleEditTimerSuccess, Run.tsx compile
  | 'initCmd';

export type Effect =
  | { effectKey: 'startEditTimer' }
  | { effectKey: 'editTimer' }
  | { effectKey: 'stop' }
  | { effectKey: 'loadFile' }
  | { effectKey: 'saveFile' }
  | { effectKey: 'initCmd', cmd: BackendCmd };

export enum BackendCmd {
  None = 0,
  Lint = 1,
  Compile = 2,
  Run = 3
}
