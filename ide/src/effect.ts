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
  | 'saveFile';

export type Effect =
  | { effectKey: 'startEditTimer' }
  | { effectKey: 'editTimer' }
  | { effectKey: 'stop' }
  | { effectKey: 'loadFile' }
  | { effectKey: 'saveFile' };
