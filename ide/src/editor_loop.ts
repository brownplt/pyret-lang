import { BackendCmd, BackendEffectKey } from './effect';

import { EditorResponseLoop } from './state';

export function backendCmdFromState(loop: EditorResponseLoop): BackendCmd {
  switch (loop) {
    case EditorResponseLoop.AutoCompile:
      return 'compile';

    case EditorResponseLoop.AutoCompileRun:
      return 'run';

    case EditorResponseLoop.Manual:
      return 'none';

    default:
      throw new Error(`Unknown editor response loop kind: ${loop}`);
  }
}

export function backendContinue(k: BackendEffectKey | 'saveFile', cmd: BackendCmd): boolean {
  switch (k) {
    case 'saveFile':
      return cmd !== 'none';

    case 'lint':
      return (cmd === 'compile') || (cmd === 'run');

    case 'compile':
      return cmd === 'run';

    case 'run':
      return false;

    default:
      throw new Error(`Unreachable state: ${k}`);
  }
}
