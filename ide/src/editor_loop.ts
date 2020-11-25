import { BackendCmd, EditorResponseLoop } from './state';

export default function backendCmdFromState(loop: EditorResponseLoop): BackendCmd {
  switch (loop) {
    case EditorResponseLoop.AutoCompile:
      return BackendCmd.Compile;

    case EditorResponseLoop.AutoCompileRun:
      return BackendCmd.Run;

    case EditorResponseLoop.Manual:
      return BackendCmd.None;

    default:
      throw new Error(`Unknown editor response loop kind: ${loop}`);
  }
}
