import { EditorMode } from "./state";
import { LintFailure } from "./DefChunks";

export type ideAction =
  { type: "setEditorMode", mode: EditorMode }
  | { type: "finishSetup" }
  | { type: "finishCreateRepl" }
  | { type: "queueRun" }
  | { type: "compile" }
  | { type: "run" }
  | { type: "finishRun" }
  | { type: "stop" }
  | { type: "runQueued" }
  | { type: "compile" }
  | { type: "compileFailure", errors: string[] }
  | { type: "runFailure", errors: string[] }
  | { type: "lintFailure", lintFailure: LintFailure }
  | { type: "lintSuccess", lintSuccess: { name: string }}
  | { type: "compileSuccess" }
  | { type: "runFinished", result: any }
  | { type: "updateRunner", runner: any }
  | { type: "beginStartup" }
  | { type: "startupCompleted" }
  | { type: "runStarted" }
  | { type: "updateContents", contents: string }
  | { type: "updateChunkContents", index: number, contents: string }
  | { type: "traverseUp", path: string }
  | { type: "traverseDown", path: string }
  | { type: "expandChild", path: string };

export type ideActionType = ideAction["type"];
