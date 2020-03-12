import { EditorMode } from "./Editor";
import { LintFailure } from "./DefChunks";

export type ideAction =
  { type: "setEditorMode", mode: EditorMode }
  | { type: "finishSetup" }
  | { type: "finishCreateRepl" }
  | { type: "queueRun" }
  | { type: "compile" }
  | { type: "runText" }
  | { type: "finishRunText" }
  | { type: "stopText" }
  | { type: "textRunQueued" }
  | { type: "textCompile" }
  | { type: "textCompileFailure", errors: string[] }
  | { type: "textRunFailure", errors: string[] }
  | { type: "textLintFailure", lintFailure: LintFailure }
  | { type: "textLintSuccess", lintSuccess: { name: string }}
  | { type: "textCompileSuccess" }
  | { type: "textRunFinished", result: any }
  | { type: "updateRunner", runner: any }
  | { type: "beginStartup" }
  | { type: "startupCompleted" }
  | { type: "textRunStarted" }
  | { type: "textUpdateContents", contents: string }
  | { type: "traverseUp", path: string }
  | { type: "traverseDown", path: string }
  | { type: "expandChild", path: string };

export type ideActionType = ideAction["type"];
