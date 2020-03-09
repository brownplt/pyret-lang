import { EditorMode } from "./Editor";
import { CompileState } from './State';

export type ideAction = setEditorMode | finishSetup | finishCreateRepl | queueRun | runText | finishRunText | stopText | compileText | textRunQueued | textCompileQueue | textCompileFailure | textRunFailure | textLintFailure | textLintSuccess | textCompileSuccess | textRunFinished | updateRunner | beginStartup | startupCompleted | textRunStarted | textUpdateContents | traverseUp | traverseDown | expandChild;

export type setEditorMode = {
  type: "setEditorMode",
  mode: EditorMode
}

export type finishSetup = {
  type: "finishSetup"
}

export type finishCreateRepl = {
  type: "finishCreateRepl"
}

export type queueRun = {
  type: "queueRun"
}

export type compileText = {
  type: "compileText"
}

export type runText = {
  type: "runText"
}

export type finishRunText = {
  type: "finishRunText"
}

export type stopText = {
  type: "stopText"
}

export type textRunQueued = {
  type: "textRunQueued"
}

export type textCompileQueue = {
  type: "textCompileQueue"
}

export type textCompileFailure = {
  type: "textCompileFailure",
  errors: string[]
}

export type textRunFailure = {
  type: "textRunFailure",
  errors: string[]
}

export type textLintFailure = {
  type: "textLintFailure",
  lintFailure: { name: string, errors: string[] }
}

export type textLintSuccess = {
  type: "textLintSuccess",
  lintSuccess: { name: string }
}

export type textCompileSuccess = {
  type: "textCompileSuccess"
}

export type textRunFinished = {
  type: "textRunFinished",
  result: any
}

export type updateRunner = {
  type: "updateRunner",
  runner: any
}

export type beginStartup = {
  type: "beginStartup"
}

export type startupCompleted = {
  type: "startupCompleted"
}

export type textRunStarted = {
  type: "textRunStarted"
}

export type textUpdateContents = {
  type: "textUpdateContents",
  contents: string
}

export type traverseUp = {
  type: "traverseUp",
  path: string[]
}

export type traverseDown = {
  type: "traverseDown",
  path: string[]
}

export type expandChild = {
  type: "expandChild",
  child: string,
  fullChildPath: string
}
