import { EditorMode } from "./Editor";

export type ideAction = setEditorMode | run;

export type setEditorMode = {
  type: "setEditorMode",
  mode: EditorMode,
}

export type run = {
  type: "run",
}

export function setEditorMode(mode: EditorMode): setEditorMode {
  return { type: "setEditorMode", mode };
}

export function run(): run {
  return { type: "run" };
}
