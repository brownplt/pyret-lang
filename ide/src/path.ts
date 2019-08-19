import {path} from './browserfs-setup';

export const root = "/";
export const compileBase = path.join(root, "projects");
export const compileBuiltinJS = path.join(root, "prewritten");
export const compileProgram = "program.arr";
export const runBase = path.join(root, "compiled", "project");
export const runProgram = "program.arr.js";
export const uncompiled = path.join(root, "uncompiled");
export const program = path.join(compileBase, compileProgram);
export const pyretJarr = "pyret.jarr";
