import CodeMirror from 'codemirror';

export type List<T> = { $name: 'empty' } | { $name: 'link', first: T, rest: List<T> };

// TODO(luna): These two no longer describe data values. This will cause
// problems. Check in about this
export type DataValue = { $brand: { names: string[] }, [key: string]: any };
export function isDataValue(x: any): x is DataValue {
  // https://eslint.org/docs/rules/no-prototype-builtins whoo that's a doozy
  // At some point, we should be more principled about this
  return Object.prototype.hasOwnProperty.call(x, '$brand')
    && Object.prototype.hasOwnProperty.call(x.$brand, 'names');
}

export function intersperse(array: Array<JSX.Element>,
  btwn: JSX.Element,
  last?: JSX.Element): Array<JSX.Element> {
  if (array.length <= 1) {
    return array;
  }
  return [
    array[0],
    ...array.slice(1, -1).flatMap((comp) => [btwn, comp]),
    last ?? btwn,
    array[array.length - 1],
  ];
}

export class NeverError extends Error {
  constructor(val: never) {
    super(`${JSON.stringify(val)} is never`);
  }
}

export type CMEditor = CodeMirror.Editor & CodeMirror.Doc;

// Returns true if
//     single-line|
// Or
//     line is wrapped |but
//     cursor is still first
// And false otherwise:
//     line is wrapped and
//     cursor| is later
export function isWrapFirst(editor: CMEditor, pos: CodeMirror.Position): boolean {
  const lineBegin = { line: pos.line, ch: 0 };
  return editor.charCoords(pos).top === editor.charCoords(lineBegin).top;
}
// Returns true if
//     single-line|
// Or
//     line is wrapped and
//     cursor is |on last wrapped line
// And false otherwise:
//     line is wrapped and
//     cursor is not |last
//     overall
export function isWrapLast(editor: CMEditor, pos: CodeMirror.Position): boolean {
  const lineBegin = { line: pos.line, ch: 9999999 };
  return editor.charCoords(pos).top === editor.charCoords(lineBegin).top;
}
