import CodeMirror from 'codemirror';

export type List<T> = { $name: 'empty' } | { $name: 'link', first: T, rest: List<T> };

// TODO(luna): These two no longer describe data values. This will cause
// problems. Check in about this
export type DataValue = { $brand: { names: string[] }, [key: string]: any };
export function isDataValue(x: any): x is DataValue {
  // https://eslint.org/docs/rules/no-prototype-builtins whoo that's a doozy
  // At some point, we should be more principled about this
  return Object.prototype.hasOwnProperty.call(x, '$brand');
}

export function intersperse(array: Array<JSX.Element>, btwn: JSX.Element): Array<JSX.Element> {
  return array.slice(1).reduce((acc, comp) => acc.concat([btwn, comp]), [array[0]]);
}

export class NeverError extends Error {
  constructor(val: never) {
    super(`${val} is never`);
  }
}

export type CMEditor = CodeMirror.Editor & CodeMirror.Doc;
