import CodeMirror from 'codemirror';

export type Srcloc =
  | { $name: 'builtin', 'module-name': string }
  | {
    $name: 'srcloc',
    'source': string,
    'start-line': number,
    'start-column': number,
    'start-char': number,
    'end-line': number,
    'end-column': number,
    'end-char': number
  };

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

// Returns true if enter should send / defocus a chat *including configuration,
// modifier keys, and the text context (in smart mode)*
export function enterShouldSend(
  editor: CMEditor, enterNewline: boolean, event?: KeyboardEvent, pos?: CodeMirror.Position,
): boolean {
  const checkPos = pos ?? editor.getCursor();
  // eslint-disable-next-line
  const token = editor.getTokenAt(checkPos);
  const lineEndToken = editor.getTokenAt({ line: checkPos.line, ch: 99999 });
  // An enter anywhere on a single-line chat in which the ENTIRE chat is
  // codemirror-parsible
  // eslint-disable-next-line
    const singleLineEnter = editor.getValue().split('\n').length === 1 && lineEndToken.state.lineState.tokens.length === 0;
  const smartEnterCondition = singleLineEnter || token.state.lineState.tokens.length === 0;
  const smartEnter = smartEnterCondition && !enterNewline;
  if (event) {
    return (smartEnter || event.ctrlKey || event.metaKey) && !event.shiftKey;
  }
  return smartEnter;
}

export function cleanStopify() {
  // @ts-ignore
  window.$S = undefined;
  // @ts-ignore
  window.$__R = undefined;
  // @ts-ignore
  window.$__T = undefined;
  // @ts-ignore
  window.$top = undefined;
}

export const CHATITOR_SESSION = 'chatidor-session';
export const TEXT_SESSION = 'text-session';

// Precondition, srcloc is not a builtin
export function srclocToCodeMirrorPosition(
  loc: Srcloc,
): {from: CodeMirror.Position, to: CodeMirror.Position} {
  if (loc.$name === 'builtin') {
    throw new Error('builtin srcloc has no position');
  }
  const from = { line: loc['start-line'] - 1, ch: loc['start-column'] };
  const to = { line: loc['end-line'] - 1, ch: loc['end-column'] };
  return { from, to };
}

// file:///projects/examplar/testwheat.arr-0-segment:8:2-8:14
const matchLocation = /.*:(\d+):(\d+)-(\d+):(\d+)$/;
export function parseLocation(location : string) : Srcloc {
  const locPieces = location.match(matchLocation);
  if(locPieces === null) { throw new Error(`Cannot match ${location} as a location`); }
  
  // TODO(joe): Think about how to manage this representation when start/end-char are unknown.
  // They aren't currently used by highlighting (CodeMirror uses line/col), and they
  // aren't in the serialization some layers see.
  return {
      $name: 'srcloc',
      'source': locPieces[0],
      'start-line': Number(locPieces[1]),
      'start-column': Number(locPieces[2]),
      'start-char': 0,
      'end-line': Number(locPieces[3]),
      'end-column': Number(locPieces[4]),
      'end-char': 0,
    };
}