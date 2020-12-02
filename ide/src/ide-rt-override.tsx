import { IDE } from './ide';

function getIDE(): IDE {
  try {
    // @ts-ignore
    return window.ide;
  } catch (e1) {
    try {
      // @ts-ignore
      return ide;
    } catch (e2) {
      throw new Error('Unable to find IDE object');
    }
  }
}

export function defaultSpyMessage(data: { message: string, loc: string}) {
  const value = (data.message) ? data.message : undefined;
  getIDE().dispatchSpyMessage(data.loc, value);
}

export function defaultSpyExpr(data: { key: string, value: any, loc: string}) {
  getIDE().dispatchSpyValue(data.loc, data.key, data.value);
}

export function defaultImageUrlProxy(url: string): string {
  // TODO(alex): Try doing something smarter in the future?
  //   This feels exploitable
  return `https://code.pyret.org/downloadImg?${url}`;
}

// Filter out builtin check block tests
// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function checkBlockFilter(srcloc: string, name: string): boolean {
  return !srcloc.includes('builtin');
}
