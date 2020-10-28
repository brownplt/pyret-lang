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

// NOTE(alex): Can't be bothered to properly configure eslint
export function foo() { }
