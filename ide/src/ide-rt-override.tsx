const IDE = (
  () => {
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
)();

export function defaultSpyMessage(data: { message: string, loc: string}) {
  // @ts-ignore
  const value = (data.message) ? data.message : undefined;
  // @ts-ignore
  return IDE.dispatch({
    type: 'update',
    key: 'rhs',
    value: {
      tag: 'spy-message',
      message: true,
      value,
      key: data.loc,
      loc: data.loc,
    },
  });
}

export function defaultSpyExpr(data: { key: string, value: any, loc: string}) {
  // @ts-ignore
  return IDE.dispatch({
    type: 'update',
    key: 'rhs',
    value: {
      tag: 'spy-value',
      value: {
        key: data.key,
        value: data.value,
      },
      key: data.loc,
      loc: data.loc,
    },
  });
}
