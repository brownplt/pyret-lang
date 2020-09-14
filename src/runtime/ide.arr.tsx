const RUNTIME = require("./runtime.js");

const IDE = (
  () => {
    // @ts-ignore
    if (window) {
      // @ts-ignore
      return window.ide;
    } else {
      try {
        // @ts-ignore
        return ide;
      } catch (e) {
        throw new Error("Unable to find IDE object");
      }
    }
  }
)();

RUNTIME.$setSpyMessageHandler((data: { message: string, loc: string}) => {

  // @ts-ignore
  const value = (data.message) ? data.message : undefined;
  // @ts-ignore
  IDE.dispatch({
    type: 'update',
    key: 'rhs',
    value: {
      tag: "spy-message",
      message: true,
      value,
      key: data.loc,
      loc: data.loc,
    },
  });
});

RUNTIME.$setSpyValueHandler((data: { key: string, value: any, loc: string}) => {
  // @ts-ignore
  IDE.dispatch({
    type: 'update',
    key: 'rhs',
    value: {
      tag: "spy-value",
      value: {
        key: data.key,
        value: data.value,
      },
      key: data.loc,
      loc: data.loc,
    },
  });
});
