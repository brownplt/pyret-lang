const RUNTIME = require("./runtime.js");

RUNTIME.$setSpyMessageHandler((data: { message: string, loc: string}) => {

  // @ts-ignore
  if (dispatch) {
    // @ts-ignore
    dispatch({
      type: 'update',
      key: 'rhs',
      value: data,
    });
    console.log("Dispatch");
  } else {
    console.log("No available dispatch");
  }

  console.log("SPY MESSAGE");
});

RUNTIME.$setSpyValueHandler((data: { key: string, value: any, loc: string}) => {
  console.log("SPY VALUE");
});
