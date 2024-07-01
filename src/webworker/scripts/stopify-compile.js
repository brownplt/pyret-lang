/// Use this script to compile NON-TOPLEVEL javascript modules

const stopify = require('@stopify/stopify');
const fs = require("fs");

let input = process.argv[2]; 
let output = process.argv[3];

let content = fs.readFileSync(input);

let wrapped = "(function(require, exports, module) {" + content + "})(require, exports, module);";

// console.log("WRAPPED:", wrapped);
let opts = {
  // compileFunction: false,
  // getters: false,
  // debug: false,
  captureMethod: "lazy",
  newMethod: "direct",
  // es: "sane",
  jsArgs: "faithful",
  // requireRuntime: false,
  // compileMode: "normal",
};

// Hack (?) to compile modules in non-top-level stopify context
let runner = stopify.stopifyLocally("(function() {})();", opts, { stackSize: 30, restoreFrames: 10 });



if(output === undefined) {
  const rts = stopify.newRTS('lazy');
  rts.stackSize = 30;
  rts.restoreFrames = 10;
  rts.remainingStack = 30;

  runner.g = { console };

  const start = eval(runner.compile(wrapped));

  runner.eventMode = 0;

  runner.continuationsRTS.runtime(start, result => {
    console.log(result);
  });
}
else {
  fs.writeFileSync(output, runner.compile(wrapped));

}



