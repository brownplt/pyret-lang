/// Use this script to compile NON-TOPLEVEL javascript modules

const stopify = require('@stopify/stopify');
const fs = require("fs");

let input = process.argv[2]; 
let output = process.argv[3];

let content = fs.readFileSync(input);

let wrapped = "(function() {" + content + "})();";

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
let runner = stopify.stopifyLocally("(function() {})();", opts);

fs.writeFileSync(output, runner.compile(wrapped));
