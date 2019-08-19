import { Equal, NotEqual, traceValue, isNotEqual } from "./runtime.arr";

let foo: NotEqual = {
  $brand: "foo",
  $tag: 1,
  reason: "bla",
  value1: 1,
  value2: 2,
};

let bar: NotEqual = NotEqual("foo", 1, 2);

console.log("foo proper:", isNotEqual(foo));
console.log("bar proper:", isNotEqual(bar));

let myRuntime = require("./runtime.arr.js");
console.log("foo pyret:", myRuntime["is-NotEqual"](foo));
console.log("bar pyret:", myRuntime["is-NotEqual"](bar));
