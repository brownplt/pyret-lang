import { PyretList } from "./common-runtime-types";

const RUNTIME = require("./runtime.js");

var IS_LINK, IS_EMPTY, EMPTY, LINK;

function filter(f: (x: any) => boolean, list) {
  let acc = [];
  let current = list;

  while (IS_LINK(current)) {
    if (f(current.first)) {
      acc.push(current.first);
    }

    current = current.rest;
  }

  return arrayToList(acc);
}

function arrayToList(array) {
  let head = EMPTY;

  for (let i = array.length - 1; i >= 0; i--) {
    head = LINK(array[i], head);
  }

  return head;
}

function foldr(f, base, list) {

  function helper(acc, current) {
    if (IS_LINK(current)) {
      return f(helper(acc, current.rest), current.first);
    }

    return acc;
  }

  return helper(base, list);
}

function foldl(f, base, list) {
  let acc = base;
  let current = list;

  while (IS_LINK(current)) {
    acc = f(acc, current.first);
    current = current.rest;
  }

  return acc;
}

function map(f, list) {

  let resultHead = RUNTIME["$shallowCopyObject"](list);
  let writeHead = resultHead;

  let current = list;
  while (IS_LINK(current)) {
    writeHead.first = f(current.first);
    writeHead.rest = RUNTIME["$shallowCopyObject"](current.rest);

    writeHead = writeHead.rest;
    current = current.rest;
  }

  return resultHead;
}

function length(list) {
  let current = list;
  let count = 0;

  while (IS_LINK(current)) {
    count += 1;
    current = current.rest;
  }

  return count;
}

function sameLength(list1, list2) {
  let current = [list1, list2];

  while (IS_LINK(current[0]) && IS_LINK(current[1])) {
    current = [current[0].rest, current[1].rest];
  }

  return (IS_EMPTY(current[0]) && IS_EMPTY(current[1]));
}


module.exports = {
  "setup": function(setupObject) {
    IS_LINK = setupObject["is-link"];
    IS_EMPTY = setupObject["is-empty"];
    EMPTY = setupObject["empty"];
    LINK = setupObject["link"];
  },
  "perf-filter": filter,
  "perf-array-to-list": arrayToList,
  "perf-foldl": foldl,
  "perf-foldr": foldr,
  "perf-map": map,
  "perf-length": length,
  "perf-same-length": sameLength,
};
