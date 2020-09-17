import { PyretList } from "./common-runtime-types";

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

  let resultHead = Object.assign({}, list);
  let writeHead = resultHead;

  let current = list;
  while (IS_LINK(current)) {
    writeHead.first = f(current.first);
    writeHead.rest = Object.assign({}, current.rest);

    writeHead = writeHead.rest;
    current = current.rest;
  }

  return resultHead;
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
};
