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

function perfFoldr(f, base, list) {
}

function perfFoldl(f, base, list) {
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
};
