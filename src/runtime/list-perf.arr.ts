var IS_LINK, IS_EMPTY;

function perfFilter(f, list) {
  let previous = null;
  let current = list;
  while (IS_LINK(current)) {
    if (filterFn(current.first)) {
      previous = current;
    }

    current = current.rest;
  }
}

function perfFoldr(f, base, list) {
}

function perfFoldl(f, base, list) {
}

module.exports = {
  "setup": function(isLink, isEmpty) {
    IS_LINK = isLink;
    IS_EMPTY = isEmpty;
  },
  "perf-filter": perfFilter,
};
