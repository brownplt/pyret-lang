#
# NOTES:
#   * Needs to be compiled with compile-mode builtin-general to replace "file" imports
#   * NOTE(luna): Consistently in this file, commented out names mean i couldn't
#     find them.  This is usually because either they don't exist in anchor right
#     now, or they were tucked away in an unexpected module / called something
#     different / are an alias to something else

# Some of the modules are exposed by their names in CPO
provide:
  # module arrays
  module lists,
  module option,
  # module error,
  module sets,
end

#   * Translated from runtime-provides in arr/compiler/compile-structs.arr
provide from G:
  # test-print,
  print,
  # display,
  # print-error,
  # display-error,
  # tostring,
  # to-string,
  # torepr,
  # to-repr,
  # brander,
  raise,
  # NOTE(alex): including 'nothing' will cause a shadowing error
  # nothing,
  # builtins,
  not,
  # is-nothing,
  # is-number,
  # is-string,
  # is-boolean,
  # is-object,
  # is-function,
  # is-raw-array,
  # is-tuple,
  # is-table,
  # is-row,
  # gensym,
  # random,
  # run-task,
  _plus,
  _minus,
  _times,
  _divide,
  _lessthan,
  _lessequal,
  _greaterthan,
  _greaterequal,
end

provide from S:
  string-equal,
  string-contains,
  string-starts-with,
  string-ends-with,
  string-append,
  string-length,
  string-is-number as string-isnumber,
  string-is-number,
  string-to-number as string-tonumber,
  string-to-number,
  string-repeat,
  string-substring,
  string-replace,
  string-split,
  string-split-all,
  string-char-at,
  string-toupper,
  string-to-upper,
  string-tolower,
  string-to-lower,
  string-explode,
  string-index-of,
  string-to-code-point,
  string-from-code-point,
  string-to-code-points,
  string-from-code-points,
end

provide from G:
  time-now,
end

provide from N:
  num-random,
  num-random-seed,
  num-max,
  num-min,
  num-equal,
  num-truncate,
  num-ceiling,
  num-floor,
  num-round,
  num-round-even,
  # num-truncate-digits,
  # num-ceiling-digits,
  # num-floor-digits,
  # num-round-digits,
  # num-round-even-digits,
  # num-truncate-place,
  # num-ceiling-place,
  # num-floor-place,
  # num-round-place,
  # num-round-even-place,
  num-abs,
  num-sin,
  num-cos,
  num-tan,
  num-asin,
  num-acos,
  num-atan,
  num-atan2,
  num-modulo,
  # num-remainder,
  num-sqrt,
  num-sqr,
  num-log,
  num-exp,
  num-exact,
  num-to-rational,
  num-to-roughnum,
  num-is-positive,
  num-is-negative,
  num-is-non-positive,
  num-is-non-negative,
  num-is-integer,
  num-is-fixnum,
  num-is-rational,
  num-is-roughnum,
  num-expt,
  num-to-string as num-tostring,
  num-to-string,
  num-to-string-digits,
  # num-within-* are all aliases for num-within-rel-*
  num-within-rel as num-within,
  num-within-rel,
  num-within-abs,
  within-rel-now as within-now,
  within-rel,
  within-rel-now,
  within-abs,
  within-abs-now,
  within,
  within-rel-now3 as within-now3,
  within-rel3,
  within-rel-now3,
  within-abs3,
  within-abs-now3,
  within-rel3 as within3,
end

provide from RA:
  raw-array-get,
  raw-array-set,
  raw-array-of,
  raw-array-build,
  # raw-array-build-opt,
  raw-array-length,
  # raw-array-to-list,
  raw-array-fold,
  # raw-array-filter,
  # raw-array-and-mapi,
  # raw-array-or-mapi,
  raw-array-map,
  # raw-array-map-1,
  # raw-array-join-str,
  # raw-array-from-list,
  raw-array,
end

provide from G:
  # ref-get,
  # ref-set,
  # ref-freeze,
  equal-always,
  equal-always3,
  equal-now,
  equal-now3,
  # roughly-equal-always,
  # roughly-equal-always3,
  # roughly-equal-now,
  # roughly-equal-now3,
  # roughly-equal,
  identical,
  identical3,
  # exn-unwrap,
end

#   * Translated from standard-imports in arr/compiler/compile-structs.arr
# TODO(alex): make-array vs array?

provide from RA:
  raw-array as array,
  raw-array-length as array-length,
  raw-array-of as array-of,
  raw-array-set as array-set-now,
  raw-array-get as array-get-now,
  is-raw-array as is-array,
end

provide from lists:
  type List,
  to-raw-array as array-from-list,
  raw-array-to-list as array-to-list-now,

  list,
  is-List,
  is-empty,
  is-link,
  empty,
  link,
  range,
  range-by,
  repeat,
  filter,
  partition,
  split-at,
  any,
  find,
  map,
  map2,
  map3,
  map4,
  map_n,
  map2_n,
  map3_n,
  map4_n,
  each,
  each2,
  each3,
  each4,
  each_n,
  each2_n,
  each3_n,
  each4_n,
  fold,
  fold2,
  fold3,
  fold4
end

provide from option:
  type Option,
  data Option,
  is-Option,
  is-some,
  is-none,
  some,
  none
end

provide from sets:
  type Set,
  tree-set,
  list-set,
  empty-set,
  empty-tree-set,
  empty-list-set,
  list-to-set,
  list-to-list-set,
  list-to-tree-set,
end

import primitive-types as PT
# why doesn't anchor have arrays?
import raw-array as RA
import lists as lists
import option as option
# TODO(luna):
# import error as error
import sets as sets
import global as G
import string as S
import number as N
