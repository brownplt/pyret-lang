use context empty-context

# Translated from compile-structs.arr, standard-imports

import lists as lists
import sets as sets
import arrays as arrays
import error as error
import option as option
import global as global
import primitive-types as primitives
import string as string
import number as number
import raw-array as raw-array
import equality as equality
import tables as tables

# This line provides these as module bindings (e.g. all code can write
# lists.map as if it had written import lists as lists)
provide: module lists, module sets, module option, module arrays, module error end

provide from arrays:
  array,
  build-array,
  array-from-list,
  is-array,
  array-of,
  array-set-now,
  array-get-now,
  array-length,
  array-to-list-now,

  type Array
end

provide from option:
  is-Option,
  some,
  none,
  is-some,
  is-none,

  type Option
end

provide from lists:
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
  fold4,

  raw-array-to-list,
  
  type List
end

provide from sets:
  set,
  tree-set,
  list-set,
  empty-set,
  empty-list-set,
  empty-tree-set,
  list-to-set,
  list-to-list-set,
  list-to-tree-set,

  type Set
end

provide from global:
  print,
  #display,
  print-error,
  #display-error,
  to-string,
  to-string,
  to-repr,
  to-repr,
  raise,
  not,
  _plus,
  _minus,
  _times,
  _divide,
  _lessthan,
  _lessequal,
  _greaterthan,
  _greaterequal,
  time-now,

  type Number,
  type Exactnum,
  type Roughnum,
  type NumInteger,
  type NumRational,
  type NumPositive,
  type NumNegative,
  type NumNonPositive,
  type NumNonNegative,
  type String,
  type Table,
  type Row,
#  type Function,
  type Boolean,
  type Object,
  type Method,
  type Nothing,
  type RawArray,
end

provide from primitives:
  is-object,
  is-boolean,
  is-function,
  is-nothing,
  is-tuple,
  nothing
end

provide from number:
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
  num-truncate-digits,
  num-ceiling-digits,
  num-floor-digits,
  num-round-digits,
  num-round-even-digits,
  num-truncate-place,
  num-ceiling-place,
  num-floor-place,
  num-round-place,
  num-round-even-place,
  num-abs,
  num-sin,
  num-cos,
  num-tan,
  num-asin,
  num-acos,
  num-atan,
  num-atan2,
  num-modulo,
  num-remainder,
  num-sqrt,
  num-sqr,
  num-log,
  num-exp,
  num-exact,
  num-to-rational,
  num-to-roughnum,
  is-number,
  num-is-positive,
  num-is-negative,
  num-is-non-positive,
  num-is-non-negative,
  num-is-integer,
  num-is-fixnum,
  num-is-rational,
  num-is-roughnum,
  num-expt,
  num-to-string,
  num-to-string-digits,
  num-within,
  num-within-rel,
  num-within-abs,
  random,
end


provide from string:
  string-equal,
  string-contains,
  string-starts-with,
  string-ends-with,
  string-append,
  string-length,
  string-is-number,
  string-to-num,
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
  is-string
end

provide from equality:
  within-now,
  within-rel,
  within-rel-now,
  within-abs,
  within-abs-now,
  within,
  within-now3,
  within-rel3,
  within-rel-now3,
  within-abs3,
  within-abs-now3,
  within3,
  equal-always,
  equal-always3,
  equal-now,
  equal-now3,
  roughly-equal-always,
  roughly-equal-always3,
  roughly-equal-now,
  roughly-equal-now3,
  roughly-equal,
  identical,
  identical3,
end

provide from raw-array:
  is-raw-array,
  raw-array-get,
  raw-array-set,
  raw-array-of,
  raw-array-build,
  raw-array-build-opt,
  raw-array-length,
  raw-array-fold,
  raw-array-filter,
  raw-array-and-mapi,
  raw-array-or-mapi,
  raw-array-map,
  raw-array-map-1,
  raw-array-from-list,
  raw-array,
end

provide from tables:
  is-table,
  is-row,
end

provide:
  tostring,
  torepr,
  num-tostring,
  string-isnumber,
  string-is-num,
  string-tonumber,
  string-to-num-opt
end

# Due to a bug in how aliases are carried across, rename these "manually"
tostring = global.to-string
torepr = global.to-repr

num-tostring = number.num-to-string

string-isnumber = string.string-is-number
string-is-num = string.string-is-number
string-tonumber = string.string-to-num
string-to-num-opt = string.string-to-number