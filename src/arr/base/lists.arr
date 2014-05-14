#lang pyret/library

provide list end
import option as O
import either as E

import list as lst

list = {
  list: lst.{ make: fun(arr): raw-array-to-list(arr) end},
  List: lst.List,
  is-empty: lst.is-empty,
  is-link: lst.is-link,
  empty: lst.empty,
  link: lst.link,
  range: lst.range,
  repeat: lst.repeat,
  filter: lst.filter,
  partition: lst.partition,
  split-at: lst.split-at,
  any: lst.any,
  find: lst.find,
  map: lst.map,
  map2: lst.map2,
  map3: lst.map3,
  map4: lst.map4,
  map_n: lst.map_n,
  map2_n: lst.map2_n,
  map3_n: lst.map3_n,
  map4_n: lst.map4_n,
  each: lst.each,
  each2: lst.each2,
  each3: lst.each3,
  each4: lst.each4,
  each_n: lst.each_n,
  each2_n: lst.each2_n,
  each3_n: lst.each3_n,
  each4_n: lst.each4_n,
  fold: lst.fold,
  fold2: lst.fold2,
  fold3: lst.fold3,
  fold4: lst.fold4,
  index: lst.index
}
