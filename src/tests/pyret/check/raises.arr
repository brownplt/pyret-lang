#lang pyret

check:
  raise("an error") raises "err"
  {}.f raises "not found"
  {}.f raises "miss"
  raise("no match") raises "miss"
end
