#lang pyret

fun make-a-hash():
  for list.fold(the-hash from {}, i from list.range(0, 10000)):
    the-hash.{ [i.tostring()]: i }
  end
where:
  checkers.check-equals("", make-a-hash(), make-a-hash())
end
