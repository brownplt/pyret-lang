#lang racket/base

(require
  rackunit
  racket/set
  pyret/lang/ast
  pyret/lang/load)

(check-equal?
  (free-ids (parse-pyret "x = y y = x"))
  (set))

(check-equal?
  (free-ids (parse-pyret "fun f(): f where: f end"))
  (set))

(check-equal?
  (free-ids (parse-pyret "fun f(): f where: g end"))
  (set 'g))

(check-equal?
  (free-ids (parse-pyret "fun f(x :: Number(p)): f where: g end"))
  (set 'Number 'p 'g))

(check-equal?
  (free-ids (parse-pyret "provide a end"))
  (set 'a))

(check-equal?
  (free-ids (parse-pyret "fun (x :: Number(z)): x + y end"))
  (set 'Number 'z 'y))

(check-equal?
  (free-ids (parse-pyret "
graph:
x = [y, z]
y = [z]
z = [x, fun(): f end]
end"))
  (set 'f))

(check-equal?
  (free-ids (parse-pyret "x :: o.foo = 5"))
  (set 'o))

(check-equal?
  (free-ids (parse-pyret "cases(List) o: | bar(x :: Number(foo)) => x + zed end"))
  (set 'List 'o 'Number 'foo 'zed))

(check-equal?
  (free-ids (parse-pyret "data D: | var1 end D var1"))
  (set))
