import string-dict as D
import ast as A
import file("../src/arr/compiler/concat-lists.arr") as CL
import file("../src/arr/compiler/js-dag-utils.arr") as DAG
import file("../src/arr/compiler/js-ast.arr") as J

mk-name = A.s-name(A.dummy-loc, _)
check "Baseline used variables test":
  # var a = 1;
  # b = 2;
  # a = c;
  a = mk-name("a")
  b = mk-name("b")
  c = mk-name("c")
  prog1 = J.j-block(
    [CL.clist: J.j-var(a, J.j-num(1)),
      J.j-expr(J.j-assign(b, J.j-num(2))),
      J.j-expr(J.j-assign(a, J.j-id(c)))])

  used-vars = DAG.used-vars-jblock(prog1)

  used-vars is {
    [D.mutable-string-dict: a.key(), a, b.key(), b, c.key(), c];
    [D.mutable-string-dict:]}
end

check "Simple closure test":
  # var a = d;
  # function(c){ return a + b + c; }
  a = mk-name("a")
  b = mk-name("b")
  c = mk-name("c")
  d = mk-name("d")
  func = J.j-fun([CL.clist: c],
    J.j-block([CL.clist:
        J.j-return(J.j-binop(J.j-id(a), J.j-plus, J.j-binop(J.j-id(b), J.j-plus, J.j-id(c))))]))
  prog = J.j-block([CL.clist: J.j-var(a, J.j-id(d)), J.j-expr(func)])

  used-vars = DAG.used-vars-jblock(prog)

  used-vars is {
    [D.mutable-string-dict: a.key(), a, b.key(), b, d.key(), d];
    [D.mutable-string-dict: a.key(), a, b.key(), b]}
end

check "Nested closures":
  a = mk-name("a")
  a-id = J.j-id(a)
  b = mk-name("b")
  b-id = J.j-id(b)
  c = mk-name("c")
  c-id = J.j-id(c)
  d = mk-name("d")
  d-id = J.j-id(d)
  e = mk-name("e")
  e-id = J.j-id(e)

  # var a = e;
  # return function(c) {
  #   return b + (function(d){ return a + c + d; })(c);
  # };
  func-inner = J.j-fun([CL.clist: d],
    J.j-block([CL.clist:
        J.j-return(J.j-binop(a-id, J.j-plus,
            J.j-binop(c-id, J.j-plus, d-id)))]))
  func-outer = J.j-fun([CL.clist: c],
    J.j-block([CL.clist:
        J.j-return(J.j-binop(b-id, J.j-plus, J.j-app(func-inner, [CL.clist: c-id])))]))

  prog = J.j-block(
    [CL.clist: J.j-var(a, e-id), J.j-return(func-outer)])

  used-vars = DAG.used-vars-jblock(prog)

  used-vars is {
    [D.mutable-string-dict: a.key(), a, b.key(), b, e.key(), e];
    [D.mutable-string-dict: a.key(), a, b.key(), b]}

end