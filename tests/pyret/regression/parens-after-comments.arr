import ast as A
import file("../test-parse-helper.arr") as P

fun program-block(contents):
  A.s-program(A.dummy-loc, A.s-provide-none(A.dummy-loc), A.s-provide-types-none(A.dummy-loc),
    empty, empty, A.s-block(A.dummy-loc, contents))
end

check "https://github.com/brownplt/pyret-lang/issues/828":
  P.does-parse(```
### Comment 1
3 * 2

### Comment 2
(1 + 2)
                     ```) is true
  P.get-parse-result(```
### Comment 1
3 * 2

### Comment 2
(1 + 2)
                     ```).visit(A.dummy-loc-visitor)
    is program-block([list:
      A.s-op(A.dummy-loc, A.dummy-loc, "op*", A.s-num(A.dummy-loc, 3), A.s-num(A.dummy-loc, 2)),
      A.s-paren(A.dummy-loc, A.s-op(A.dummy-loc, A.dummy-loc, "op+", A.s-num(A.dummy-loc, 1), A.s-num(A.dummy-loc, 2)))
    ])

  P.get-parse-result("ab#|...|#cd").visit(A.dummy-loc-visitor)
    is program-block([list:
      A.s-id(A.dummy-loc, A.s-name(A.dummy-loc, "ab")),
      A.s-id(A.dummy-loc, A.s-name(A.dummy-loc, "cd"))
    ])

  # Because of the whitespace changes for block comments, the angle bracket
  # closing RawArray<DataSourceLoaderOption#|...|#> should be counted
  # as closing the type definition, even though it lexes as a GT now rather than
  # as a RANGLE
  P.does-parse(```
type DataSourceLoader#|<A,B>|# = {
  load :: (RawArray<String>, RawArray<DataSourceLoaderOption#|<A,B>|#> -> LoadedTable#|<A,B>|#)
}
               ```) is true
  
end
