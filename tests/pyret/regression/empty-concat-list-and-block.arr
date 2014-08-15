# This test triggered an error in concat-lists and anf-loop-compiler,
# which were not properly checking for whether a concat-list was devoid of contents,
# or actually concat-empty, leading to a failure

# Additionally, once those were fixed, this program triggered a codepath in the compiler
# that had not been triggered before, that had a lingering type error.  Moreover,
# that whole code path was itself buggy and introduced names into the compiled code that
# used outside their defined scope.  All in all, this little program triggered several bugs,
# all of which are now fixed.

data Foo:
  | foo(id :: String)
end

fun bar(f) -> Number:
  cases(Foo) f:
    | foo(id :: String) => 5
  end
end

check:
  bar(foo("x")) is 5
  bar(foo(5)) raises ""
end
