provide {
    union: union,
    find: find,
} end

include string-dict

fun union(equiv :: MutableStringDict<String>, a :: String, b :: String):
  equiv.set-now(find(equiv, a), find(equiv, b))
end

rec shadow find = lam(equiv :: MutableStringDict<String>, a :: String) -> String:
  cases (Option) equiv.get-now(a) block:
    | none => 
      equiv.set-now(a, a)
      a
    | some(v) => 
      if v == a:
        v
      else:
        find(equiv, v)
      end
  end
end

check:
  sd = [mutable-string-dict: ]
  find(sd, "a") is "a"
  union(sd, "b", "c")
  find(sd, "b") is find(sd, "c")
  find(sd, "a") is-not find(sd, "c")
  union(sd, "b", "d")
  find(sd, "c") is find(sd, "d")
end
