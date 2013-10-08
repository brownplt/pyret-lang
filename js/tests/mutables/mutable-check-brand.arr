m = mk-mutable(5, fun(x): check-brand(String, x, "String") end, fun(x): true end)
m.get()
