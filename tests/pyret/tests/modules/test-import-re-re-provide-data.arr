include file("./re-re-provide-data.arr")

cases(BT) mt:
  | mt => print("a")
  | node(_, _, _) => print("b")
end

check:
  t :: BT = mt
  mt satisfies is-object
  node satisfies is-function
  is-mt satisfies is-function
  is-node satisfies is-function
  is-BT satisfies is-function

end
