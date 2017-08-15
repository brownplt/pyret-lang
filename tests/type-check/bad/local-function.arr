fun rev<T>(l :: List<T>) -> List<T>:
  fun h(shadow l, rv):
    cases (List) l:
      | empty => rv
      | link(f, r) =>
        h(r, link(f, rv))
    end
  end
  h(l, 3)
end
