data Box<T>:
  | box(ref v :: T)
end

n1 = box(1)
n1!v