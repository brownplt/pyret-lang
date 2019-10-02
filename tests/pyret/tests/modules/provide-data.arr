provide:
  data BT
end

data BT:
  | mt
  | node(v :: Number, left :: BT, right :: BT)
end
