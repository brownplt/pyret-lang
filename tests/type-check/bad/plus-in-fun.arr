fun lst_to_strs(lst :: List<String>) -> Number block:
  var result :: Number = 0
  for each(l from lst):
    result := result + l
  end
  result
end
