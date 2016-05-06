fun lst_to_strs(lst :: List<String>) -> Number:
  var result :: Number = 0
  for each(l from lst):
    result := result + l
  end
  result
end
