  graph:
  BOS = [PVD, WOR]
  WOR = [BOS]
  PVD = [BOS]
  end
  test-print(BOS.first == PVD)
  test-print(BOS.rest.first == WOR)
  test-print(WOR.first == BOS)
  test-print(PVD.first == BOS)

  tostring(BOS) == "[cyclic-field, cyclic-field]"
  test-print(torepr(BOS))
