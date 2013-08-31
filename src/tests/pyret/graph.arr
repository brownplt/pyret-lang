#lang pyret/whalesong

check:
  graph:
    BOS = [PVD, WOR]
    WOR = [BOS]
    PVD = [BOS]
  end
  print("Done creating graph")
  BOS.first is PVD
  print("Done comparing 1")
  BOS.rest.first is WOR
  WOR.first is BOS
  PVD.first is BOS
end

