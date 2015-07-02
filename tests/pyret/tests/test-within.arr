check "num-within-abs":
   1  is%(num-within-abs(0.1))       1
   1  is%(num-within-abs(0.1))      ~1
  ~3  is%(num-within-abs(0.1))      ~3
  ~2  is-not%(num-within-abs(0.1))  ~3
  ~2  is%(num-within-abs(1.1))      ~3
  ~2  is%(num-within-abs(1))        ~3
  ~2  is%(num-within-abs(~1))       ~3
  ~2  is-not%(num-within-abs(~0.9)) ~3
   2  is%(num-within-abs(1))        ~3
   2  is%(num-within-abs(1.1))      ~3
   2  is-not%(num-within-abs(0.9))  ~3
   5  is%(num-within-abs(4))         3

   num-within-abs(-0.1)(1, 1.05) raises "negative tolerance"
end

check "within":
   1  is%(within-abs(0.1))       1
   1  is%(within-abs(0.1))      ~1
  ~3  is%(within-abs(0.1))      ~3
  ~2  is-not%(within-abs(0.1))  ~3
  ~2  is%(within-abs(1.1))      ~3
  ~2  is%(within-abs(1))        ~3
  ~2  is%(within-abs(~1))       ~3
  ~2  is-not%(within-abs(~0.9)) ~3
   2  is%(within-abs(1))        ~3
   2  is%(within-abs(1.1))      ~3
   2  is-not%(within-abs(0.9))  ~3
   5  is%(within-abs(4))         3

   within-abs(-0.1)(1, 1.05) raises "negative tolerance"

   l1 = [list: 1]
   l2 = [list: 1.2]
   l1 is%(within-abs(0.5))  l2
   l1 is-not%(within-abs(0.1)) l2
   l1 is%(within(~0.5))  l2
   l1 is-not%(within-abs(~0.1)) l2

   l3 = [list: ~1]
   l4 = [list: 1.2]
   l3 is%(within-abs(0.5))  l4
   l3 is-not%(within-abs(0.1)) l4
   l3 is%(within-abs(~0.5))  l4
   l3 is-not%(within-abs(~0.1)) l4

   l5 = [list: 1]
   l6 = [list: ~1.2]
   l5 is%(within-abs(0.5))  l6
   l5 is-not%(within-abs(0.1)) l6
   l5 is%(within-abs(~0.5))  l6
   l5 is-not%(within-abs(~0.1)) l6

   l7 = [list: 1]
   l8 = [list: ~1.2]
   l7 is%(within-abs(0.5))  l8
   l7 is-not%(within-abs(0.1)) l8
   l7 is%(within-abs(~0.5))  l8
   l7 is-not%(within-abs(~0.1)) l8
end

check "within-rel":
   1  is%(within-rel(0.1))       1
   1  is%(within-rel(0.1))      ~1
  ~3  is%(within-rel(0.1))      ~3
  ~2  is-not%(within-rel(0.1))  ~3
  ~2  is%(within-rel(0.4))      ~3
  ~20  is-not%(within-rel(~0.3))   ~30
   2  is-not%(within-rel(0.1))    ~3
   #5  is%(within-rel(0.5))         3
   5  is-not%(within-rel(0.5))         3
   3  is%(within-rel(0.5))       5

   within-rel(-0.1)(1, 1.05) raises "tolerance"
   # rel tols > 1 are now acceptable
   #within-rel(1.1)(~2, ~3)   raises "relative tolerance"
   #within-rel(4)(5, 3)       raises "relative tolerance"
   within-rel(1.1)(~2, ~3)   is true
   within-rel(4)(5, 3)       is true

   l1 = [list: 1]
   l2 = [list: 1.2]
   l1 is%(within-rel(0.5))  l2
   l1 is-not%(within-rel(0.1)) l2
   l1 is%(within-rel(~0.5))  l2
   l1 is-not%(within-rel(~0.1)) l2

   l3 = [list: ~1]
   l4 = [list: 1.2]
   l3 is%(within-rel(0.5))  l4
   l3 is-not%(within-rel(0.1)) l4
   l3 is%(within-rel(~0.5))  l4
   l3 is-not%(within-rel(~0.1)) l4

   l5 = [list: 1]
   l6 = [list: ~1.2]
   l5 is%(within-rel(0.5))  l6
   l5 is-not%(within-rel(0.1)) l6
   l5 is%(within-rel(~0.5))  l6
   l5 is-not%(within-rel(~0.1)) l6

   l7 = [list: 1]
   l8 = [list: ~1.2]
   l7 is%(within-rel(0.5))  l8
   l7 is-not%(within-rel(0.1)) l8
   l7 is%(within-rel(~0.5))  l8
   l7 is-not%(within-rel(~0.1)) l8
end

data Box:
  | box(ref v)
end

check "within-abs-now":
  b1 = box(5)
  b2 = box(5)
  l1 = [list: 2, b1]
  l2 = [list: 2.1, b2]

  2 is%(within-abs(0.3)) 2.1
  l1 is%(within-abs-now(0.3)) l2
  l1 is%(within-rel-now(0.5)) l2
  l1 is-not%(within-abs(0.3)) l2
  l1 is-not%(within-rel(0.5)) l2

  b1!{v: 10}

  l1 is-not%(within-abs-now(0.3)) l2
  l1 is-not%(within-rel-now(0.5)) l2
end

check "num-within-rel":
  100000 is%(num-within-rel(0.1)) 95000
  100000 is-not%(num-within-rel(0.1)) 85000
  -100000 is%(num-within-rel(0.1)) -95000
  -100000 is-not%(num-within-rel(0.1)) -85000
  usr-num-within-rel = lam(rel-tol):
                     lam(a, b):
                       abs-tol = num-abs(((a + b) / 2) * rel-tol)
                       (num-within-abs(abs-tol))(a, b)
                     end
                    end
  -100000 is%(num-within-rel(0.1)) -95000
  -100000 is-not%(num-within-rel(0.1)) -85000
  100000 is%(usr-num-within-rel(0.1)) 95000
  100000 is-not%(usr-num-within-rel(0.1)) 85000
  -100000 is%(usr-num-within-rel(0.1)) -95000
  -100000 is-not%(usr-num-within-rel(0.1)) -85000
end

check "comparing very small roughnums":
  within-abs(~5e-324)(~12e-324, ~5e-324) raises "too small"
  within-abs(~5e-324)(~12e-300, ~5e-324) is false
  within-abs(~5e-312)(~12e-300, ~5e-324) is false
  within-abs(5e-324)(12e-324, 5e-324) is false
  within-abs(~10e-324)(~30e-324, ~10e-324) is false
  within-abs(~1e-322)(~2.5e-322, ~1e-322) is false
  within-abs(~1e-300)(~2.5e-300, ~1e-300) is false
  within-abs(~1e-10)(~2.5e-10, ~1e-10) is false
end

check "rational with huge num and den":
  within-abs(0.1)(0, 1623661768784682555628130151803732303823731397584604134434712085109537989167054680295758318409915953030552964736059575595461966294775666179502061096203869360118880960667793751146401768244701375970246097720878895899268070204857508281530675977229160007910464168127741485245775743781785616662411713763689231281064379513523432502547356005487844663970152189160201765407027985353074208688533763274501604996414630630394190081448539115299233288126813967753879087576171919/755514561303212495820119886905991823211308425045882281922060624339278043382100575558499380814515152419865252136969066086837161325241011943120713668028741350351111648910894342499713054600527610841990182908340717629849286713218055161453042659353860567734406665523150739074507776863711937202614139387402967614602827448114338965584371047818122254165209560756327433738273322173223495584757894740144146963057126420855755764727296465065007588010409322127232154083518081) is false
end
