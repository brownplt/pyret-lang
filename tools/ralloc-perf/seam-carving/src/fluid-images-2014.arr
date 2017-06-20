#lang pyret

provide *

import image as image
import file("fluid-images-support-2014.arr") as support
Image = support.Image
type Color = support.Color
color = support.color
image-from-url = support.image-from-url
image-to-2d-color-list = support.image-to-2d-color-list
image-from-2d-color-list = support.image-from-2d-color-list


TRAFALGAR-SQUARE = image-from-url("http://cs.brown.edu/courses/cs019/2012/images/seam_carving/trafalgar-square.jpg")
ULURU = image-from-url("http://cs.brown.edu/courses/cs019/2012/images/seam_carving/uluru.jpg")
#CAR = image-from-url("http://cs.brown.edu/courses/cs019/2012/images/seam_carving/car.jpg")

# weight: the total energy of all the pixels in the seam
# positions: the x offsets of the seam at each column of an image
data Seam:
  | seam(weight :: Number, positions :: List)
end

data Triple:
  | triple(a :: Any, b :: Any, c :: Any)
end

fun energy(a :: Number, b :: Number, c :: Number,
           d :: Number, e :: Number, f :: Number,
           g :: Number, h :: Number, i :: Number) -> Number:
  doc: "input:  the brightnesses of a 3x3 grid of pixels output: the energy of the center pixel"
  xenergy = (a + ((2 * d) + g)) - (c + ((2 * f) + i))
  yenergy = (a + ((2 * b) + c)) - (g + ((2 * h) + i))
  num-sqrt((xenergy * xenergy) + (yenergy * yenergy))
where:
  energy(0, 0, 0,
         0, 0, 0,
         0, 0, 0) is 0
  energy(0, 1, 0,
         0, 7, 0,
         0, 0, 0) is 2
  energy(1, 2, 5,
         1, 9, 4,
         0, 2, 5) is num-sqrt(226)
end

fun brightness(c) -> Number:
  doc: "output: color's brightness"
  c.red + c.green + c.blue
where:
  brightness(color(0, 0, 0)) is 0
  brightness(color(0, 100, 0)) is 100
  brightness(color(255, 255, 255)) is 765
end

fun map-triplets(proc :: (Any, Any, Any -> Any),
                 lst :: List,
                 edge :: Any) -> List:
   doc: "output: the output of proc from every consecutive triplet of lst with edge appended to both ends"
  cases(List) lst:
    | empty => empty
    | link(_, rest) => map3(proc,
                            link(edge, lst.take(lst.length() - 1)),
                            lst,
        rest.append([list: edge]))
  end
where:
  map-triplets(lam(a, b, c): a + (b + c) end, empty, 0) is empty
  map-triplets(lam(a, b, c): a - (b - c) end, [list: 1], 4) is [list: 7]
  map-triplets(lam(a, b, c): a + (b + c) end, [list: 0, 2, 4, 5, 2, 4], 5)
    is [list: 7, 6, 11, 11, 11, 11]
end

fun drop-at(lst :: List, pos :: Number):
  doc: "input:  a non-empty list and a position in the list. output: the list with the element at pos dropped"
  if pos == 0: lst.rest else: link(lst.first, drop-at(lst.rest, pos - 1)) end
where:
  # the awesome cannonball operator seems to be removed... :(
  # [list: 1]^drop-at(0) is empty
  # [list: 1, 2]^drop-at(0) is [list: 2]
  # [list: "a", "b", "c", "d"]^drop-at(2) is [list: "a", "b", "d"]
  drop-at([list: 1], 0) is empty
  drop-at([list: 1, 2], 0) is [list: 2]
  drop-at([list: "a", "b", "c", "d"], 2) is [list: "a", "b", "d"]

end

fun img-energy(img :: List) -> List:
  doc: "input:  a brightness matrix for an image. output: the energy matrix of the image"
  map-triplets(lam(prev-row, curr-row, next-row):
                 map-triplets(lam(prev, curr, next):
                                energy(prev.a, curr.a, next.a,
                                       prev.b, curr.b, next.b,
                                       prev.c, curr.c, next.c)
                              end,
                              map3(triple, prev-row, curr-row, next-row),
                              triple(0, 0, 0))
               end,
               img,
               repeat(img.first.length(), 0))
where:
  img-energy([list: [list: 10]]) is [list: [list: 0]]
  img-energy([list: [list: 10, 10]]) is [list: [list: 20, 20]]
end

fun best-seam(seams :: List) -> Seam:
  doc: "input:  a non-empty list of seams. output: the seam with the lowest weight"
  for fold(best from seams.first, elem from seams.rest):
    if elem.weight < best.weight: elem
    else: best
    end
  end
where:
  best-seam([list: seam(0, empty)]) is seam(0, [list: ])
  best-seam([list: seam(2, [list: 0]), seam(1, [list: 1])]) is seam(1, [list: 1])
  best-seam([list: seam(1, [list: 0]), seam(1, [list: 1])]) is seam(1, [list: 0])
end

fun best-of-3(left :: Option, center :: Seam, right :: Option) -> Seam:
  doc: "input:  a triplet of seams, of which the edges are optional. output: the leftmost lowest weight seam"
  fun to-list(ms):
    cases(Option) ms: | some(s) => [list:s] | none => empty end
  end
  
  best-seam(to-list(left).append(link(center, to-list(right))))
where:
  best-of-3(none, seam(3, [list: 1]), none) is seam(3, [list: 1])
  best-of-3(none, seam(3, [list: 1]), some(seam(3, [list: 5]))) is seam(3, [list: 1])
  best-of-3(some(seam(0, [list: 0])), seam(0, [list: 1]), some(seam(0, [list: 2])))
    is seam(0, [list: 0])
  best-of-3(some(seam(4, [list: 0])), seam(3, [list: 1]), some(seam(2, [list: 2])))
    is seam(2, [list: 2])
end

fun energy-seams(img :: List) -> List:
  doc: "input:  an energy matrix for an image. output: the lowest weight seams starting at each of the top row pixels"
  width = img.first.length()
  img.foldr(lam(row, seams):
              map3(lam(e, s, pos):
                     seam(e + s.weight, link(pos, s.positions))
                   end,
                   row,
                   map-triplets(lam(l, c, r): best-of-3(l, c.value, r) end,
                                seams.map(some),
                                none),
                   range(0, width))
            end,
    repeat(width, seam(0, empty)))
where:
  energy-seams([list: [list: 0, 4, 2, 6],
      [list: 2, 5, 8, 2],
      [list: 2, 4, 6, 9]])
    is [list: seam(4, [list: 0, 0, 0]),
    seam(8, [list: 1, 0, 0]),
    seam(9, [list: 2, 1, 0]),
    seam(14, [list: 3, 3, 2])]
end

fun remove-seam(img :: List, s :: Seam) -> List:
  doc: "input:  an image matrix and a seam in that image. output: the matrix with the seam removed"
  map2(drop-at, img, s.positions)
where:
  remove-seam([list: [list: 3, 5, 8, 8],
      [list: 2, 5, 8, 3],
      [list: 3, 5, 7, 2]],
    seam(5, [list: 0, 0, 0]))
    is [list: [list: 5, 8, 8],
    [list: 5, 8, 3],
    [list: 5, 7, 2]]
  remove-seam([list: [list: 3, 5, 8, 8],
      [list: 2, 5, 8, 3],
      [list: 3, 5, 7, 2]],
    seam(5, [list: 1, 1, 2]))
    is [list: [list: 3, 8, 8],
    [list: 2, 8, 3],
    [list: 3, 5, 2]]
end

fun remove-seams(colors :: List, brightnesses :: List, n :: Number) -> List:
  doc: "input:  brightness and color matrices for an image, and a natural number less than or equal to the width of the image. output: the color matrix with n seams carved"
  if n == 0: colors
  else:
    min-seam = best-seam(energy-seams(img-energy(brightnesses)))
    remove-seams(remove-seam(colors, min-seam),
                 remove-seam(brightnesses, min-seam),
                 n - 1)
  end
end

fun seam-carve(img, n :: Number):
  doc: "input:  an image and a natural number less than or equal to the width of an image. output: the image with n seams carved"
  if n > img.width:
    raise("can't carve more seams than width of image")
  else:
    colors = image-to-2d-color-list(img)
    brightnesses = for map(row from colors): row.map(brightness) end
    seams-removed = remove-seams(colors, brightnesses, n)
    image-from-2d-color-list(seams-removed)
  end
#where:
  #CAR^seam-carve(50000) raises "width"
  #(CAR <> CAR^seam-carve(1)) is true
  #(CAR^seam-carve(2) == CAR^seam-carve(1)^seam-carve(1)) is true
end

#bnglr-dncr = image-from-url("http://cs.brown.edu/courses/cs019/2012/images/seam_carving/bangalore-dancers.jpg")
#gnlr-list = image-to-2d-color-list(bnglr-dncr)
#bnglr-dncr.img
#CAR.img
#seam-carve(CAR, 2).img

#image.save-file(CAR.img, "test")
