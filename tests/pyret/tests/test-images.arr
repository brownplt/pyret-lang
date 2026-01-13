include image-typed
include image-structs
include either

fun within-n-badness(n):
  lam(img1, img2):
    diff = images-difference(img1, img2)
    cases(Either) diff:
      | left(err) => false
      | right(shadow diff) => diff < n
    end
  end
end


check "Image-url":
  # prior to fixing #1831, this would fail
  image-url("https://bootstrapworld.org/images/icon.png") satisfies is-image
end

check "Overlay equality":
  fun mk-image():
    overlay(circle(100, mode-solid, red), circle(50, mode-solid, red))
  end
  mk-image() is mk-image()
  mk-image() is underlay(circle(50, mode-solid, red), circle(100, mode-solid, red))
end

check "Composing lists of images":
  red-circ = circle(10, mode-solid, red)
  yellow-circ = circle(20, mode-solid, yellow)
  green-circ = circle(30, mode-solid, green)

  overlay-list([list: red-circ, yellow-circ, green-circ])
    is overlay(red-circ, overlay(yellow-circ, green-circ))
  overlay-align-list(x-left, y-bottom, [list: red-circ, yellow-circ, green-circ])
    is overlay-align(x-left, y-bottom, red-circ, overlay-align(x-left, y-bottom, yellow-circ, green-circ))
  underlay-list([list: red-circ, yellow-circ, green-circ])
    is underlay(red-circ, underlay(yellow-circ, green-circ))
  underlay-align-list(x-left, y-bottom, [list: red-circ, yellow-circ, green-circ])
    is underlay-align(x-left, y-bottom, red-circ, underlay-align(x-left, y-bottom, yellow-circ, green-circ))
  above-list([list: red-circ, yellow-circ, green-circ])
    is above(red-circ, above(yellow-circ, green-circ))
  above-align-list(x-left, [list: red-circ, yellow-circ, green-circ])
    is above-align(x-left, red-circ, above-align(x-left, yellow-circ, green-circ))
  # NOTE(joe): @blerner and I both tested this, and it's fine on Firefox but on
  # Chrome has a badness of about 0.008, which is totally acceptable
  below-list([list: red-circ, yellow-circ, green-circ])
    is%(within-n-badness(1)) flip-vertical(above-list([list: red-circ, yellow-circ, green-circ]))
  below-align-list(x-left, [list: red-circ, yellow-circ, green-circ])
    is flip-vertical(above-align(x-left, red-circ, above-align(x-left, yellow-circ, green-circ)))
  beside-list([list: red-circ, yellow-circ, green-circ])
    is beside(red-circ, beside(yellow-circ, green-circ))
  beside-align-list(y-top, [list: red-circ, yellow-circ, green-circ])
    is beside-align(y-top, red-circ, beside-align(y-top, yellow-circ, green-circ))
end

check "Polygons":
  triangle(4, mode-solid, red) satisfies is-image
  triangle(~4, mode-solid, red) satisfies is-image
  triangle(4, mode-solid, red, true) raises ""
  triangle(blue, mode-solid, red, true) raises ""

  right-triangle(45, 52, mode-solid, black) satisfies is-image
  right-triangle(~45, ~52, mode-solid, black) satisfies is-image
  right-triangle(blue, 44, mode-solid, black) raises ""
  right-triangle(4, 44, mode-solid, black, true) raises ""

  isosceles-triangle(4, 56, mode-solid, black) satisfies is-image
  isosceles-triangle(~4, ~56, mode-solid, black) satisfies is-image
  isosceles-triangle(red, 56, mode-solid, black) raises ""
  isosceles-triangle(4, 56, mode-solid, black, true) raises ""

  fun test-triangle-fun(f) block:
    f(4, 5, 6, mode-outline, black) satisfies is-image
    f(~4, ~5, ~6, mode-outline, black) satisfies is-image
    f(1, 2, 3, "", true) raises ""
    f(1, 2, 3, mode-outline, red, true) raises ""
  end
  triangles = [list:
    triangle-sss,
    triangle-ass,
    triangle-sas,
    triangle-ssa,
    triangle-aas,
    triangle-saa
  ]
  each(test-triangle-fun, triangles)

  square(44, mode-solid, red) satisfies is-image
  square(~44, mode-solid, red) satisfies is-image
  square(blue, mode-solid, green) raises ""
  square(44, mode-solid, green, true) raises ""

  rectangle(30, 50, mode-solid, blue) satisfies is-image
  rectangle(~30, ~50, mode-solid, blue) satisfies is-image
  rectangle(blue, 50, mode-solid, blue) raises ""
  rectangle(30, 50, mode-solid, blue, true) raises ""

  rhombus(40, 45, mode-solid, black) satisfies is-image
  rhombus(~40, ~45, mode-solid, black) satisfies is-image
  rhombus(blue, 45, mode-solid, black) raises ""
  rhombus(40, 45, mode-solid, black, true) raises ""

  star(33, mode-solid, blue) satisfies is-image
  star(~33, mode-solid, blue) satisfies is-image
  star({}, mode-solid, blue) raises ""
  star(33, mode-solid, blue, true) raises ""

  radial-star(33, 10, 50, mode-solid, blue) satisfies is-image
  radial-star(33, ~10, ~50, mode-solid, blue) satisfies is-image
  radial-star(blue, 10, 50, mode-solid, blue) raises ""
  radial-star(33, 10, 50, mode-solid, blue, true) raises ""
  radial-star(1, 10, 50, mode-solid, blue) raises ""

  star-sized(33, 10, 50, mode-solid, blue) satisfies is-image
  star-sized(33, ~10, ~50, mode-solid, blue) satisfies is-image
  star-sized(blue, 10, 50, mode-solid, blue) raises ""
  star-sized(33, 10, 50, mode-solid, blue, true) raises ""
  star-sized(1, 10, 50, mode-solid, blue) raises ""

  star-polygon(43, 3, 5, mode-solid, blue) satisfies is-image
  star-polygon(~43, 3, 5, mode-solid, blue) satisfies is-image
  star-polygon(blue, 3, 5, mode-solid, blue) raises ""
  star-polygon(43, 3, 5, mode-solid, blue, {}) raises ""
  star-polygon(43, 2, 5, mode-solid, blue) raises ""
  star-polygon(43, 3, 0, mode-solid, blue) raises ""

  regular-polygon(45, 10, mode-solid, blue) satisfies is-image
  regular-polygon(45, 10, mode-solid, blue) satisfies is-image
  regular-polygon(true, 10, mode-solid, blue) raises ""
  regular-polygon(45, 10, mode-solid, blue, 4) raises ""
  regular-polygon(45, 2, mode-solid, blue) raises ""

  point-polygon([list: point(~0, ~0), point(~0, 15/6), point(15/6, ~15/6), point(~15/6, 0)], mode-solid, red) is square(15/6, mode-solid, red)
  point-polygon([list: point(~0, ~0), point(~0, -15/6), point(-15/6, ~-15/6), point(~-15/6, 0)], mode-solid, red) is square(15/6, mode-solid, red)



  empty-scene(20, 50) satisfies is-image
end

check "color-lists":
  color-list-to-image([list: red, green, blue], 2, 2, 1, 1) raises ""
  color-list-to-image([list: red, green, blue, black], 2, 2, 1, 1) satisfies is-image

  color-list-to-bitmap([list: red, green, blue], 2, 2) raises ""
  color-list-to-bitmap([list: red, green, blue, black], 2, 2) satisfies is-image

  color-list-to-image([list: ], 0, 0, 0, 0) does-not-raise
end

check "trimming":
  sqr = square(40, mode-solid, red)
  image-width(sqr) is 40
  trim-image(sqr) satisfies is-image
  image-width(trim-image(sqr)) is 40
  image-height(sqr) is 40
  image-height(trim-image(sqr)) is 40

  blank = rectangle(40, 20, mode-solid, transparent)
  image-width(blank) is 40
  trim-image(blank) satisfies is-image
  image-width(trim-image(blank)) is 0
  image-height(blank) is 20
  image-height(trim-image(blank)) is 0

  trim-image(trim-image(blank)) satisfies is-image

end

check "properties":
  
  image-width(ellipse(30, 40, mode-solid, orange)) is 30
  image-height(ellipse(30, 40, mode-solid, orange)) is 40


  even-overlay = overlay(circle(20, mode-solid, orange), circle(20, mode-solid, purple))
  image-height(even-overlay) is 40
  image-width(even-overlay) is 40
  
  indigo-Good = text-font("Goodbye", 48, indigo, "Helvetica", ff-modern, fs-normal, fw-normal, false)
  image-height(indigo-Good) is%(within-abs(5)) 45
  image-baseline(indigo-Good) is%(within-abs(2)) 35

  indigo-good = text-font("goodbye", 48, indigo, "Helvetica", ff-modern, fs-normal, fw-normal, false)
  image-height(indigo-good) is%(within-abs(5)) 41
  image-baseline(indigo-good) is%(within-abs(2)) 33

  indigo-mm = text-font("mm", 48, indigo, "Helvetica", ff-modern, fs-normal, fw-normal, false)
  image-height(indigo-mm) is%(within-abs(5)) 25
  image-baseline(indigo-mm) is%(within-abs(2)) 25

  image-baseline(rectangle(100, 100, mode-solid, black)) is 100

  image-height(rectangle(100, 100, mode-solid, black)) is 100

  # Regression: changed to allow truly-empty images
  image-width(rectangle(0, 100, mode-solid, black)) is 0

  image-height(rectangle(100, 0, mode-solid, black)) is 0
end

check "predicates":
  mode-solid satisfies is-FillMode
  mode-outline satisfies is-FillMode
  "checkered" violates is-FillMode

  0 violates is-step-count
  1 satisfies is-step-count

  2 violates is-side-count
  3 satisfies is-side-count

  -290 violates is-angle
  290 satisfies is-angle
  0 satisfies is-angle
  360 violates is-angle
  359 satisfies is-angle

  "up-top" violates is-XPlace
  x-left satisfies is-XPlace
  x-right satisfies is-XPlace
  x-middle satisfies is-XPlace

  "centered" violates is-YPlace
  y-top satisfies is-YPlace
  y-bottom satisfies is-YPlace
  y-center satisfies is-YPlace
  y-baseline satisfies is-YPlace

  pink satisfies is-Color
  "puke" violates is-Color

end


check "Crop":
  cropsquare = crop(0, 0, 100, 100, rectangle(400, 100, mode-solid, red))
  sq = rectangle(100, 100, mode-solid, red)
  cropsquare is sq
end

logo = image-file("img/pyret-logo.png")
check:
  image-width(logo) is 501
  image-height(logo) is 488
  color-at-position(logo, 250, 280) is color(238, 30, 16, 1)
end

save-image(logo, "pyret-logo-copy.png")
logo2 = image-file("pyret-logo-copy.png")
check:
  logo is logo2
end
