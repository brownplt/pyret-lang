import image-structs as IS

examples:
  fun negate(f): lam(x): not(f(x)) end end

  "red" satisfies is-image-color
  "blue" satisfies is-image-color

  IS.color(5, 4, 3, 2) satisfies is-image-color
  "not-blue" satisfies negate(is-image-color)
  
  "solid" satisfies is-mode
  "outline" satisfies is-mode
  45 satisfies is-mode
  -1 satisfies negate(is-mode)

  "left" satisfies is-x-place
  "right" satisfies is-x-place
  "center" satisfies is-x-place
  "middle" satisfies is-x-place
  "not-a-place" satisfies negate(is-x-place)

  "top" satisfies is-y-place
  "bottom" satisfies is-y-place
  "baseline" satisfies is-y-place
  "center" satisfies is-y-place
  "middle" satisfies is-y-place
  "not-a-place" satisfies negate(is-y-place)

  0 satisfies is-angle
  359.9999 satisfies is-angle
  360 satisfies negate(is-angle)
  45.5 satisfies is-angle
  3 / 7 satisfies is-angle
  -1 satisfies negate(is-angle)
  360.1 satisfies negate(is-angle)

  -0.5 satisfies negate(is-side-count)
  0 satisfies negate(is-side-count)
  1 satisfies negate(is-side-count)
  2 satisfies negate(is-side-count)
  3 satisfies is-side-count
  5 satisfies is-side-count
  6 satisfies is-side-count
  6.5 satisfies negate(is-side-count)

  
  -0.5 satisfies negate(is-step-count)
  0 satisfies negate(is-step-count)
  1 satisfies is-step-count
  2 satisfies is-step-count
  3 satisfies is-step-count
  2.3 satisfies negate(is-step-count)


  fun make-pyret():
    bitmap-url("https://raw.githubusercontent.com/brownplt/pyret-lang/master/img/pyret-logo.png")
  end
  pyret = make-pyret()

  pyret satisfies is-image

  images-equal(circle(100, "solid", "red"), circle(100, "solid", "red")) is true
  images-equal(pyret, make-pyret()) is true

  text("my string", 12, "blue") satisfies is-image
  text-font("a string", 12, "green", "Lucida", "roman", "normal", "bold", false) satisfies is-image
  
  text(5, 12, "blue") raises "String"
  text("my string", "nan", "blue") raises "Positive Integer"
  text(5, 12, 42) raises "String"
  
  f = lam(v): 5 end
  
  circle(f, "solid", "red") raises "Number"
  circle(50, 50, "red") satisfies is-image
  circle(50, -2, "red") raises "Mode"
  circle(50, "solid", f) raises "Color"
  
  c = circle(50, "solid", "red")
  t = triangle(100, "outline", "blue")
  s = square(80, "solid", "green")
  r = rectangle(40, 90, "outline", "purple")
  
  c satisfies is-image
  t satisfies is-image
  overlay(c, t) satisfies is-image
  overlay(c, 5) raises "Image"
  overlay(5, c) raises "Image"
  
  overlay-xy(c, 4.5, -20, t) satisfies is-image
  overlay-xy(c, c, c, c) raises "Number"
  overlay-xy("a", 4.5, -20, t) raises "Image"
  
  overlay-align("left", "top", c, t) satisfies is-image
  underlay(c, t) satisfies is-image
  underlay-xy(c, 4.5, -20, t) satisfies is-image
  underlay-align("left", "top", c, t) satisfies is-image
  beside(c, t) satisfies is-image
  beside-align("top", c, t) satisfies is-image
  above(c, t) satisfies is-image
  above-align("right", c, t) satisfies is-image

  empty-scene(60, 80) satisfies is-image
  put-image(c, 40, -20, empty-scene(40, 40)) satisfies is-image
  place-image(c, 40, -20, empty-scene(20, 40)) satisfies is-image
  place-image-align(c, 40, -20, "center", "baseline", empty-scene(10, 60)) satisfies is-image

  rotate(30, t) satisfies is-image
  scale(30, t) satisfies is-image
  scale-xy(30, 40, t) satisfies is-image
  flip-horizontal(t) satisfies is-image
  flip-vertical(t) satisfies is-image
  frame(t) satisfies is-image

  crop(20, 20, 40, 40, t) satisfies is-image

  line(20, 30, "red") satisfies is-image
  add-line(c, -40, -30, 20, 10, "blue") satisfies is-image
  scene-line(c, -40, -30, 20, 10, "blue") satisfies is-image

  s satisfies is-image
  r satisfies is-image

  regular-polygon(30, 6, "solid", "yellow") satisfies is-image
  ellipse(30, 40, "solid", "orange") satisfies is-image

  triangle-sas(30, 40, 50, "solid", "black") satisfies is-image
  triangle-sss(30, 40, 50, "solid", "black") satisfies is-image
  triangle-ass(30, 40, 50, "solid", "black") satisfies is-image
  triangle-ssa(30, 40, 50, "solid", "black") satisfies is-image
  triangle-aas(30, 40, 50, "solid", "black") satisfies is-image
  triangle-asa(30, 40, 50, "solid", "black") satisfies is-image
  triangle-saa(30, 40, 50, "solid", "black") satisfies is-image

  right-triangle(3, 4, "solid", "red") satisfies is-image
  isosceles-triangle(5, 30, "solid", "red") satisfies is-image

  star(20, "solid", "red") satisfies is-image
  star-sized(7, 50, 20, "solid", "green") satisfies is-image
  radial-star(9, 40, 10, "solid", "green") satisfies is-image
  star-polygon(30, 9, 4, "solid", "blue") satisfies is-image
  rhombus(20, 72, "solid", "yellow") satisfies is-image

  # NOTE: These fail with DOMExceptions because of same-origin-policy issues
  #image-to-color-list(pyret) satisfies is-link
  #color-list-to-image(image-to-color-list(pyret), image-width(pyret), image-height(pyret), 0, 0) satisfies is-image
  #color-list-to-bitmap(image-to-color-list(pyret), image-width(pyret), image-height(pyret)) satisfies is-image
  #image-baseline(pyret) is 488

  name-to-color("red") satisfies is-image-color
  
end
