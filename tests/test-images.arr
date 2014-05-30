import image as I
import image-structs as IS

check:
  fun negate(f): lam(x): not(f(x)) end end

  "red" satisfies I.is-image-color
  "blue" satisfies I.is-image-color

  IS.color(5, 4, 3, 2) satisfies I.is-image-color
  "not-blue" satisfies negate(I.is-image-color)
  
  "solid" satisfies I.is-mode
  "outline" satisfies I.is-mode
  45 satisfies I.is-mode
  -1 satisfies negate(I.is-mode)

  "left" satisfies I.is-x-place
  "right" satisfies I.is-x-place
  "center" satisfies I.is-x-place
  "middle" satisfies I.is-x-place
  "not-a-place" satisfies negate(I.is-x-place)

  "top" satisfies I.is-y-place
  "bottom" satisfies I.is-y-place
  "baseline" satisfies I.is-y-place
  "center" satisfies I.is-y-place
  "middle" satisfies I.is-y-place
  "not-a-place" satisfies negate(I.is-y-place)

  0 satisfies I.is-angle
  359.9999 satisfies I.is-angle
  360 satisfies negate(I.is-angle)
  45.5 satisfies I.is-angle
  3 / 7 satisfies I.is-angle
  -1 satisfies negate(I.is-angle)
  360.1 satisfies negate(I.is-angle)

  -0.5 satisfies negate(I.is-side-count)
  0 satisfies negate(I.is-side-count)
  1 satisfies negate(I.is-side-count)
  2 satisfies negate(I.is-side-count)
  3 satisfies I.is-side-count
  5 satisfies I.is-side-count
  6 satisfies I.is-side-count
  6.5 satisfies negate(I.is-side-count)

  
  -0.5 satisfies negate(I.is-step-count)
  0 satisfies negate(I.is-step-count)
  1 satisfies I.is-step-count
  2 satisfies I.is-step-count
  3 satisfies I.is-step-count
  2.3 satisfies negate(I.is-step-count)


  fun make-pyret():
    I.bitmap-url("https://raw.githubusercontent.com/brownplt/pyret-lang/master/img/pyret-logo.png")
  end
  pyret = make-pyret()

  pyret satisfies I.is-image

  I.image-equals(I.circle(100, "solid", "red"), I.circle(100, "solid", "red")) is true
  I.image-equals(pyret, make-pyret()) is true

  I.text("my string", 12, "blue") satisfies I.is-image
  I.text-font("a string", 12, "green", "Lucida", "roman", "normal", "bold", false) satisfies I.is-image

  circle = I.circle(50, "solid", "red")
  triangle = I.triangle(100, "outline", "blue")
  square = I.square(80, "solid", "green")
  rectangle = I.rectangle(40, 90, "outline", "purple")
  
  circle satisfies I.is-image
  triangle satisfies I.is-image
  I.overlay(circle, triangle) satisfies I.is-image
  I.overlay-xy(circle, 4.5, -20, triangle) satisfies I.is-image
  I.overlay-align("left", "top", circle, triangle) satisfies I.is-image
  I.underlay(circle, triangle) satisfies I.is-image
  I.underlay-xy(circle, 4.5, -20, triangle) satisfies I.is-image
  I.underlay-align("left", "top", circle, triangle) satisfies I.is-image
  I.beside(circle, triangle) satisfies I.is-image
  I.beside-align("top", circle, triangle) satisfies I.is-image
  I.above(circle, triangle) satisfies I.is-image
  I.above-align("right", circle, triangle) satisfies I.is-image

  I.empty-scene(60, 80) satisfies I.is-image
  I.put-image(circle, 40, -20, I.empty-scene(40, 40)) satisfies I.is-image
  I.place-image(circle, 40, -20, I.empty-scene(20, 40)) satisfies I.is-image
  I.place-image-align(circle, 40, -20, "center", "baseline", I.empty-scene(10, 60)) satisfies I.is-image

  I.rotate(30, triangle) satisfies I.is-image
  I.scale(30, triangle) satisfies I.is-image
  I.scale-xy(30, 40, triangle) satisfies I.is-image
  I.flip-horizontal(triangle) satisfies I.is-image
  I.flip-vertical(triangle) satisfies I.is-image
  I.frame(triangle) satisfies I.is-image

  I.crop(20, 20, 40, 40, triangle) satisfies I.is-image

  I.line(20, 30, "red") satisfies I.is-image
  I.add-line(circle, -40, -30, 20, 10, "blue") satisfies I.is-image
  I.scene-line(circle, -40, -30, 20, 10, "blue") satisfies I.is-image

  square satisfies I.is-image
  rectangle satisfies I.is-image

  I.regular-polygon(30, 6, "solid", "yellow") satisfies I.is-image
  I.ellipse(30, 40, "solid", "orange") satisfies I.is-image

  I.triangle-sas(30, 40, 50, "solid", "black") satisfies I.is-image
  I.triangle-sss(30, 40, 50, "solid", "black") satisfies I.is-image
  I.triangle-ass(30, 40, 50, "solid", "black") satisfies I.is-image
  I.triangle-ssa(30, 40, 50, "solid", "black") satisfies I.is-image
  I.triangle-aas(30, 40, 50, "solid", "black") satisfies I.is-image
  I.triangle-asa(30, 40, 50, "solid", "black") satisfies I.is-image
  I.triangle-saa(30, 40, 50, "solid", "black") satisfies I.is-image

  I.right-triangle(3, 4, "solid", "red") satisfies I.is-image
  I.isosceles-triangle(5, 30, "solid", "red") satisfies I.is-image

  I.star(20, "solid", "red") satisfies I.is-image
  I.star-sized(7, 50, 20, "solid", "green") satisfies I.is-image
  I.radial-star(9, 40, 10, "solid", "green") satisfies I.is-image
  I.star-polygon(30, 9, 4, "solid", "blue") satisfies I.is-image
  I.rhombus(20, 72, "solid", "yellow") satisfies I.is-image

  # NOTE: These fail with DOMExceptions because of same-origin-policy issues
  #I.image-to-color-list(pyret) satisfies is-link
  #I.color-list-to-image(I.image-to-color-list(pyret), I.image-width(pyret), I.image-height(pyret), 0, 0) satisfies I.is-image
  #I.color-list-to-bitmap(I.image-to-color-list(pyret), I.image-width(pyret), I.image-height(pyret)) satisfies I.is-image
  #I.image-baseline(pyret) is 488

  I.name-to-color("red") satisfies I.is-image-color
  
end
