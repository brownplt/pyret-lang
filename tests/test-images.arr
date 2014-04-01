import image as I
import image-structs as IS

check:
  fun negate(f): f(_) end

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
  360 satisfies I.is-angle
  45.5 satisfies I.is-angle
  3 / 7 satisfies I.is-angle
  -1 satisfies negate(I.is-angle)
  360.1 satisfies negate(I.is-angle)

  -0.5 satisfies negate(I.is-side-count)
  0 satisfies I.is-side-count
  1 satisfies I.is-side-count
  2 satisfies I.is-side-count
  3 satisfies I.is-side-count
  2.3 satisfies negate(I.is-side-count)


  fun make-pyret():
    I.bitmap-url("https://raw.githubusercontent.com/brownplt/pyret-lang/master/img/pyret-logo.png")
  end
  pyret = make-pyret()

  pyret satisfies I.is-image

  I.image-equals(I.circle(100, "solid", "red"), I.circle(100, "solid", "red")) is true
  I.image-equals(pyret, make-pyret()) is true

  I.text("my string", 12, "blue") satisfies I.is-image

end

