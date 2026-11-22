import image-typed as IT
import image as UT
include image

w :: IT.Image = UT.scale(2, IT.circle(30, IT.mode-solid, IT.red))
x :: UT.Image = UT.scale(2, IT.circle(30, IT.mode-solid, IT.yellow))
y :: IT.Image = IT.scale(2, UT.circle(30, "solid", "green"))
z :: UT.Image = IT.scale(2, UT.circle(30, "solid", "blue"))

fun i(i1 :: Image, i2 :: Image, i3 :: Image, i4 :: Image):
  "success"
end

check:
  i(w, x, y, z) is "success"
  i(1, 2, 3, 4) raises "Image"
end

