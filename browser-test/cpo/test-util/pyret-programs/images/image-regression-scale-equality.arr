include image-typed
include image-structs

check "regression":
  # Regression test for a bad merge that made scale-image comparison fail
  scale(0.2, circle(50, mode-solid, red)) is-not
    scale(0.3, circle(50, mode-solid, red))

  scale(0.2, circle(50, mode-solid, red)) is-not
    scale(0.3, circle(50, mode-solid, red))
end
