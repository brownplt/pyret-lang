
check:
  greentriangle :: Number -> Image
  fun greentriangle(n):
    triangle(n, "solid", "green")
  examples:
    greentriangle(50) is triangle(50, "solid", "green")
  end

  val x = 22
  x is 22
end

check:
  examples:
    5 is 5
  end
end
