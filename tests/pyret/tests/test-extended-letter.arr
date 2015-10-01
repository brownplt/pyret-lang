check "that-identifiers-with-common-unicode-letters-work":
  kälte-in-F = -40
  kälte-in-C = (kälte-in-F - 32) * 5/9
  kälte-in-C is -40

  R-in-Ω = 3
  R-in-μΩ = R-in-Ω * 1e6
  R-in-μΩ is 3e6

  π = 4 * num-atan(1)
  ν = 100e6
  ω = 2 * π * ν
  ω is%(within(0.1e6)) 628e6

  c = 299792458
  λ = c / ν
  λ is%(within(0.01)) 3
end
