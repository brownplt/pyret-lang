import shared-gdrive("HelloWorld.arr", "10NfEXFUNWvU-yOv-hhx3Qbj5G4STmoZK") as H

# HelloWorld.arr contains a function `f`, which multiplies an input number by 2.
# Source: https://code.pyret.org/editor#program=10NfEXFUNWvU-yOv-hhx3Qbj5G4STmoZK

# I'm planning on moving this to the regular tests folder, but I left it here for now.

check "Testing f":

  H.f(0) is 0

  H.f(1) is 2

  H.f(-1) is -2

  H.f(5) is 10

  H.f(2.5) is 5

  H.f(0.5) is 1

  rand1 = num-random(10000000)
  H.f(rand1) is (rand1 * 2)

  input = range(0, 100).map(lam(x): num-random(1000) end)

  input.map(H.f) is input.map(lam(x): x * 2 end)

end