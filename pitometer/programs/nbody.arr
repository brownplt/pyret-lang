pi = 3.14159265358979323
SOLAR_MASS = 4 * pi * pi
DAYS_PER_YEAR = 365.24

data Body:
  | planet(ref x, ref y, ref z, ref vx, ref vy, ref vz, ref mass) with:
    method move(self, dt):
      self!{
        x: self!x + (dt * self!vx),
        y: self!y + (dt * self!vy),
        z: self!z + (dt * self!vz),
      }
    end,
    method advance(self, dx, dy, dz, delta):
      self!{
        vx: self!vx + (dx * delta),
        vy: self!vy + (dy * delta),
        vz: self!vz + (dz * delta)
      }
    end,
    method speedSq(self):
      (self!vx * self!vx) + (self!vy * self!vy) + (self!vz * self!vz)
    end
end

fun initial-values():
  sun = planet(0,0,0,0,0,0,SOLAR_MASS)
  jupiter = planet(
    4.84143144246472090e+00,
    -1.16032004402742839e+00,
    -1.03622044471123109e-01,
    1.66007664274403694e-03 * DAYS_PER_YEAR,
    7.69901118419740425e-03 * DAYS_PER_YEAR,
    -6.90460016972063023e-05 * DAYS_PER_YEAR,
    9.54791938424326609e-04 * SOLAR_MASS)
  saturn = planet(
    8.34336671824457987e+00,
    4.12479856412430479e+00,
    -4.03523417114321381e-01,
    -2.76742510726862411e-03 * DAYS_PER_YEAR,
    4.99852801234917238e-03 * DAYS_PER_YEAR,
    2.30417297573763929e-05 * DAYS_PER_YEAR,
    2.85885980666130812e-04 * SOLAR_MASS)
  uranus = planet(
    1.28943695621391310e+01,
    -1.51111514016986312e+01,
    -2.23307578892655734e-01,
    2.96460137564761618e-03 * DAYS_PER_YEAR,
    2.37847173959480950e-03 * DAYS_PER_YEAR,
    -2.96589568540237556e-05 * DAYS_PER_YEAR,
    4.36624404335156298e-05 * SOLAR_MASS)
  neptune = planet(
    1.53796971148509165e+01,
    -2.59193146099879641e+01,
    1.79258772950371181e-01,
    2.68067772490389322e-03 * DAYS_PER_YEAR,
    1.62824170038242295e-03 * DAYS_PER_YEAR,
    -9.51592254519715870e-05 * DAYS_PER_YEAR,
    5.15138902046611451e-05  * SOLAR_MASS)

  init-values = [list: sun, jupiter, saturn, uranus, neptune]

  {px; py; pz} = for fold(acc from {0;0;0}, b from init-values):
    {
      acc.{0} + (b!vx * b!mass);
      acc.{1} + (b!vy * b!mass);
      acc.{2} + (b!vz * b!mass)
    }
  end

  dummy = sun!{
    vx: 0 - (px / SOLAR_MASS),
    vy: 0 - (py / SOLAR_MASS),
    vz: 0 - (pz / SOLAR_MASS)
  }

  init-values
end

fun energy(bodies):
  var e = 0.0
  dummy = for fold(acc from 0, i from range(0, bodies.length())) block:
    e := e + (0.5 * bodies.get(i)!mass * bodies.get(i).speedSq())
    for fold(acc2 from 0, j from range(i + 1, bodies.length())):
      dx = bodies.get(i)!x - bodies.get(j)!x
      dy = bodies.get(i)!y - bodies.get(j)!y
      dz = bodies.get(i)!z - bodies.get(j)!z
      dist = num-sqrt((dx * dx) + (dy * dy) + (dz * dz))
      e := e - ((bodies.get(i)!mass * bodies.get(j)!mass) / dist)
    end
  end
  e
end

fun advance(bodies, dt) block:
  for map(i from range(0, bodies.length())):
    for map(j from range(i + 1, bodies.length())) block:
      dx = bodies.get(i)!x - bodies.get(j)!x
      dy = bodies.get(i)!y - bodies.get(j)!y
      dz = bodies.get(i)!z - bodies.get(j)!z
      dist = num-sqrt((dx * dx) + (dy * dy) + (dz * dz))
      mag = dt / (dist * dist * dist)

      bodies.get(i).advance(dx, dy, dz, 0 - (bodies.get(j)!mass * mag))
      bodies.get(j).advance(dx, dy, dz, bodies.get(i)!mass * mag)
    end
  end

  for map(b from bodies):
    b.move(dt)
  end
end

bodies = initial-values()

print(energy(bodies))
print("\n")

n = 20000
d = for map(c from range(0, n)):
    advance(bodies, 0.01)
end

print(energy(bodies))
print("\n")
