### pass
import global as G
import list as L

m1 = 1 + 1000000
r1 = m1 == 1000001

m2 = 1 - 1000000
r2 = m2 == -999999

m3 = 5 * 1450
r3 = m3 == 7250

m4 = 1230 / 5
r4 = m4 == 246

m5 = 113 / 5
r5 = m5 == (226 / 10)

m6 = 30 / 90
r6 = m6 == (1 / 3)

m7 = (1 / 10) + (2 / 10)
r7 = m7 == (3 / 10)

m8 = ((1 / 10) + (2 / 10)) * (1 / 30)
r8 = m8 == (1 / 100)

m9 = (12.3 / 5.5)
r9 = m9 == (123 / 55)

m10 = (1.1 / 34) * 2
r10 = m10  == (2.2 / 34)

final = r1
  and r2
  and r3
  and r4
  and r5
  and r6
  and r7
  and r8
  and r9
  and r10

if final:
  G.console-log("pass")
else:
  G.console-log([L.list:
    r1,
    r2,
    r3,
    r4,
    r5,
    r6,
    r7,
    r8,
    r9,
    r10,
  ])
end
