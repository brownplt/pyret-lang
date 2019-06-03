check:
  2%<m> + 2%<m> is 4%<m>
  (2%<m> + 2%<s>) raises "Cannot add units: m and s"

  2%<m> - 2%<m> is 0%<m>
  (2%<m> - 2%<s>) raises "Cannot add units: m and s"

  2%<m> * 2%<m> is 4%<m ^ 2>
  2%<m> * 2%<m ^ -1> is 4
  2%<m> * 2%<s> is 4%<m * s>
  2 * 2%<m> is 4%<m>

  2%<m> / 2%<m> is 1
  2%<m> / 2%<s> is 1%<m / s>
  2%<m> / 2%<s ^ 2> is 1%<m / (s ^ 2)>

  2 == 2%<m> is false
  2%<s> == 2%<m> is false

  2%<m> is 2%<m>
  2 is 2%<m / m>
  2%<s * m> is 2%<m * s>
  2%<(((s * m) ^ 2) / t) ^ 2> is 2%<m * s * m * s * (((m * s) / t) ^ 2)>
end
