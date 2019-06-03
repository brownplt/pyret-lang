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
end
