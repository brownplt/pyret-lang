### Looks shipshape

import global as G
f = lam({sum; count} as input :: { Number; Number }, n :: Number):
  { {sum + n; count + 1}; (sum + n) / (count + 1) }
end
check:
  f({2; 4}, 10) is { {12; 5}; 2.4 }
end
