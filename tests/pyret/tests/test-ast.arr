import ast as A
import parse-pyret as P

check:
  P.parse-dialect("Pyret", "import gdrive(\"foo\") as G", "test") satisfies A.is-s-program
  P.parse-dialect("Pyret", "import gdrive(\"foo\", \"bar\") as G", "test") satisfies A.is-s-program
end
