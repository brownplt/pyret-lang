import file as F
import ast as A
import parse-pyret as P
import builtin-modules as B
import cmdline-lib as C
import checker as CH
import format as FMT
import string-dict as SD
import json as J
import graph as G
import image as I
import s-exp as S
import load-lib as LL
import world as W
import runtime-lib as RL
import particle as PART
import pathlib as PATH
import plot as PLOT

print("A dummy location: " + torepr(A.dummy-loc))
print("Reading from a file: " + F.input-file("test.txt").read-file())
print("Parsing a small program: " + torepr(P.surface-parse("x = 10", "test")))

l = B.builtin-raw-locator("src/js/troveA/builtin-modules")

print("Using a raw locator: " + string-substring(l.get-raw-compiled(), 0, 30))

print("Command line arguments: " + torepr(C.command-line-arguments()))

print("Rendering empty check results: " + tostring(CH.render-check-results([list:])))

print("Using format strings: " + torepr(FMT.format("~a ~a", [list: {x:5}, true])))

print("Printing a stringdict: " + torepr([SD.string-dict: "a", 5]))

print("Reading JSON: " + torepr(J.read-json("[\"forall\", [\"a\"]]")))

print("Graph.tree-options: " + torepr(G.tree-options))

print("Image-width of a radius=30 circle: " + torepr(I.image-width(I.circle(30, "solid", "red"))))

print("An sexp: " + torepr(S.read-s-exp("(a 5 \"str\" (nested (list 1 2 3)))")))

print("Load-lib.render-check-results: " + torepr(LL.render-check-results))

print("BigBang: " + torepr(W.big-bang))

print("Particle.D7 is " + torepr(PART.D7))

print("Path separator is: " + torepr(PATH.path-sep))

print("Plot.plot-options: " + torepr(PLOT.plot-options))
