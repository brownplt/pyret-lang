
var tester = require("../test-util/util.js");
var fs = require("fs");

function makeTest(name, imageLib, expr, baseurl, badness) {
  return `
include ${imageLib}
include image-structs
include either

i2 = image-url("${baseurl}" + "/test-images/" + "${name}" + ".png")
i1 = ${expr}

i1
i2

check:
  fun within-n-badness(n):
    lam(img1, img2):
      diff = images-difference(img1, img2)
      cases(Either) diff block:
        | left(diff-error) =>
          "no error" is diff-error
          false
        | right(badness) =>
          when not(badness <= n) block:
            badness satisfies (_ <= n)
          end
          badness <= n
      end
    end
  end
  i1 is%(within-n-badness(${badness})) i2
end`
}

// Like makeTest, for images containing system-font text whose rendered
// dimensions vary by a few pixels across browsers/OSes. Checks that width and
// height are within sizeTol px, then pads both onto a shared transparent box
// before comparing pixels with images-difference.
function makeTextTest(name, imageLib, expr, baseurl, badness, sizeTol) {
  var isTyped = imageLib === "image-typed";
  var solid = isTyped ? "mode-solid" : '"solid"';
  var xLeft = isTyped ? "x-left" : '"left"';
  var yTop  = isTyped ? "y-top"  : '"top"';
  return `
include ${imageLib}
include image-structs
include either

i2 = image-url("${baseurl}" + "/test-images/" + "${name}" + ".png")
i1 = ${expr}

i1
i2

check:
  w1 = image-width(i1)
  h1 = image-height(i1)
  w2 = image-width(i2)
  h2 = image-height(i2)
  num-abs(w1 - w2) satisfies (_ <= ${sizeTol})
  num-abs(h1 - h2) satisfies (_ <= ${sizeTol})
  bg = rectangle(num-max(w1, w2), num-max(h1, h2), ${solid}, color(0, 0, 0, 0))
  norm1 = overlay-align(${xLeft}, ${yTop}, i1, bg)
  norm2 = overlay-align(${xLeft}, ${yTop}, i2, bg)
  cases(Either) images-difference(norm1, norm2) block:
    | left(diff-error) =>
      "no error" is diff-error
    | right(badness) =>
      badness satisfies (_ <= ${badness})
  end
end`
}

var UNTYPED = {
  'xLeft': '"left"',
  'xMiddle': '"middle"',
  'xRight': '"right"',
  'xPinhole': '"pinhole"',
  'yTop': '"top"',
  'yCenter': '"center"',
  'yBaseline': '"baseline"',
  'yBottom': '"bottom"',
  'yPinhole': '"pinhole"',
  'solid': '"solid"',
  'outline': '"outline"',
  'fade': '',
  'red': '"red"',
  'green': '"green"',
  'blue': '"blue"',
  'purple': '"purple"',
  'navy': '"navy"',
  'pink': '"pink"',
  'yellow': '"yellow"',
  'orange': '"orange"',
  'brown': '"brown"',
  'tan': '"tan"',
  'white': '"white"',
  'gray': '"gray"',
  'black': '"black"'
}
var TYPED = {
  'xLeft': 'x-left',
  'xMiddle': 'x-middle',
  'xRight': 'x-right',
  'xPinhole': 'x-pinhole',
  'yTop': 'y-top',
  'yCenter': 'y-center',
  'yBaseline': 'y-baseline',
  'yBottom': 'y-bottom',
  'yPinhole': 'y-pinhole',
  'solid': 'mode-solid',
  'outline': 'mode-outline',
  'fade': 'mode-fade',
  'red': 'red',
  'green': 'green',
  'blue': 'blue',
  'purple': 'purple',
  'navy': 'navy',
  'pink': 'pink',
  'yellow': 'yellow',
  'orange': 'orange',
  'brown': 'brown',
  'tan': 'tan',
  'white': 'white',
  'gray': 'gray',
  'black': 'black'
}
  

function makeTests(ARGS) {
  function replaceArgs(strings, ...keys) {
    var result = [strings[0]];
    keys.forEach(function(key, i) {
      var value = ARGS[key];
      result.push(value, strings[i + 1]);
    });
    return result.join('');
  }
return [
["above-align-left",
replaceArgs`
  above-align(${"xLeft"},
    circle(30, ${"solid"}, ${"red"}),
    above-align(${"xLeft"}, circle(50, ${"solid"}, ${"green"}), circle(20, ${"solid"}, ${"blue"})))`],

["above-align-right",
replaceArgs`
  above-align(${"xRight"},
    circle(30, ${"solid"}, ${"red"}),
    above-align(${"xRight"}, circle(50, ${"solid"}, ${"green"}), circle(20, ${"solid"}, ${"blue"})))`],

["above-align-middle",
replaceArgs`
  above-align(${"xMiddle"},
    circle(30, ${"solid"}, ${"red"}),
    above-align(${"xMiddle"}, circle(50, ${"solid"}, ${"green"}), circle(20, ${"solid"}, ${"blue"})))`],

["above-list",
replaceArgs`
  above-list([list: circle(50, ${"solid"}, ${"red"}), square(70, ${"solid"}, ${"blue"})])`],

["beside-align-top",
replaceArgs`
  beside-align(${"yTop"},
    circle(30, ${"solid"}, ${"red"}),
    beside-align(${"yTop"}, circle(50, ${"solid"}, ${"green"}), circle(20, ${"solid"}, ${"blue"})))`],

["beside-align-bottom",
replaceArgs`
  beside-align(${"yBottom"},
    circle(30, ${"solid"}, ${"red"}),
    beside-align(${"yBottom"}, circle(50, ${"solid"}, ${"green"}), circle(20, ${"solid"}, ${"blue"})))`],

["beside-align-middle",
replaceArgs`
  beside-align(${"yCenter"},
    circle(30, ${"solid"}, ${"red"}),
    beside-align(${"yCenter"}, circle(50, ${"solid"}, ${"green"}), circle(20, ${"solid"}, ${"blue"})))`],


["solid-green-circle",
replaceArgs`
  circle(20, ${"solid"}, ${"green"})`],

["outline-turquoise-rectangle",
replaceArgs`
  rectangle(20, 30, ${"outline"}, turquoise)`],

["solid-translucent-red-rectangle",
replaceArgs`
  rectangle(200, 300, ${"fade"}(10 / 255), ${"red"})`],

["outline-red-rectangle",
replaceArgs`
  rectangle(200, 300, ${"outline"}, ${"red"})`],

["invisible-red-rectangle",
replaceArgs`
  rectangle(200, 300, ${"fade"}(0), ${"red"})`],

["solid-red-triangle",
replaceArgs`
  triangle(50, ${"solid"}, ${"red"})`],

["halfred-triangle",
replaceArgs`
  triangle(50, ${"solid"}, color(255, 0, 0, 128 / 255))`],

["translucent-halfred-triangle",
replaceArgs`
  triangle(50, ${"fade"}(128 / 255), color(255, 0, 0, 128 / 255))`],

["quarterred-triangle",
replaceArgs`
  triangle(50, ${"solid"}, color(255, 0, 0, 64 / 255))`],

["blue-circle-center-scene",
replaceArgs`
  place-image(circle(50, ${"solid"}, ${"blue"}), 50, 50, empty-scene(100, 100))`],

["blue-circle-ur-corner-scene",
replaceArgs`
  put-image(circle(50, ${"solid"}, ${"blue"}), 100, 100, empty-scene(100, 100))`],

["framed-overlay-triangle",
replaceArgs`
  frame(
    overlay-align(${"xPinhole"}, ${"yPinhole"}, 
      circle(3, ${"solid"}, ${"black"}), 
      draw-pinhole(triangle(50, ${"outline"}, ${"blue"}))))`],

["rotate-right-triangle-center",
replaceArgs`
  for fold(acc from empty-image, row from range(0, 3)):
    above(
      for fold(shadow acc from empty-image, col from range(0, 10)):
        beside(acc,
          overlay-align(${"xPinhole"}, ${"yPinhole"}, 
            draw-pinhole(circle(50 / num-sqrt(2), ${"outline"}, ${"black"})), 
            draw-pinhole(rotate(((row * 10) + col) * 12,
                center-pinhole(triangle-sas(50, 90, 50, ${"outline"}, ${"green"}))))))
      end,
      acc)
  end`],

  ["rotate-right-triangle-pinhole",
replaceArgs`
  for fold(acc from empty-image, row from range(0, 3)):
    above(
      for fold(shadow acc from empty-image, col from range(0, 10)):
        beside(acc,
          overlay-align(${"xPinhole"}, ${"yPinhole"}, 
            draw-pinhole(circle(50 / num-sqrt(2), ${"outline"}, ${"black"})), 
            draw-pinhole(rotate(((row * 10) + col) * 12, triangle-sas(50, 90, 50, ${"outline"}, ${"green"})))))
      end,
      acc)
  end`],

["rotate-equilateral-triangle-pinhole",
replaceArgs`
  for fold(acc from empty-image, row from range(0, 3)):
    above(
      for fold(shadow acc from empty-image, col from range(0, 10)):
        beside(acc,
          overlay-align(${"xPinhole"}, ${"yPinhole"}, 
            draw-pinhole(circle(50 / num-sqrt(3), ${"outline"}, ${"black"})), 
            draw-pinhole(rotate(((row * 10) + col) * 12, triangle(50, ${"outline"}, ${"green"})))))
      end,
      acc)
  end`],

["triangle-pinhole-center", 
replaceArgs`
  beside(
    overlay(
      triangle(50, ${"outline"}, ${"red"}),
      rotate(180, triangle(50, ${"outline"}, ${"purple"})))
    ,
    overlay-align(${"xMiddle"}, ${"yCenter"},
      triangle(50, ${"outline"}, ${"red"}),
      rotate(180, triangle(50, ${"outline"}, ${"purple"}))))`],

["star-pinhole-center",
replaceArgs`
  beside(
     overlay(
      star(50, ${"outline"}, ${"red"}),
      rotate(180, star(50, ${"outline"}, ${"purple"})))
    ,
    overlay-align(${"xMiddle"}, ${"yCenter"},
      star(50, ${"outline"}, ${"red"}),
      rotate(180, star(50, ${"outline"}, ${"purple"}))))`],

["red-black-ellipses",
replaceArgs`
  overlay(ellipse(10, 10, ${"solid"}, ${"red"}),
    overlay(ellipse(20, 20, ${"solid"}, ${"black"}),
      overlay(ellipse(30, 30, ${"solid"}, ${"red"}),
        overlay(ellipse(40, 40, ${"solid"}, ${"black"}),
          overlay(ellipse(50, 50, ${"solid"}, ${"red"}),
            ellipse(60, 60, ${"solid"}, ${"black"}))))))`],

["centered-target",
replaceArgs`
  place-image(
    overlay(ellipse(10, 10, ${"solid"}, ${"white"}),
      overlay(ellipse(20, 20, ${"solid"}, ${"black"}),
        overlay(ellipse(30, 30, ${"solid"}, ${"white"}),
          overlay(ellipse(40, 40, ${"solid"}, ${"black"}),
            overlay(ellipse(50, 50, ${"solid"}, ${"white"}),
              ellipse(60, 60, ${"solid"}, ${"black"})))))),
    150, 100,
    rectangle(300, 200, ${"solid"}, ${"black"}))`],

["wedge-wheel",
replaceArgs`
  for fold(img from empty-image, quad from range(0, 4)):
    above(
      img,
      for fold(shadow img from empty-image, angle from range(0, 9)):
        beside(img, 
          overlay-align(${"xPinhole"}, ${"yPinhole"},
            frame(draw-pinhole(wedge(30, (90 * quad) + (10 * angle), ${"outline"}, ${"red"}))),
            rectangle(70, 70, ${"outline"}, ${"gray"})))
      end)
  end`],

["rotate-wedge",
replaceArgs`
  for fold(img from empty-image, quad from range(0, 4)):
    above(
      img,
      for fold(shadow img from empty-image, angle from range(0, 9)):
        beside(img, 
          overlay-align(${"xPinhole"}, ${"yPinhole"},
            frame(draw-pinhole(rotate((90 * quad) + (10 * angle), wedge(30, 60, ${"outline"}, ${"red"})))),
            rectangle(70, 70, ${"outline"}, ${"gray"})))
      end)
  end`],

["rotate-overlay-wedge",
replaceArgs`
  let stroked-wedge = overlay(wedge(30, 60, ${"outline"}, ${"red"}), wedge(30, 60, ${"solid"}, ${"tan"})):
    for fold(img from empty-image, quad from range(0, 4)):
      above(
        img,
        for fold(shadow img from empty-image, angle from range(0, 9)):
          beside(img, 
            overlay-align(${"xPinhole"}, ${"yPinhole"},
              frame(draw-pinhole(rotate((90 * quad) + (10 * angle), 
                    stroked-wedge))),
              rectangle(70, 70, ${"outline"}, ${"gray"})))
        end)
    end
  end`],

["trivial-pursuit",  
replaceArgs`
 frame(for fold(img from empty-image, c from [list: ${"pink"}, ${"yellow"}, ${"orange"}, ${"blue"}, ${"brown"}, ${"green"}]):
      overlay-align(${"xPinhole"}, ${"yPinhole"}, rotate(60, img),
        overlay-align(${"xPinhole"}, ${"yPinhole"},
          wedge(30, 60, ${"outline"}, ${"purple"}),
          wedge(30, 60, ${"solid"}, c)))
   end)`],

 
["australia-ish",
replaceArgs`
let
flag2 =
  place-image(
    rotate(90,
      underlay-align(${"xMiddle"}, ${"yCenter"},
        rectangle(50, 450, ${"solid"}, ${"white"}),
        underlay-align(${"xMiddle"}, ${"yCenter"},
          rotate(90, rectangle(50, 450, ${"solid"}, ${"white"})),
          underlay-align(${"xMiddle"}, ${"yCenter"},
            rotate(90, rectangle(30, 450, ${"solid"}, ${"red"})),
            rotate(180, rectangle(30, 450, ${"solid"}, ${"red"})))))),
    200, 100,
    place-image(
      rotate(65,
        underlay-align(${"xMiddle"}, ${"yCenter"},
          rectangle(15, 450, ${"solid"}, ${"red"}),
          rotate(50, rectangle(15, 450, ${"solid"}, ${"red"})))),
      200, 100,
      place-image(
        rotate(65,
          underlay-align(${"xMiddle"}, ${"yCenter"},
            rectangle(40, 450, ${"solid"}, ${"white"}),
            rotate(50, rectangle(40, 450, ${"solid"}, ${"white"})))),
        200, 100,
        rectangle(400, 200, ${"solid"}, ${"navy"})))):

place-image(flag2, 200, 100,
    place-image(star-polygon(30, 7, 3, ${"solid"}, ${"white"}),
      650, 60,
      place-image(star-polygon(50, 7, 3, ${"solid"}, ${"white"}),
        200, 300,
        place-image(star-polygon(40, 7, 3, ${"solid"}, ${"white"}),
          500, 300,
          place-image(star-polygon(40, 7, 3, ${"solid"}, ${"white"}),
            490, 220,
            rectangle(900, 400, ${"solid"}, ${"navy"}))))))
end`],

];
}

function makeTextTests(ARGS) {
  function replaceArgs(strings, ...keys) {
    var result = [strings[0]];
    keys.forEach(function(key, i) {
      var value = ARGS[key];
      result.push(value, strings[i + 1]);
    });
    return result.join('');
  }
  return [
    ["howdy-yall",
    replaceArgs`
      let aligned-text =
        overlay-align(${"xMiddle"}, ${"yBaseline"},
          line(100, 0, ${"red"}),
          beside-align(${"yBaseline"},
            overlay-align(${"xMiddle"}, ${"yBaseline"}, line(100, 0, ${"red"}), text("Ĥowdy", 50, ${"black"})),
            overlay-align(${"xMiddle"}, ${"yBaseline"}, line(100, 0, ${"green"}), text(" y'all", 10, ${"blue"})))):
        place-image(line(200, 0, ${"purple"}), 0, image-baseline(aligned-text), frame(aligned-text))
      end`,
      40, 3],
  ];
}

const DEFAULT_BADNESS = 26;
const DEFAULT_TEXT_SIZE_TOL = 3;
const IMAGE_BASE_URL = process.env.BASE_URL || process.env.PYRET_FIXTURE_BASE;

describe("Image equality - typed", function() {
  before(tester.setupMulti("Rendering errors"));
  after(tester.teardownMulti);

  makeTests(TYPED).forEach(function(t) {
    var badness = t.length >= 3 ? t[2]: DEFAULT_BADNESS;
    tester.testRunAndAllTestsPass(it, "image-equality-typed-" + t[0],
                                  makeTest(t[0], "image-typed", t[1], IMAGE_BASE_URL, badness) );
  });
  makeTextTests(TYPED).forEach(function(t) {
    var badness = t.length >= 3 ? t[2]: DEFAULT_BADNESS;
    var sizeTol = t.length >= 4 ? t[3]: DEFAULT_TEXT_SIZE_TOL;
    tester.testRunAndAllTestsPass(it, "image-equality-typed-" + t[0],
                                  makeTextTest(t[0], "image-typed", t[1], IMAGE_BASE_URL, badness, sizeTol) );
  });
});

describe("Image equality - untyped", function() {
  before(tester.setupMulti("Rendering errors"));
  after(tester.teardownMulti);

  makeTests(UNTYPED).forEach(function(t) {
    var badness = t.length >= 3 ? t[2]: DEFAULT_BADNESS;
    tester.testRunAndAllTestsPass(it, "image-equality-untyped-" + t[0],
                                  makeTest(t[0], "image", t[1], IMAGE_BASE_URL, badness) );
  });
  makeTextTests(UNTYPED).forEach(function(t) {
    var badness = t.length >= 3 ? t[2]: DEFAULT_BADNESS;
    var sizeTol = t.length >= 4 ? t[3]: DEFAULT_TEXT_SIZE_TOL;
    tester.testRunAndAllTestsPass(it, "image-equality-untyped-" + t[0],
                                  makeTextTest(t[0], "image", t[1], IMAGE_BASE_URL, badness, sizeTol) );
  });
});
/*
fun make-stars(n):
  ask:
    | 1 == n then: star(12, mode-solid, "purple")
    | otherwise: beside(star(12, mode-solid, "purple"), make-stars(n - 1))
  end
end
fun bar-graph(l1):
  ask:
    | l1 == empty then: circle(0, "outline", blue)
    | otherwise: above-align("left", make-stars(l1.first), bar-graph(l1.rest))
  end
end
t("bar-graph",
  bar-graph([list: 1, 3, 5, 3, 9, 5, 3, 4, 4, 3, 5, 2]))

*/
