({
  requires: [
    { "import-type": "builtin", "name": "image-lib" },
    { "import-type": "builtin", "name": "make-image" }
  ],
  nativeRequires: [
    "pyret-base/js/js-numbers",
  ],
  provides: {
    shorthands: {
      "Image": { tag: "name",
                 origin: { "import-type": "uri", uri: "builtin://image-lib" },
                 name: "Image" },
      "FillMode": "String",
      "FontFamily": "String",
      "FontStyle": "String",
      "FontWeight": "String",
      "XPlace": "String",
      "YPlace": "String",
      "ColorString": "String",
      "Color": { tag: "name",
                  origin: { "import-type": "uri", uri: "builtin://color" },
                  name: "Color" },
      "OptColor": ["tyapp", { tag: "name",
                              origin: { "import-type": "uri", uri: "builtin://option" },
                              name: "Option" },
                   [{ tag: "name",
                      origin: { "import-type": "uri", uri: "builtin://color" },
                      name: "Color" }]],
      "Either": { tag: "name",
                  origin: { "import-type": "uri", uri: "builtin://either" },
                  name: "Either" },
      "LoC": ["tyapp", { tag: "name",
                         origin: { "import-type": "uri", uri: "builtin://lists" },
                         name: "List" },
              [{ tag: "name",
                 origin: { "import-type": "uri", uri: "builtin://color" },
                 name: "Color" }]],
      "LoI": ["tyapp", { tag: "name",
                         origin: { "import-type": "uri", uri: "builtin://lists" },
                         name: "List" },
              [{ tag: "name",
                 origin: { "import-type": "uri", uri: "builtin://image-lib" },
                 name: "Image" }]],
      "LoP": ["tyapp", { tag: "name",
                         origin: { "import-type": "uri", uri: "builtin://lists" },
                         name: "List" },
              [{ tag: "name",
                 origin: { "import-type": "uri", uri: "builtin://internal-image-shared" },
                 name: "Point" }]],
    },
    values: {
      "circle": ["arrow", ["Number", "FillMode", "ColorString"], "Image"],
      "is-image-color": ["arrow", ["tany"], "Boolean"],
      "is-mode": ["arrow", ["tany"], "Boolean"],
      "is-x-place": ["arrow", ["tany"], "Boolean"],
      "is-y-place": ["arrow", ["tany"], "Boolean"],
      "is-angle": ["arrow", ["tany"], "Boolean"],
      "is-side-count": ["arrow", ["tany"], "Boolean"],
      "is-step-count": ["arrow", ["tany"], "Boolean"],
      "is-image": ["arrow", ["tany"], "Boolean"],
      "image-url": ["arrow", ["String"], "Image"],
      "image-file": ["arrow", ["String"], "Image"],
      "save-image": ["arrow", ["Image", "String"], "Image"],
      "images-equal": ["arrow", ["Image", "Image"], "Boolean"],
      "images-difference": ["arrow", ["Image", "Image"], ["tyapp", "Either", ["String", "Number"]]],
      "text": ["arrow", ["String", "Number", "ColorString"], "Image"],
      "text-font": ["arrow",
                    ["String", "Number", "ColorString", "String", "FontFamily", "FontStyle", "FontWeight", "Boolean"],
                    "Image"],
      "overlay": ["arrow", ["Image", "Image"], "Image"],
      "overlay-list": ["arrow", ["LoI"], "Image"],
      "overlay-xy": ["arrow", ["Image", "Number", "Number", "Image"], "Image"],
      "overlay-align": ["arrow", ["XPlace", "YPlace", "Image", "Image"], "Image"],
      "overlay-align-list": ["arrow", ["XPlace", "YPlace", "LoI"], "Image"],
      "overlay-onto-offset": ["arrow",
                              ["Image", "XPlace", "YPlace", "Number", "Number", "Image", "XPlace", "YPlace"],
                              "Image"],
      "underlay": ["arrow", ["Image", "Image"], "Image"],
      "underlay-list": ["arrow", ["LoI"], "Image"],
      "underlay-xy": ["arrow", ["Image", "Number", "Number","Image"], "Image"],
      "underlay-align": ["arrow", ["XPlace", "YPlace", "Image", "Image"], "Image"],
      "underlay-align-list": ["arrow", ["XPlace", "YPlace", "LoI"], "Image"],
      "beside": ["arrow", ["Image", "Image"], "Image"],
      "beside-list": ["arrow", ["LoI"], "Image"],
      "beside-align": ["arrow", ["YPlace", "Image", "Image"], "Image"],
      "beside-align-list": ["arrow", ["YPlace", "LoI"], "Image"],
      "above": ["arrow", ["Image", "Image"], "Image"],
      "above-list": ["arrow", ["LoI"], "Image"],
      "above-align": ["arrow", ["XPlace", "Image", "Image"], "Image"],
      "above-align-list": ["arrow", ["XPlace", "LoI"], "Image"],
      "below": ["arrow", ["Image", "Image"], "Image"],
      "below-list": ["arrow", ["LoI"], "Image"],
      "below-align": ["arrow", ["XPlace", "Image", "Image"], "Image"],
      "below-align-list": ["arrow", ["XPlace", "LoI"], "Image"],
      "empty-scene": ["arrow", ["Number", "Number"], "Image"],
      "empty-color-scene": ["arrow", ["Number", "Number", "ColorString"], "Image"],
      "put-image": ["arrow", ["Image", "Number", "Number", "Image"], "Image"],
      "translate": ["arrow", ["Image", "Number", "Number", "Image"], "Image"],
      "place-image": ["arrow", ["Image", "Number", "Number", "Image"], "Image"],
      "place-image-align": ["arrow", ["Image", "Number", "Number", "XPlace", "YPlace", "Image"], "Image"],
      "move-pinhole": ["arrow", ["Number", "Number", "Image"], "Image"],
      "place-pinhole": ["arrow", ["Number", "Number", "Image"], "Image"],
      "center-pinhole": ["arrow", ["Image"], "Image"],
      "rotate": ["arrow", ["Number", "Image"], "Image"],
      "scale": ["arrow", ["Number", "Image"], "Image"],
      "scale-xy": ["arrow", ["Number", "Number", "Image"], "Image"],
      "flip-horizontal": ["arrow", ["Image"], "Image"],
      "flip-vertical": ["arrow", ["Image"], "Image"],
      "reflect-x": ["arrow", ["Image"], "Image"],
      "reflect-y": ["arrow", ["Image"], "Image"],
      "frame": ["arrow", ["Image"], "Image"],
      "draw-pinhole": ["arrow", ["Image"], "Image"],
      "crop": ["arrow", ["Number", "Number", "Number", "Number", "Image"], "Image"],
      "line": ["arrow", ["Number", "Number", "ColorString"], "Image"],
      "add-line": ["arrow", ["Image", "Number", "Number", "Number", "Number", "ColorString"], "Image"],
      "scene-line": ["arrow", ["Image", "Number", "Number", "Number", "Number", "ColorString"], "Image"],
      "square": ["arrow", ["Number", "FillMode", "ColorString"], "Image"],
      "rectangle": ["arrow", ["Number", "Number", "FillMode", "ColorString"], "Image"],
      "regular-polygon": ["arrow", ["Number", "Number", "FillMode", "ColorString"], "Image"],
      "point-polygon": ["arrow", ["LoP", "FillMode", "ColorString"], "Image"],
      "ellipse": ["arrow", ["Number", "Number", "FillMode", "ColorString"], "Image"],
      "wedge": ["arrow", ["Number", "Number", "FillMode", "ColorString"], "Image"],
      "triangle": ["arrow", ["Number", "FillMode", "ColorString"], "Image"],
      "triangle-sas": ["arrow", ["Number", "Number", "Number", "FillMode", "ColorString"], "Image"],
      "triangle-sss": ["arrow", ["Number", "Number", "Number", "FillMode", "ColorString"], "Image"],
      "triangle-ass": ["arrow", ["Number", "Number", "Number", "FillMode", "ColorString"], "Image"],
      "triangle-ssa": ["arrow", ["Number", "Number", "Number", "FillMode", "ColorString"], "Image"],
      "triangle-aas": ["arrow", ["Number", "Number", "Number", "FillMode", "ColorString"], "Image"],
      "triangle-asa": ["arrow", ["Number", "Number", "Number", "FillMode", "ColorString"], "Image"],
      "triangle-saa": ["arrow", ["Number", "Number", "Number", "FillMode", "ColorString"], "Image"],
      "right-triangle": ["arrow", ["Number", "Number", "FillMode", "ColorString"], "Image"],
      "isosceles-triangle": ["arrow", ["Number", "Number", "FillMode", "ColorString"], "Image"],
      "star": ["arrow", ["Number", "FillMode", "ColorString"], "Image"],
      "star-sized": ["arrow", ["Number", "Number", "Number", "FillMode", "ColorString"], "Image"],
      "radial-star": ["arrow", ["Number", "Number", "Number", "FillMode", "ColorString"], "Image"],
      "star-polygon": ["arrow", ["Number", "Number", "Number", "FillMode", "ColorString"], "Image"],
      "rhombus": ["arrow", ["Number", "Number", "FillMode", "ColorString"], "Image"],
      "trim-image": ["arrow", ["Image"], "Image"],
      "image-to-color-list": ["arrow", ["Image"], "LoC"],
      "color-list-to-image": ["arrow", ["LoC", "Number", "Number", "Number", "Number"], "Image"],
      "color-at-position": ["arrow", ["Image", "Number", "Number"], "Color"],
      "color-list-to-bitmap": ["arrow", ["LoC", "Number", "Number"], "Image"],
      "image-width": ["arrow", ["Image"], "Number"],
      "image-height": ["arrow", ["Image"], "Number"],
      "image-baseline": ["arrow", ["Image"], "Number"],
      "image-pinhole-x": ["arrow", ["Image"], "Number"],
      "image-pinhole-y": ["arrow", ["Image"], "Number"],
      "name-to-color": ["arrow", ["String"], "OptColor"],
      "color-named": ["arrow", ["String"], "Color"],
      "empty-image": "Image"
    },
    aliases: {
      "Image": { tag: "name",
                 origin: { "import-type": "uri", uri: "builtin://image-lib" },
                 name: "Image" }
    }
  },
  theModule: function(runtime, namespace, uri, imageLib, makeImage, jsnums) {
    var ffi = runtime.ffi;

    var isString = runtime.isString;

    var image = runtime.getField(imageLib, "internal");
    var colorDb = image.colorDb;

    const checkArity = ffi.checkArity;
    const c = function(name, ...argsAndAnns) {
      runtime.checkArgsInternalInline("image-untyped", name, ...argsAndAnns);
    };
    const c1 = function(name, arg, ann) {
      runtime.checkArgsInternal1("image-untyped", name, arg, ann);
    };
    const c2 = function(name, arg1, ann1, arg2, ann2) {
      runtime.checkArgsInternal2("image-untyped", name, arg1, ann1, arg2, ann2);
    };
    const c3 = function(name, arg1, ann1, arg2, ann2, arg3, ann3) {
      runtime.checkArgsInternal3("image-untyped", name, arg1, ann1, arg2, ann2, arg3, ann3);
    };

    var ann = function(name, pred) {
      return runtime.makePrimitiveAnn(name, pred);
    };

    var identity = function(x) { return x; };

    var isPlaceX = function(x) {
      return (isString(x) &&
              (x.toString().toLowerCase() == "left"  ||
               x.toString().toLowerCase() == "right" ||
               x.toString().toLowerCase() == "center" ||
               x.toString().toLowerCase() == "pinhole" ||
               x.toString().toLowerCase() == "middle"));
    };
    var isPlaceY = function(x) {
      return (isString(x) &&
              (x.toString().toLowerCase() == "top"	  ||
               x.toString().toLowerCase() == "bottom"   ||
               x.toString().toLowerCase() == "baseline" ||
               x.toString().toLowerCase() == "center"   ||
               x.toString().toLowerCase() == "pinhole"  ||
               x.toString().toLowerCase() == "middle"));
    };

    var checkImagePred = function(val) {
      return runtime.isOpaque(val) && image.isImage(val.val);
    };
    var checkScenePred = function(val) {
      return runtime.isOpaque(val) && image.isScene(val.val);
    };

    var unwrapPoint2D = function(val) {
      var gf = runtime.getField;
      var hf = runtime.hasField;
      if (hf(val, "r") && hf(val, "theta")) {
        var r = jsnums.toFixnum(gf(val, "r"));
        var theta = jsnums.toFixnum(gf(val, "theta"));
        return { x: r * Math.cos(theta), y: r * Math.sin(theta) };
      }
      return { x: jsnums.toFixnum(gf(val, "x")), y: jsnums.toFixnum(gf(val, "y")) };
    };

    var annListImage = ann("List<Image>", function(val) {
      if (!runtime.ffi.isList(val)) return false;
      var cur = val;
      var gf = runtime.getField;
      while (runtime.unwrap(ffi.isLink(cur))) {
        var f = gf(cur, "first");
        if (!checkImagePred(f)) return false;
        cur = gf(cur, "rest");
      }
      return true;
    });

    var unwrapColor = function(val) {
      var aColor = val;
      if (colorDb.get(aColor)) {
        aColor = colorDb.get(aColor);
      }
      return aColor;
    };

    const ANNOTS = {
      annString: runtime.String,
      annNumber: runtime.Number,
      annPositive: runtime.NumPositive,
      annNumNonNegative: runtime.NumNonNegative,
      annByte: ann("Number between 0 and 255", function(val) {
        return runtime.isNumber(val)
          && jsnums.greaterThanOrEqual(val, 0, runtime.NumberErrbacks)
          && jsnums.greaterThanOrEqual(255, val, runtime.NumberErrbacks);
      }),
      annReal: ann("Real Number", function(val) {
        return runtime.isNumber(val) && jsnums.isReal(val);
      }),
      annNatural: ann("Natural Number", function(val) {
        return runtime.isNumber(val) && jsnums.isInteger(val)
          && jsnums.greaterThanOrEqual(val, 0, runtime.NumberErrbacks);
      }),
      unwrapColor: unwrapColor,
      annColor: ann("Color", image.isColorOrColorString),
      annPoint2D: image.annPoint,
      annMode: ann("Mode (\"outline\" or \"solid\")", function(x) {
        return (isString(x) &&
                (x.toString().toLowerCase() == "solid" ||
                 x.toString().toLowerCase() == "outline")) ||
          ((jsnums.isReal(x)) &&
           (jsnums.greaterThanOrEqual(x, 0, runtime.NumberErrbacks) &&
            jsnums.lessThanOrEqual(x, 1, runtime.NumberErrbacks)));
      }),
      unwrapMode: function(val) {
        if (typeof val === "string")
          return val;
        else
          return jsnums.toFixnum(val);
      },
      annFontFamily: ann("Font Family", function(x){
        return (isString(x) &&
                (x.toString().toLowerCase() == "default" ||
                 x.toString().toLowerCase() == "decorative" ||
                 x.toString().toLowerCase() == "roman" ||
                 x.toString().toLowerCase() == "script" ||
                 x.toString().toLowerCase() == "swiss" ||
                 x.toString().toLowerCase() == "modern" ||
                 x.toString().toLowerCase() == "symbol" ||
                 x.toString().toLowerCase() == "system"))
          || (x === false);		// false is also acceptable
      }),
      unwrapFontFamily: identity,
      annFontStyle: ann("Font Style (\"normal\", \"italic\", or \"slant\")", function(x){
        return (isString(x) &&
                (x.toString().toLowerCase() == "normal" ||
                 x.toString().toLowerCase() == "italic" ||
                 x.toString().toLowerCase() == "slant"))
          || (x === false);		// false is also acceptable
      }),
      unwrapFontStyle: identity,
      annFontWeight: ann("Font Weight", function(x){
        return (isString(x) &&
                (x.toString().toLowerCase() == "normal" ||
                 x.toString().toLowerCase() == "bold" ||
                 x.toString().toLowerCase() == "light"))
          || (x === false);		// false is also acceptable
      }),
      unwrapFontWeight: identity,
      annPlaceX: ann("X Place (\"left\", \"middle\", \"center\", \"pinhole\", or \"right\")", isPlaceX),
      unwrapPlaceX: function(val) {
        if (val.toString().toLowerCase() == "center") return "middle";
        return val;
      },
      annPlaceY: ann("Y Place (\"top\", \"bottom\", \"center\", \"pinhole\", \"baseline\", or \"middle\")", isPlaceY),
      unwrapPlaceY: function(val) {
        if (val.toString().toLowerCase() == "middle") return "center";
        return val;
      },
      annImage: ann("Image", checkImagePred),
      unwrapImage: function(val) {
        return val.val;
      },
      annImageOrScene: ann("Image", function(val) {
        return runtime.isOpaque(val) && (image.isImage(val.val) || image.isScene(val.val));
      }),
      unwrapImageOrScene: function(val) {
        return val.val;
      },
      annAngle: ann("Angle (a number 'x' where 0 <= x < 360)", image.isAngle),
      annListImage: annListImage,
      unwrapListofImage: identity,
      annListColor: ann("List<Color>", function(val) {
        return runtime.ffi.isList(val);
      }),
      unwrapListofColor: function(val) {
        return ffi.makeList(ffi.toArray(val).map(unwrapColor));
      },
      annListPoint2D: ann("List<Point>", function(val) {
        return runtime.ffi.isList(val);
      }),
      unwrapListofPoint2D: function(val) {
        return ffi.toArray(val).map(unwrapPoint2D);
      },
      annSideCount: ann("Side Count", image.isSideCount),
      annStepCount: ann("Step Count", image.isStepCount),
      annPointCount: ann("Points Count", image.isPointsCount)
    };


    var values = makeImage.makeImageLib("image-untyped", ANNOTS);
    function f(name, fun) {
      values[name] = runtime.makeFunction(fun, name);
    }

    f("is-image-color", function(maybeColor) {
      checkArity(1, arguments, "image", false);
      return runtime.wrap(image.isColorOrColorString(maybeColor));
    });
    f("is-mode", function(maybeMode) {
      checkArity(1, arguments, "is-mode", false);
      return runtime.wrap(isMode(maybeMode));
    });
    f("is-x-place", function(maybeXPlace) {
      checkArity(1, arguments, "is-x-place", false);
      return runtime.wrap(isPlaceX(maybeXPlace));
    });
    f("is-y-place", function(maybeYPlace) {
      checkArity(1, arguments, "is-y-place", false);
      return runtime.wrap(isPlaceY(maybeYPlace));
    });


    
    return runtime.makeModuleReturn(values, {
        "Image": image.Image,
        "Scene": runtime.makePrimitiveAnn("Scene", checkScenePred)
      });
  }
})
