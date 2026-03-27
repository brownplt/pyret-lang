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
      "FillMode": { tag: "name",
                    origin: { "import-type": "uri", uri: "builtin://internal-image-shared" },
                    name: "FillMode" },
      "FontFamily": { tag: "name",
                    origin: { "import-type": "uri", uri: "builtin://internal-image-shared" },
                    name: "FontFamily" },
      "FontStyle": { tag: "name",
                    origin: { "import-type": "uri", uri: "builtin://internal-image-shared" },
                    name: "FontStyle" },
      "FontWeight": { tag: "name",
                    origin: { "import-type": "uri", uri: "builtin://internal-image-shared" },
                      name: "FontWeight" },
      "Point": { tag: "name",
                 origin: { "import-type": "uri", uri: "builtin://internal-image-shared" },
                 name: "Point" },
      "XPlace": { tag: "name",
                  origin: { "import-type": "uri", uri: "builtin://internal-image-shared" },
                  name: "XPlace" },
      "YPlace": { tag: "name",
                  origin: { "import-type": "uri", uri: "builtin://internal-image-shared" },
                  name: "YPlace" },
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
      "circle": ["arrow", ["Number", "FillMode", "Color"], "Image"],
      "is-angle": ["arrow", ["tany"], "Boolean"],
      "is-side-count": ["arrow", ["tany"], "Boolean"],
      "is-step-count": ["arrow", ["tany"], "Boolean"],
      "is-image": ["arrow", ["tany"], "Boolean"],
      "image-url": ["arrow", ["String"], "Image"],
      "image-file": ["arrow", ["String"], "Image"],
      "save-image": ["arrow", ["Image", "String"], "Image"],
      "images-equal": ["arrow", ["Image", "Image"], "Boolean"],
      "images-difference": ["arrow", ["Image", "Image"], ["tyapp", "Either", ["String", "Number"]]],
      "text": ["arrow", ["String", "Number", "Color"], "Image"],
      "text-font": ["arrow",
                    ["String", "Number", "Color", "String", "FontFamily", "FontStyle", "FontWeight", "Boolean"],
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
      "empty-color-scene": ["arrow", ["Number", "Number", "Color"], "Image"],
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
      "line": ["arrow", ["Number", "Number", "Color"], "Image"],
      "add-line": ["arrow", ["Image", "Number", "Number", "Number", "Number", "Color"], "Image"],
      "scene-line": ["arrow", ["Image", "Number", "Number", "Number", "Number", "Color"], "Image"],
      "square": ["arrow", ["Number", "FillMode", "Color"], "Image"],
      "rectangle": ["arrow", ["Number", "Number", "FillMode", "Color"], "Image"],
      "regular-polygon": ["arrow", ["Number", "Number", "FillMode", "Color"], "Image"],
      "point-polygon": ["arrow", ["LoP", "FillMode", "Color"], "Image"],
      "ellipse": ["arrow", ["Number", "Number", "FillMode", "Color"], "Image"],
      "wedge": ["arrow", ["Number", "Number", "FillMode", "Color"], "Image"],
      "triangle": ["arrow", ["Number", "FillMode", "Color"], "Image"],
      "triangle-sas": ["arrow", ["Number", "Number", "Number", "FillMode", "Color"], "Image"],
      "triangle-sss": ["arrow", ["Number", "Number", "Number", "FillMode", "Color"], "Image"],
      "triangle-ass": ["arrow", ["Number", "Number", "Number", "FillMode", "Color"], "Image"],
      "triangle-ssa": ["arrow", ["Number", "Number", "Number", "FillMode", "Color"], "Image"],
      "triangle-aas": ["arrow", ["Number", "Number", "Number", "FillMode", "Color"], "Image"],
      "triangle-asa": ["arrow", ["Number", "Number", "Number", "FillMode", "Color"], "Image"],
      "triangle-saa": ["arrow", ["Number", "Number", "Number", "FillMode", "Color"], "Image"],
      "right-triangle": ["arrow", ["Number", "Number", "FillMode", "Color"], "Image"],
      "isosceles-triangle": ["arrow", ["Number", "Number", "FillMode", "Color"], "Image"],
      "star": ["arrow", ["Number", "FillMode", "Color"], "Image"],
      "star-sized": ["arrow", ["Number", "Number", "Number", "FillMode", "Color"], "Image"],
      "radial-star": ["arrow", ["Number", "Number", "Number", "FillMode", "Color"], "Image"],
      "star-polygon": ["arrow", ["Number", "Number", "Number", "FillMode", "Color"], "Image"],
      "rhombus": ["arrow", ["Number", "Number", "FillMode", "Color"], "Image"],
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
                 name: "Image" },
    }
  },
  theModule: function(runtime, namespace, uri, imageLib, makeImage, jsnums) {
    var ffi = runtime.ffi;

    var image = runtime.getField(imageLib, "internal");
    var colorDb = image.colorDb;

    const checkArity = ffi.checkArity;
    const c = function(name, ...argsAndAnns) {
      runtime.checkArgsInternalInline("image-typed", name, ...argsAndAnns);
    };
    const c1 = function(name, arg, ann) {
      runtime.checkArgsInternal1("image-typed", name, arg, ann);
    };
    const c2 = function(name, arg1, ann1, arg2, ann2) {
      runtime.checkArgsInternal2("image-typed", name, arg1, ann1, arg2, ann2);
    };
    const c3 = function(name, arg1, ann1, arg2, ann2, arg3, ann3) {
      runtime.checkArgsInternal3("image-typed", name, arg1, ann1, arg2, ann2, arg3, ann3);
    };

    var ann = function(name, pred) {
      return runtime.makePrimitiveAnn(name, pred);
    };

    var identity = function(x) { return x; };
    var pyAlwaysTrue = runtime.makeFunction(function(_) { return true; }, "No-op");

    var checkImagePred = function(val) {
      return runtime.isOpaque(val) && image.isImage(val.val);
    };
    var checkScenePred = function(val) {
      return runtime.isOpaque(val) && image.isScene(val.val);
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
    var unwrapListofImage = identity;

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
      unwrapColor: identity,
      annColor: image.annColor,
      annPoint: image.annPoint,
      annMode: image.annFillMode,
      unwrapMode: function(m) {
        return runtime.ffi.cases(pyAlwaysTrue, "FillMode", m, {
          "mode-solid":   function(_) { return "solid"; },
          "mode-outline": function(_) { return "outline"; },
          "mode-fade":    function(v) { return jsnums.toFixnum(v); },
        });
      },
      annFontFamily: image.annFontFamily,
      unwrapFontFamily: function(ff) {
        return runtime.ffi.cases(pyAlwaysTrue, "FontFamily", ff, {
          "ff-default":    function(_) { return "default"; },
          "ff-decorative": function(_) { return "decorative"; },
          "ff-roman":      function(_) { return "roman"; },
          "ff-script":     function(_) { return "script"; },
          "ff-swiss":      function(_) { return "swiss"; },
          "ff-modern":     function(_) { return "modern"; },
          "ff-symbol":     function(_) { return "symbol"; },
          "ff-system":     function(_) { return "system"; },
        });
      },   
      annFontStyle: image.annFontStyle,
      unwrapFontStyle: function(fs) {
        return runtime.ffi.cases(pyAlwaysTrue, "FontStyle", fs, {
          "fs-normal": function(_) { return "normal"; },
          "fs-italic": function(_) { return "italic"; },
          "fs-slant":  function(_) { return "slant"; },
        });
      },
      annFontWeight: image.annFontWeight,
      unwrapFontWeight: function(fw){
        return runtime.ffi.cases(pyAlwaysTrue, "FontWeight", fw, {
          "fw-normal": function(_) { return "normal"; },
          "fw-bold": function(_) { return "bold"; },
          "fw-light": function(_) { return "light"; },
        });
      },
      annPlaceX: image.annXPlace,
      unwrapPlaceX: function(px) {
        return runtime.ffi.cases(pyAlwaysTrue, "XPlace", px, {
          "x-left": function(_) { return "left"; },
          "x-middle": function(_) { return "middle"; },
          "x-pinhole": function(_) { return "pinhole"; },
          "x-right": function(_) { return "right"; }
        });
      },
      annPlaceY: image.annYPlace,
      unwrapPlaceY: function(py) {
        return runtime.ffi.cases(pyAlwaysTrue, "YPlace", py, {
          "y-top": function(_) { return "top"; },
          "y-center": function(_) { return "center"; },
          "y-pinhole": function(_) { return "pinhole"; },
          "y-baseline": function(_) { return "baseline"; },
          "y-bottom": function(_) { return "bottom"; }
        });
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
      annListColor: ann("List<Color>", function(val) {
        if (!runtime.ffi.isList(val)) return false;
        var cur = val;
        var gf = runtime.getField;
        while (runtime.unwrap(ffi.isLink(cur))) {
          var f = gf(cur, "first");
          if (!image.isColor(f)) return false;
          cur = gf(cur, "rest");
        }
        return true;
      }),
      unwrapListofColor: identity,
      annListPoint2D: ann("List<Point>", function(val) {
        if (!runtime.ffi.isList(val)) return false;
        var cur = val;
        var gf = runtime.getField;
        var count = 0;
        while (runtime.unwrap(ffi.isLink(cur))) {
          var f = gf(cur, "first");
          if (!image.isPoint(f)) return false;
          cur = gf(cur, "rest");
          count++;
        }
        return true;
      }),
      annListImage: annListImage,
      unwrapListofImage: unwrapListofImage,
      unwrapListofPoint2D: function(val) {
        return ffi.toArray(val).map(unwrapPoint2D);
      },
      annSideCount: ann("Side Count", image.isSideCount),
      annStepCount: ann("Step Count", image.isStepCount),
      annPointCount: ann("Points Count", image.isPointsCount)
    };


    var values = makeImage.makeImageLib("image-typed", ANNOTS);
    function f(name, fun) {
      values[name] = runtime.makeFunction(fun, name);
    }


    return runtime.makeModuleReturn(values, {
        "Image": image.Image,
        "Scene": runtime.makePrimitiveAnn("Scene", checkScenePred)
      });
  }
})
