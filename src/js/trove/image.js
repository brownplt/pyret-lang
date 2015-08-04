define([
    "./image-lib",
    "js/js-numbers",
    "js/ffi-helpers",
    "js/runtime-util",
    "js/type-util"
  ], function(imageLib, jsnums, ffiLib, util, t) {

  var TImage = t.localType("Image");
  var tscene = t.localType("Scene");
  var numpred = t.arrow([t.number], t.boolean);
  var strpred = t.arrow([t.string], t.boolean);
  var Num = t.number;
  var Str = t.string;
  var Color = Str;
  var Fun = t.arrow
  var TriangleFun = Fun([Num, Num, Num, Str, Color], TImage);

  return util.definePyretModule(
    "image",
    [],
    {
      values: {
        "circle": Fun([Num, Color, Color], TImage),
        "is-image-color": strpred,
        "is-mode": strpred,
        "is-x-place": strpred,
        "is-y-place": strpred,
        "is-angle": numpred,
        "is-side-count": numpred,
        "is-step-count": numpred,
        "is-image": Fun([t.any], t.boolean),
        "bitmap-url": Fun([Str], TImage),
        "open-image-url": Fun([Str], TImage),
        "image-url": Fun([Str], TImage),
        "images-equal": Fun([TImage, TImage], t.boolean),
        "text": Fun([Str, Num, Color], TImage),
        "text-font": Fun([Str, Num, Color, Str, Str, Str, Str, t.boolean], TImage),
        "overlay": Fun([TImage, TImage], TImage),
        "overlay-xy": Fun([TImage, Num, Num, TImage], TImage),
        "overlay-align": Fun([Str, Str, TImage, TImage], TImage),
        "underlay": Fun([TImage, TImage], TImage),
        "underlay-xy": Fun([TImage, Num, Num, TImage], TImage),
        "underlay-align": Fun([Str, Str, TImage, TImage], TImage),
        "beside": Fun([TImage, TImage], TImage),
        "beside-align": Fun([Str, TImage, TImage], TImage),
        "above": Fun([TImage, TImage], TImage),
        "above-align": Fun([Str, TImage, TImage], TImage),
        "empty-scene": Fun([Num, Num], tscene),
        "put-image": Fun([TImage, Num, Num, TImage], TImage),
        "place-image": Fun([TImage, Num, Num, TImage], TImage),
        "place-image-align": Fun([TImage, Num, Num, Str, Str, TImage], TImage),
        "rotate": Fun([Num, TImage], TImage),
        "scale": Fun([Num, TImage], TImage),
        "scale-xy": Fun([Num, Num, TImage], TImage),
        "flip-horizontal": Fun([TImage], TImage),
        "flip-vertical": Fun([TImage], TImage),
        "frame": Fun([TImage], TImage),
        "crop": Fun([Num, Num, Num, Num, TImage], TImage),
        "line": Fun([Num, Num, Color], TImage),
        "add-line": Fun([TImage, Num, Num, Num, Num, Color], TImage),
        "scene-line": Fun([TImage, Num, Num, Num, Num, Color], TImage),
        "square": Fun([Num, Str, Color], TImage),
        "rectangle": Fun([Num, Num, Str, Color], TImage),
        "regular-polygon": Fun([Num, Num, Str, Color], TImage),
        "ellipse": Fun([Num, Num, Str, Color], TImage),
        "triangle": Fun([Num, Str, Color], TImage),
        "triangle-sas": TriangleFun,
        "triangle-sss": TriangleFun,
        "triangle-ass": TriangleFun,
        "triangle-ssa": TriangleFun,
        "triangle-aas": TriangleFun,
        "triangle-asa": TriangleFun,
        "triangle-saa": TriangleFun,
        "right-triangle": Fun([Num, Num, Str, Color], TImage),
        "isosceles-triangle": Fun([Num, Num, Str, Color], TImage),
        "star": Fun([Num, Str, Color], TImage),
        "star-sized": Fun([Num, Num, Num, Str, Color], TImage),
        "radial-star": Fun([Num, Num, Num, Str, Color], TImage),
        "star-polygon": Fun([Num, Num, Num, Str, Color], TImage),
        "rhombus": Fun([Num, Num, Str, Color], TImage),
        "image-to-color-list":
          Fun([TImage], t.tyapp(t.libName("lists", "List"), [Color])),
        "color-list-to-image":
          Fun([t.tyapp(t.libName("lists", "List"), [Color])], TImage),
        "color-list-to-bitmap":
          Fun([t.tyapp(t.libName("lists", "List"), [Color])], TImage),
        "image-width": Fun([TImage], Num),
        "image-height": Fun([TImage], Num),
        "image-baseline": Fun([TImage], Num),
        "name-to-color": Fun([Str], Color)
      },
      datatypes: {
        Image: t.dataType("Image", [], [], {}),
        Scene: t.dataType("Scene", [], [], {})
      },
      aliases: {
        Image: t.localType("Image") 
      }
    },
    function(runtime, namespace) {
      return runtime.loadJSModules(namespace, [imageLib, ffiLib], function(image, ffi) {
        var colorDb = image.colorDb;

        var isString = runtime.isString;

        var isFontFamily = function(x){
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
        };
        var isFontStyle = function(x){
            return (isString(x) &&
              (x.toString().toLowerCase() == "normal" ||
               x.toString().toLowerCase() == "italic" ||
               x.toString().toLowerCase() == "slant"))
          || (x === false);		// false is also acceptable
        };
        var isFontWeight = function(x){
            return (isString(x) &&
              (x.toString().toLowerCase() == "normal" ||
               x.toString().toLowerCase() == "bold" ||
               x.toString().toLowerCase() == "light"))
          || (x === false);		// false is also acceptable
        };
        var isMode = function(x) {
          return (isString(x) &&
                (x.toString().toLowerCase() == "solid" ||
                 x.toString().toLowerCase() == "outline")) ||
        ((jsnums.isReal(x)) &&
         (jsnums.greaterThanOrEqual(x, 0) &&
          jsnums.lessThanOrEqual(x, 255)));
      };

      var isPlaceX = function(x) {
          return (isString(x) &&
            (x.toString().toLowerCase() == "left"  ||
             x.toString().toLowerCase() == "right" ||
             x.toString().toLowerCase() == "center" ||
             x.toString().toLowerCase() == "middle"));
      };

      var isPlaceY = function(x) {
          return (isString(x) &&
            (x.toString().toLowerCase() == "top"	  ||
             x.toString().toLowerCase() == "bottom"   ||
             x.toString().toLowerCase() == "baseline" ||
             x.toString().toLowerCase() == "center"   ||
             x.toString().toLowerCase() == "middle"));
      };

      var isStyle = function(x) {
          return (isString(x) &&
            (x.toString().toLowerCase() == "solid" ||
             x.toString().toLowerCase() == "outline"));
      };

      var less = function(lhs, rhs) {
        return (rhs - lhs) > 0.00001;
      }

      var p = function(pred, name) {
        return function(val) { runtime.makeCheckType(pred, name)(val); return val; }
      }

      var checkString = p(runtime.isString, "String");
      var checkStringOrFalse = p(function(val) { return runtime.isString(val) || runtime.isPyretFalse; }, "String or false");

      var checkByte = p(function(val) {
          return runtime.isNumber(val) && jsnums.greaterThanOrEqual(val, 0) && jsnums.greaterThanOrEqual(255, val);
        }, "Number between 0 and 255");
      var checkReal = p(function(val) {
          return runtime.isNumber(val) && jsnums.isReal(val);
        }, "Real Number");
      var checkBoolean = p(runtime.isBoolean, "Boolean");

      var checkNatural = p(function(val) {
          return runtime.isNumber(val) && jsnums.isInteger(val) && jsnums.greaterThanOrEqual(val, 0);
        }, "Natural Number");

      var checkPositiveInteger = p(function(val) {
          return runtime.isNumber(val) && jsnums.isInteger(val) && jsnums.greaterThanOrEqual(val, 0);
        }, "Positive Integer");

      var checkNonNegativeReal = p(function(val) {
          return runtime.isNumber(val) && jsnums.isReal(val) && jsnums.greaterThanOrEqual(val, 0);
        }, "Non-negative Real Number");


        var _checkColor = p(image.isColorOrColorString, "Color");

        var checkColor = function(val) {
            var aColor = _checkColor(val);
            if (colorDb.get(aColor)) {
              aColor = colorDb.get(aColor);
            }
            return aColor;
        };

        var checkImagePred = function(val) {
          return runtime.isOpaque(val) && image.isImage(val.val);
        };
        var checkImageType = runtime.makeCheckType(checkImagePred, "Image");
        var checkImage = function(val) {
          checkImageType(val);
          return val.val;
        }
        var checkImageOrScenePred = function(val) {
          return runtime.isOpaque(val) && (image.isImage(val.val) || image.isScene(val.val));
        };
        var checkImageOrSceneType = runtime.makeCheckType(checkImageOrScenePred, "Image")
        var checkImageOrScene = function(val) {
          checkImageOrSceneType(val);
          return val.val;
        }

        var checkScenePred = function(val) {
          return runtime.isOpaque(val) && image.isScene(val.val);
        };

        var checkFontFamily = p(isFontFamily, "Font Family");

        var checkFontStyle = p(isFontStyle, "Font Style");

        var checkFontWeight = p(isFontWeight, "Font Weight");

        var checkPlaceX = p(isPlaceX, "X Place");

        var checkPlaceY = p(isPlaceY, "Y Place");


        var checkAngle = p(image.isAngle, "Angle");


        var checkMode = p(isMode, "Mode");

        var checkSideCount = p(image.isSideCount, "Side Count");

        var checkStepCount = p(image.isStepCount, "Step Count");

        var checkPointsCount = p(image.isPointsCount, "Points Count");

        var checkArity = ffi.checkArity;

        var checkListofColor = p(function(val) {
          return ffi.makeList(ffi.toArray(val).map(checkColor));
        }, "List<Color>");

      var checkAngle = p(image.isAngle, "Angle");

      var checkMode = p(isMode, "Mode");

      var checkSideCount = p(image.isSideCount, "Side Count");

      var checkStepCount = p(image.isStepCount, "Step Count");

      var checkPointsCount = p(image.isPointsCount, "Points Count");

      var checkArity = ffi.checkArity;

      var checkListofColor = p(function(val) {
        return ffi.makeList(ffi.toArray(val).map(checkColor));
      }, "List<Color>");

      var throwMessage = ffi.throwMessageException;

      function makeImage(i) {
        return runtime.makeOpaque(i, image.imageEquals);
      }

      // Useful trigonometric functions based on htdp teachpack

      // excess : compute the Euclidean excess
      //  Note: If the excess is 0, then C is 90 deg.
      //        If the excess is negative, then C is obtuse.
      //        If the excess is positive, then C is acuse.
      function excess(sideA, sideB, sideC) {
         return sideA*sideA + sideB*sideB - sideC*sideC;
      }

      // return c^2 = a^2 + b^2 - 2ab cos(C)
      function cosRel(sideA, sideB, angleC) {
          return (sideA*sideA) + (sideB*sideB) - (2*sideA*sideB*Math.cos(angleC * Math.PI/180));
      }

      //////////////////////////////////////////////////////////////////////
      var f = runtime.makeFunction;
      var bitmapURL = f(function(maybeUrl) {
        checkArity(1, arguments, "bitmapURL");
        var url = checkString(maybeUrl);
        runtime.pauseStack(function(restarter) {
          var rawImage = new Image();
          if(runtime.hasParam("imgUrlProxy")) {
            url = runtime.getParam("imgUrlProxy")(url);
          }
          rawImage.onload = function() {
            restarter.resume(makeImage(image.makeFileImage(String(url), rawImage)));
          };
          rawImage.onerror = function(e) {
            restarter.error(runtime.ffi.makeMessageException("unable to load " + url + ": " + e.message));
          };
          rawImage.src = String(url);
        });
      });
      return runtime.makeObject({
        "provide-plus-types": runtime.makeObject({
          types: {
            "Image": runtime.makePrimitiveAnn("Image", checkImagePred),
            "Scene": runtime.makePrimitiveAnn("Scene", checkScenePred)
          },
          values: runtime.makeObject({
            "circle": f(function(maybeRadius, maybeMode, maybeColor) {
              checkArity(3, arguments, "circle");
              var radius = checkNonNegativeReal(maybeRadius);
              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              return makeImage(image.makeCircleImage(jsnums.toFixnum(radius), String(mode), color));
            }),
            "is-image-color": f(function(maybeImage) {
              checkArity(1, arguments, "is-image-color");
              return runtime.wrap(image.isColorOrColorString(maybeImage));
            }),
            "is-mode": f(function(maybeMode) {
              checkArity(1, arguments, "is-mode");
              return runtime.wrap(isMode(maybeMode));
            }),
            "is-x-place": f(function(maybeXPlace) {
              checkArity(1, arguments, "is-x-place");
              return runtime.wrap(isPlaceX(maybeXPlace));
            }),
            "is-y-place": f(function(maybeYPlace) {
              checkArity(1, arguments, "is-y-place");
              return runtime.wrap(isPlaceY(maybeYPlace));
            }),
            "is-angle": f(function(maybeAngle) {
              checkArity(1, arguments, "is-angle");
              return runtime.wrap(image.isAngle(maybeAngle));
            }),
            "is-side-count": f(function(maybeSideCount) {
              checkArity(1, arguments, "is-side-count");
              return runtime.wrap(image.isSideCount(maybeSideCount));
            }),
            "is-step-count": f(function(maybeStepCount) {
              checkArity(1, arguments, "is-step-count");
              return runtime.wrap(image.isStepCount(maybeStepCount));
            }),
            "is-image": f(function(maybeImage) {
              checkArity(1, arguments, "is-image");
              runtime.confirm(maybeImage, runtime.isOpaque);
              return runtime.wrap(image.isImage(maybeImage.val));
            }),
            "bitmap-url": bitmapURL,
            "open-image-url": bitmapURL,
            "image-url": bitmapURL,
            "images-equal": f(function(maybeImage1, maybeImage2) {
              checkArity(2, arguments, "images-equal");
              // TODO: The original version of the image library's equals function passes a union-find datastructure
              // for some reason.  Our runtime.same method doesn't.  Could be a problem...
              var img1 = checkImage(maybeImage1);
              var img2 = checkImage(maybeImage2);
              return runtime.wrap(image.imageEquals(img1, img2));
            }),
            "text": f(function(maybeString, maybeSize, maybeColor) {
              checkArity(3, arguments, "text");
              var string = checkString(maybeString);
              var size = checkPositiveInteger(maybeSize);
              var color = checkColor(maybeColor);
              return makeImage(
                image.makeTextImage(String(string), jsnums.toFixnum(size), color,
                                    "normal", "Optimer", "", "", false));
            }),
            "text-font": f(function(maybeString, maybeSize, maybeColor, maybeFace,
                                    maybeFamily, maybeStyle, maybeWeight, maybeUnderline) {
              checkArity(8, arguments);
              var string = checkString(maybeString);
              var size = checkByte(maybeSize);
              var color = checkColor(maybeColor);
              var face = checkStringOrFalse(maybeFace);
              var family = checkFontFamily(maybeFamily);
              var style = checkFontStyle(maybeStyle);
              var weight = checkFontWeight(maybeWeight);
              var underline = checkBoolean(maybeUnderline);
              return makeImage(
                image.makeTextImage(String(string), jsnums.toFixnum(size), color,
                                    String(face), String(family), String(style),
                                    String(weight), underline));
            }),

            "overlay": f(function(maybeImg1, maybeImg2) {
              checkArity(2, arguments, "overlay");
              var img1 = checkImage(maybeImg1);
              var img2 = checkImage(maybeImg2);
              return makeImage(image.makeOverlayImage(img1, img2, "middle", "middle"));
            }),

            "overlay-xy": f(function(maybeImg1, maybeDx, maybeDy, maybeImg2) {
              checkArity(4, arguments, "overlay-xy");
              var img1 = checkImage(maybeImg1);
              var dx = checkReal(maybeDx);
              var dy = checkReal(maybeDy);
              var img2 = checkImage(maybeImg2);
              return makeImage(
                image.makeOverlayImage(img1, img2, jsnums.toFixnum(dx), jsnums.toFixnum(dy)));
            }),

            "overlay-align": f(function(maybePlaceX, maybePlaceY, maybeImg1, maybeImg2) {
              checkArity(4, arguments, "overlay-align");
              var placeX = checkPlaceX(maybePlaceX);
              var placeY = checkPlaceY(maybePlaceY);
              var img1 = checkImage(maybeImg1);
              var img2 = checkImage(maybeImg2);
              return makeImage(image.makeOverlayImage(img1, img2, String(placeX), String(placeY)));
            }),

            "underlay": f(function(maybeImg1, maybeImg2) {
              checkArity(2, arguments, "underlay");
              var img1 = checkImage(maybeImg1);
              var img2 = checkImage(maybeImg2);
              return makeImage(image.makeOverlayImage(img2, img1, "middle", "middle"));
            }),

            "underlay-xy": f(function(maybeImg1, maybeDx, maybeDy, maybeImg2) {
              checkArity(4, arguments, "underlay-xy");
              var img1 = checkImage(maybeImg1);
              var dx = checkReal(maybeDx);
              var dy = checkReal(maybeDy);
              var img2 = checkImage(maybeImg2);
              return makeImage(
                image.makeOverlayImage(img2, img1, -jsnums.toFixnum(dx), -jsnums.toFixnum(dy)));
            }),

            "underlay-align": f(function(maybePlaceX, maybePlaceY, maybeImg1, maybeImg2) {
              checkArity(4, arguments, "underlay-align");
              var placeX = checkPlaceX(maybePlaceX);
              var placeY = checkPlaceY(maybePlaceY);
              var img1 = checkImage(maybeImg1);
              var img2 = checkImage(maybeImg2);
              return makeImage(image.makeOverlayImage(img2, img1, String(placeX), String(placeY)));
            }),

            "beside": f(function(maybeImg1, maybeImg2) {
              checkArity(2, arguments, "beside");
              var img1 = checkImage(maybeImg1);
              var img2 = checkImage(maybeImg2);
              return makeImage(image.makeOverlayImage(img1, img2, "beside", "middle"));
            }),

            "beside-align": f(function(maybePlaceY, maybeImg1, maybeImg2) {
              checkArity(3, arguments, "beside-align");
              var placeY = checkPlaceY(maybePlaceY);
              var img1 = checkImage(maybeImg1);
              var img2 = checkImage(maybeImg2);
              return makeImage(image.makeOverlayImage(img1, img2, "beside", String(placeY)));
            }),

            "above": f(function(maybeImg1, maybeImg2) {
              checkArity(2, arguments, "above");
              var img1 = checkImage(maybeImg1);
              var img2 = checkImage(maybeImg2);
              return makeImage(image.makeOverlayImage(img1, img2, "middle", "above"));
            }),

            "above-align": f(function(maybePlaceX, maybeImg1, maybeImg2) {
              checkArity(3, arguments, "above-align");
              var placeX = checkPlaceX(maybePlaceX);
              var img1 = checkImage(maybeImg1);
              var img2 = checkImage(maybeImg2);
              return makeImage(image.makeOverlayImage(img1, img2, String(placeX), "above"));
            }),

            "empty-scene": f(function(maybeWidth, maybeHeight) {
              checkArity(2, arguments, "empty-scene");
              var width = checkNonNegativeReal(maybeWidth);
              var height = checkNonNegativeReal(maybeHeight);
              return makeImage(
                image.makeSceneImage(jsnums.toFixnum(width), jsnums.toFixnum(height), [], true));
            }),
            "put-image": f(function(maybePicture, maybeX, maybeY, maybeBackground) {
              checkArity(4, arguments, "put-image");
              var picture = checkImage(maybePicture);
              var x = checkReal(maybeX);
              var y = checkReal(maybeY);
              var background = checkImageOrScene(maybeBackground);
              if (image.isScene(background)) {
                return makeImage(background.add(picture, jsnums.toFixnum(x), background.getHeight() - jsnums.toFixnum(y)));
              } else {
                var newScene = image.makeSceneImage(background.getWidth(), background.getHeight(), [], false);
                newScene = newScene.add(background, background.getWidth()/2, background.getHeight()/2);
                newScene = newScene.add(picture, jsnums.toFixnum(x), background.getHeight() - jsnums.toFixnum(y));
                return makeImage(newScene);
              }
            }),
            "place-image": f(function(maybePicture, maybeX, maybeY, maybeBackground) {
              checkArity(4, arguments, "place-image");
              var picture = checkImage(maybePicture);
              var x = checkReal(maybeX);
              var y = checkReal(maybeY);
              var background = checkImageOrScene(maybeBackground);
              if (image.isScene(background)) {
                return makeImage(background.add(picture, jsnums.toFixnum(x), jsnums.toFixnum(y)));
              } else {
                var newScene = image.makeSceneImage(background.getWidth(), background.getHeight(), [], false);
                newScene = newScene.add(background, background.getWidth()/2, background.getHeight()/2);
                newScene = newScene.add(picture, jsnums.toFixnum(x), jsnums.toFixnum(y));
                return makeImage(newScene);
              }
            }),
            "place-image-align": f(function(maybeImg, maybeX, maybeY, maybePlaceX, maybePlaceY, maybeBackground) {
              checkArity(6, arguments, "place-image-align");
              var img = checkImage(maybeImg);
              var x = checkReal(maybeX);
              var y = checkReal(maybeY);
              var placeX = checkPlaceX(maybePlaceX);
              var placeY = checkPlaceY(maybePlaceY);
              var background = checkImageOrScene(maybeBackground);
              if      (placeX == "left"  ) { x = x + img.getWidth()/2; }
              else if (placeX == "right" ) { x = x - img.getWidth()/2; }
              if      (placeY == "top"   ) { y = y + img.getHeight()/2; }
              else if (placeY == "bottom") { y = y - img.getHeight()/2; }

              if (image.isScene(background)) {
                return makeImage(background.add(img, jsnums.toFixnum(x), jsnums.toFixnum(y)));
              } else {
                var newScene = image.makeSceneImage(background.getWidth(),
                                              background.getHeight(),
                                              [],
                                              false);
                newScene = newScene.add(background, background.getWidth()/2, background.getHeight()/2);
                newScene = newScene.add(img, jsnums.toFixnum(x), jsnums.toFixnum(y));
                return makeImage(newScene);
              }
            }),

            "rotate": f(function(maybeAngle, maybeImg) {
              checkArity(2, arguments, "rotate");
              var angle = checkAngle(maybeAngle);
              var img = checkImage(maybeImg);
              return makeImage(image.makeRotateImage(jsnums.toFixnum(-angle), img));
            }),

            "scale": f(function(maybeFactor, maybeImg) {
              checkArity(2, arguments, "scale");
              var factor = checkReal(maybeFactor);
              var img = checkImage(maybeImg);
              return makeImage(image.makeScaleImage(jsnums.toFixnum(factor), jsnums.toFixnum(factor), img));
            }),

            "scale-xy": f(function(maybeXFactor, maybeYFactor, maybeImg) {
              checkArity(3, arguments, "scale-xy");
              var xFactor = checkReal(maybeXFactor);
              var yFactor = checkReal(maybeYFactor);
              var img = checkImage(maybeImg);
              return makeImage(image.makeScaleImage(jsnums.toFixnum(xFactor), jsnums.toFixnum(yFactor), img));
            }),

            "flip-horizontal": f(function(maybeImg) {
              checkArity(1, arguments, "flip-horizontal");
              var img = checkImage(maybeImg);
              return makeImage(image.makeFlipImage(img, "horizontal"));
            }),

            "flip-vertical": f(function(maybeImg) {
              checkArity(1, arguments, "flip-vertical");
              var img = checkImage(maybeImg);
              return makeImage(image.makeFlipImage(img, "vertical"));
            }),

            "frame": f(function(maybeImg) {
              checkArity(1, arguments, "frame");
              var img = checkImage(maybeImg);
              return makeImage(image.makeFrameImage(img));
            }),

            "crop": f(function(maybeX, maybeY, maybeWidth, maybeHeight, maybeImg) {
              checkArity(5, arguments, "crop");
              var x = checkReal(maybeX);
              var y = checkReal(maybeY);
              var width = checkNonNegativeReal(maybeWidth);
              var height = checkNonNegativeReal(maybeHeight);
              var img = checkImage(maybeImg);
              return makeImage(image.makeCropImage(jsnums.toFixnum(x), jsnums.toFixnum(y),
                                                            jsnums.toFixnum(width), jsnums.toFixnum(height), img));
            }),

            "line": f(function(maybeX, maybeY, maybeC) {
              checkArity(3, arguments, "line");
              var x = checkReal(maybeX);
              var y = checkReal(maybeY);
              var c = checkColor(maybeC);
              return makeImage(
                image.makeLineImage(jsnums.toFixnum(x), jsnums.toFixnum(y), c, true));
            }),

            "add-line": f(function(maybeImg, maybeX1, maybeY1, maybeX2, maybeY2, maybeC) {
              checkArity(6, arguments, "add-line");
              var x1 = checkReal(maybeX1);
              var y1 = checkReal(maybeY1);
              var x2 = checkReal(maybeX2);
              var y2 = checkReal(maybeY2);
              var c = checkColor(maybeC);
              var img = checkImage(maybeImg);
              var line = image.makeLineImage(jsnums.toFixnum(x2 - x1), jsnums.toFixnum(y2 - y1), c, true);
              var leftmost = Math.min(x1, x2);
              var topmost = Math.min(y1, y2);
              return makeImage(image.makeOverlayImage(line, img, -leftmost, -topmost));
            }),

            "scene-line": f(function(maybeImg, maybeX1, maybeY1, maybeX2, maybeY2, maybeC) {
              checkArity(6, arguments, "scene-line");
              var x1 = checkReal(maybeX1);
              var y1 = checkReal(maybeY1);
              var x2 = checkReal(maybeX2);
              var y2 = checkReal(maybeY2);
              var c = checkColor(maybeC);
              var img = checkImage(maybeImg);
              var line = image.makeLineImage(jsnums.toFixnum(x2 - x1), jsnums.toFixnum(y2 - y1), c, true);

              var newScene = image.makeSceneImage(jsnums.toFixnum(img.getWidth()),
                                                  jsnums.toFixnum(img.getHeight()),
                                                  [],
                                                  true);
              newScene = newScene.add(img, img.getWidth()/2, img.getHeight()/2);
              // make an image containing the line
              var line = image.makeLineImage(jsnums.toFixnum(x2-x1),
                                             jsnums.toFixnum(y2-y1),
                                             c,
                                             false),
              leftMost = Math.min(x1,x2),
              topMost = Math.min(y1,y2);
              return makeImage(newScene.add(line, line.getWidth()/2+leftMost, line.getHeight()/2+topMost));
            }),

            "square": f(function(maybeSide, maybeMode, maybeColor) {
              checkArity(3, arguments, "square");
              var side = checkNonNegativeReal(maybeSide);
              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              return makeImage(image.makeSquareImage(jsnums.toFixnum(side), String(mode), color));
            }),

            "rectangle": f(function(maybeWidth, maybeHeight, maybeMode, maybeColor) {
              checkArity(4, arguments, "rectangle");
              var width = checkNonNegativeReal(maybeWidth);
              var height = checkNonNegativeReal(maybeHeight);
              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              return makeImage(
                image.makeRectangleImage(jsnums.toFixnum(width), jsnums.toFixnum(height), String(mode), color));
            }),

            "regular-polygon": f(function(maybeLength, maybeCount, maybeMode, maybeColor) {
              checkArity(4, arguments, "regular-polygon");
              var length = checkNonNegativeReal(maybeLength);
              var count = checkNonNegativeReal(maybeCount);
              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              return makeImage(
                image.makePolygonImage(jsnums.toFixnum(length), jsnums.toFixnum(count), jsnums.toFixnum(1), String(mode), color));
            }),

            "ellipse": f(function(maybeWidth, maybeHeight, maybeMode, maybeColor) {
              checkArity(4, arguments, "ellipse");
              var width = checkNonNegativeReal(maybeWidth);
              var height = checkNonNegativeReal(maybeHeight);
              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              return makeImage(
                image.makeEllipseImage(jsnums.toFixnum(width), jsnums.toFixnum(height), String(mode), color));
            }),

            "triangle": f(function(maybeSide, maybeMode, maybeColor) {
              checkArity(3, arguments, "triangle");
              var side = checkNonNegativeReal(maybeSide);
              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              return makeImage(
                // Angle makes triangle point up
                image.makeTriangleImage(jsnums.toFixnum(side), jsnums.toFixnum(360-60), jsnums.toFixnum(side),
                                        String(mode), color));
            }),

            "triangle-sas": f(function(maybeSideA, maybeAngleB, maybeSideC, maybeMode, maybeColor) {
              checkArity(5, arguments, "triangle-sas");
              var sideA = checkNonNegativeReal(maybeSideA);
              var angleB = checkAngle(maybeAngleB);
              var sideC = checkNonNegativeReal(maybeSideC);

              var sideB2 = cosRel(sideA, sideC, angleB);
              var sideB  = Math.sqrt(sideB2);

              if (sideB2 <= 0) {
                throwMessage("The given side, angle and side will not form a triangle: "
                             + sideA + ", " + angleB + ", " + sideC);
              } else {
                if (less(sideA + sideC, sideB) ||
                    less(sideB + sideC, sideA) ||
                    less(sideA + sideB, sideC)) {
                  throwMessage("The given side, angle and side will not form a triangle: "
                               + sideA + ", " + angleB + ", " + sideC);
                } else {
                  if (less(sideA + sideC, sideB) ||
                      less(sideB + sideC, sideA) ||
                      less(sideA + sideB, sideC)) {
                    throwMessage("The given side, angle and side will not form a triangle: " 
                                 + sideA + ", " + angleB + ", " + sideC);
                  }
                }
              }

              var angleA = Math.acos(excess(sideB, sideC, sideA) / (2 * sideB * sideC)) * (180 / Math.PI);

              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              return makeImage(
                image.makeTriangleImage(jsnums.toFixnum(sideC), jsnums.toFixnum(angleA), jsnums.toFixnum(sideB),
                                        String(mode), color));
            }),

            "triangle-sss": f(function(maybeSideA, maybeSideB, maybeSideC, maybeMode, maybeColor) {
              checkArity(5, arguments, "triangle-sss");
              var sideA = checkNonNegativeReal(maybeSideA);
              var sideB = checkNonNegativeReal(maybeSideB);
              var sideC = checkNonNegativeReal(maybeSideC);
              if (less(sideA + sideB, sideC) ||
                  less(sideC + sideB, sideA) ||
                  less(sideA + sideC, sideB)) {
                throwMessage("The given sides will not form a triangle: "
                             + sideA + ", " + sideB + ", " + sideC);
              }

              var angleA = Math.acos(excess(sideB, sideC, sideA) / (2 * sideB * sideC)) * (180 / Math.PI);

              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              return makeImage(
                image.makeTriangleImage(jsnums.toFixnum(sideC), jsnums.toFixnum(angleA), jsnums.toFixnum(sideB),
                                        String(mode), color));
            }),

            "triangle-ass": f(function(maybeAngleA, maybeSideB, maybeSideC, maybeMode, maybeColor) {
              checkArity(5, arguments, "triangle-ass");
              var angleA = checkAngle(maybeAngleA);
              var sideB = checkNonNegativeReal(maybeSideB);
              var sideC = checkNonNegativeReal(maybeSideC);
              if (less(180, angleA)) {
                throwMessage("The given angle, side and side will not form a triangle: "
                             + angleA + ", " + sideB + ", " + sideC);
              }
              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              return makeImage(
                image.makeTriangleImage(jsnums.toFixnum(sideC), jsnums.toFixnum(angleA), jsnums.toFixnum(sideB),
                                        String(mode), color));
            }),

            "triangle-ssa": f(function(maybeSideA, maybeSideB, maybeAngleC, maybeMode, maybeColor) {
              checkArity(5, arguments, "triangle-ssa");
              var sideA  = checkNonNegativeReal(maybeSideA);
              var sideB  = checkNonNegativeReal(maybeSideB);
              var angleC = checkAngle(maybeAngleC);
              if (less(180, angleC)) {
                throwMessage("The given side, side and angle will not form a triangle: "
                             + sideA + ", " + sideB + ", " + angleC);
              }
              var sideC2 = cosRel(sideA, sideB, angleC);
              var sideC  = Math.sqrt(sideC2);

              if (sideC2 <= 0) {
                throwMessage("The given side, side and angle will not form a triangle: "
                             + sideA + ", " + sideB + ", " + angleC);
              } else {
                if (less(sideA + sideB, sideC) ||
                    less(sideC + sideB, sideA) ||
                    less(sideA + sideC, sideB)) {
                  throwMessage("The given side, side and angle will not form a triangle: "
                               + sideA + ", " + sideB + ", " + angleC);
                }
              }

              var angleA = Math.acos(excess(sideB, sideC, sideA) / (2 * sideB * sideC)) * (180 / Math.PI);

              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              return makeImage(
                image.makeTriangleImage(jsnums.toFixnum(sideC), jsnums.toFixnum(angleA), jsnums.toFixnum(sideB),
                                        String(mode), color));
            }),

            "triangle-aas": f(function(maybeAngleA, maybeAngleB, maybeSideC, maybeMode, maybeColor) {
              checkArity(5, arguments, "triangle-aas");
              var angleA = checkAngle(maybeAngleA);
              var angleB = checkAngle(maybeAngleB);
              var sideC = checkNonNegativeReal(maybeSideC);
              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              var angleC = (180 - angleA - angleB);
              if (less(angleC, 0)) {
                throwMessage("The given angle, angle and side will not form a triangle: "
                             + angleA + ", " + angleB + ", " + sideC);
              }
              var hypotenuse = sideC / (Math.sin(angleC*Math.PI/180))
              var sideB = hypotenuse * Math.sin(angleB*Math.PI/180);
              return makeImage(
                image.makeTriangleImage(jsnums.toFixnum(sideC), jsnums.toFixnum(angleA), jsnums.toFixnum(sideB),
                                        String(mode), color));
            }),

            "triangle-asa": f(function(maybeAngleA, maybeSideB, maybeAngleC, maybeMode, maybeColor) {
              checkArity(5, arguments, "triangle-asa");
              var angleA = checkAngle(maybeAngleA);
              var sideB = checkNonNegativeReal(maybeSideB);
              var angleC = checkAngle(maybeAngleC);
              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              var angleB = (180 - angleA - angleC);
              if (less(angleB, 0)) {
                throwMessage("The given angle, side and angle will not form a triangle: "
                             + angleA + ", " + sideB + ", " + angleC);
              }
              var base = (sideB * Math.sin(angleA*Math.PI/180)) / (Math.sin(angleB*Math.PI/180));
              var sideC = (sideB * Math.sin(angleC*Math.PI/180)) / (Math.sin(angleB*Math.PI/180));
              return makeImage(
                image.makeTriangleImage(jsnums.toFixnum(sideC), jsnums.toFixnum(angleA), jsnums.toFixnum(sideB),
                                        String(mode), color));
            }),

            "triangle-saa": f(function(maybeSideA, maybeAngleB, maybeAngleC, maybeMode, maybeColor) {
              checkArity(5, arguments, "triangle-saa");
              var sideA = checkNonNegativeReal(maybeSideA);
              var angleB = checkAngle(maybeAngleB);
              var angleC = checkAngle(maybeAngleC);
              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              var angleA = (180 - angleC - angleB);
              var hypotenuse = sideA / (Math.sin(angleA*Math.PI/180));
              var sideC = hypotenuse * Math.sin(angleC*Math.PI/180);
              var sideB = hypotenuse * Math.sin(angleB*Math.PI/180);
              return makeImage(
                image.makeTriangleImage(jsnums.toFixnum(sideC), jsnums.toFixnum(angleA), jsnums.toFixnum(sideB),
                                        String(mode), color));
            }),

            "right-triangle": f(function(maybeSide1, maybeSide2, maybeMode, maybeColor) {
              checkArity(4, arguments, "right-triangle");
              var side1 = checkNonNegativeReal(maybeSide1);
              var side2 = checkNonNegativeReal(maybeSide2);
              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              return makeImage(
                // add 180 to make the triangle point up
                image.makeTriangleImage(jsnums.toFixnum(side1), jsnums.toFixnum(360-90), jsnums.toFixnum(side2),
                                       String(mode), color));
            }),

            "isosceles-triangle": f(function(maybeSide, maybeAngleC, maybeMode, maybeColor) {
              checkArity(4, arguments, "isosceles-triangle");
              var side = checkNonNegativeReal(maybeSide);
              var angleC = checkAngle(maybeAngleC);
              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              var angleAB = (180-angleC)/2;
              var base = 2*side*Math.sin((angleC*Math.PI/180)/2);
              return makeImage(
                // add 180 to make the triangle point up
                image.makeTriangleImage(jsnums.toFixnum(base), jsnums.toFixnum(360-angleAB), jsnums.toFixnum(side),
                                        String(mode), color));
            }),

            "star": f(function(maybeSide, maybeMode, maybeColor) {
              checkArity(3, arguments, "star");
              var side = checkNonNegativeReal(maybeSide);
              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              return makeImage(
                image.makePolygonImage(jsnums.toFixnum(side), jsnums.toFixnum(5), jsnums.toFixnum(2),
                                    String(mode), color));
            }),
            // TODO: This was split from the variable-arity case in the original whalesong "star" function
            "star-sized": f(function(maybeSideCount, maybeOuter, maybeInner, maybeMode, maybeColor) {
              checkArity(5, arguments, "star-sized");
              var sideCount = checkSideCount(maybeSideCount);
              var outer = checkNonNegativeReal(maybeOuter);
              var inner = checkNonNegativeReal(maybeInner);
              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              return makeImage(
                image.makeStarImage(jsnums.toFixnum(sideCount), jsnums.toFixnum(inner), jsnums.toFixnum(outer),
                                    String(mode), color));
            }),
            // TODO: Same as star-sized?
            "radial-star": f(function(maybePoints, maybeOuter, maybeInner, maybeMode, maybeColor) {
              checkArity(5, arguments, "radial-star");
              var points = checkPointsCount(maybePoints);
              var outer = checkNonNegativeReal(maybeOuter);
              var inner = checkNonNegativeReal(maybeInner);
              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              return makeImage(
                image.makeStarImage(jsnums.toFixnum(points), jsnums.toFixnum(inner), jsnums.toFixnum(outer),
                                    String(mode), color));
            }),

            "star-polygon": f(function(maybeLength, maybeCount, maybeStep, maybeMode, maybeColor) {
              checkArity(5, arguments, "star-polygon");
              var length = checkNonNegativeReal(maybeLength);
              var count = checkNonNegativeReal(maybeCount);
              var step = checkStepCount(maybeStep);
              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              return makeImage(
                image.makePolygonImage(jsnums.toFixnum(length), jsnums.toFixnum(count), jsnums.toFixnum(step),
                                    String(mode), color));
            }),

            "rhombus": f(function(maybeLength, maybeAngle, maybeMode, maybeColor) {
              checkArity(4, arguments, "rhombus");
              var length = checkNonNegativeReal(maybeLength);
              var angle = checkAngle(maybeAngle); // TODO: This was originally checkNonNegativeReal, seemed like a bug
              var mode = checkMode(maybeMode);
              var color = checkColor(maybeColor);
              return makeImage(
                image.makeRhombusImage(jsnums.toFixnum(length), jsnums.toFixnum(angle), String(mode), color));
            }),

            "image-to-color-list": f(function(maybeImage) {
              checkArity(1, arguments, "image-to-color-list");
              var img = checkImage(maybeImage);
              return image.imageToColorList(img);
            }),

            "color-list-to-image": f(function(maybeList, maybeWidth, maybeHeight, maybePinholeX, maybePinholeY) {
              checkArity(5, arguments, "color-list-to-image");
              var loc = checkListofColor(maybeList);
              var width = checkNatural(maybeWidth);
              var height = checkNatural(maybeHeight);
              var pinholeX = checkNatural(maybePinholeX);
              var pinholeY = checkNatural(maybePinholeY);
              // TODO: why no jsnums.toFixnum here?
              return makeImage(image.colorListToImage(loc, width, height, pinholeX, pinholeY));
            }),

            "color-list-to-bitmap": f(function(maybeList, maybeWidth, maybeHeight) {
              checkArity(3, arguments, "color-list-to-bitmap");
              var loc = checkListofColor(maybeList);
              var width = checkNatural(maybeWidth);
              var height = checkNatural(maybeHeight);
              return makeImage(image.colorListToImage(loc, width, height, 0, 0));
            }),

            "image-width": f(function(maybeImg) {
              checkArity(1, arguments, "image-width");
              var img = checkImage(maybeImg);
              return runtime.wrap(img.getWidth());
            }),

            "image-height": f(function(maybeImg) {
              checkArity(1, arguments, "image-height");
              var img = checkImage(maybeImg);
              return runtime.wrap(img.getHeight());
            }),

              "image-baseline": f(function(maybeImg) {
                checkArity(1, arguments, "image-baseline");
                var img = checkImage(maybeImg);
                return runtime.wrap(img.getBaseline());
              }),

              "name-to-color": f(function(maybeName) {
                checkArity(1, arguments, "name-to-color");
                var name = checkString(maybeName);
                return runtime.wrap(colorDb.get(String(name)) || false);
              })
            }),
          }),
          answer: runtime.namespace.get("nothing")
        });
      });
    }); // end rt/ns fun
});
