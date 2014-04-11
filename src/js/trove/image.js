define([
    "./image-lib",
    "js/js-numbers",
    "js/ffi-helpers"
  ], function(imageLib, jsnums, ffiLib) {

  return function(runtime, namespace) {

    var image = imageLib(runtime, namespace);
    var colorDb = image.colorDb;
    var ffi = ffiLib(runtime, namespace);

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




    var p = function(pred) {
      return function(val) { return runtime.confirm(val, pred); };
    }

    var checkString = p(runtime.isString);
    var checkStringOrFalse = p(function(val) { return runtime.isString(val) || runtime.isPyretFalse; });

    var checkByte = p(function(val) {
        return runtime.isNumber(val) && jsnums.greaterThanOrEqual(val, 0) && jsnums.greaterThanOrEqual(255, val);
      });
    var checkReal = p(function(val) {
        return runtime.isNumber(val) && jsnums.isReal(val);
      });
    var checkBoolean = p(runtime.isBoolean);

    var checkNatural = p(function(val) {
        return runtime.isNumber(val) && jsnums.isExactInteger(val) && jsnums.greaterThanOrEqual(val, 1);
      });

    var checkPositiveInteger = p(function(val) {
        return runtime.isNumber(val) && jsnums.isExactInteger(val) && jsnums.greaterThanOrEqual(val, 0);
      });

    var checkNonNegativeReal = p(function(val) {
        return runtime.isNumber(val) && jsnums.isReal(val) && jsnums.greaterThanOrEqual(val, 0);
      });

    var _checkColor = p(image.isColorOrColorString);

    var checkColor = function(val) {
        var aColor = _checkColor(val);
        if (colorDb.get(aColor)) {
          aColor = colorDb.get(aColor);
        }
        return aColor;
    };

    var checkImage = function(maybeImage) { 
      runtime.confirm(maybeImage, runtime.isOpaque);
      runtime.confirm(maybeImage, function(x) { return image.isImage(x.val); });
      return maybeImage.val;
    };
    var checkImageOrScene = function(maybeImage) {
      runtime.confirm(maybeImage, runtime.isOpaque);
      runtime.confirm(maybeImage, function(x) { return image.isImage(x.val) || image.isScene(x.val); });
      return maybeImage.val;
    };

    var checkFontFamily = p(isFontFamily);

    var checkFontStyle = p(isFontStyle);

    var checkFontWeight = p(isFontWeight);

    var checkPlaceX = p(isPlaceX);

    var checkPlaceY = p(isPlaceY);


    var checkAngle = p(image.isAngle);


    var checkMode = p(isMode);

    var checkSideCount = p(image.isSideCount);

    var checkStepCount = p(image.isStepCount);

    var checkPointsCount = p(image.isPointsCount);


    var checkListofColor = p(function(val) {
      return ffi.makeList(ffi.toArray(val).map(p(isColor)));
    });

    var checkArity = function(expected, args) {
      if (expected !== args.length) {
        throw runtime.ffi.throwArityErrorC(["image"], expected, args);
      }
    }

    function makeImage(i) {
      return runtime.makeOpaque(i, image.imageEquals);
    }


    //////////////////////////////////////////////////////////////////////
    var f = runtime.makeFunction;
    var bitmapURL = f(function(maybeUrl) {
      checkArity(1, arguments);
      var url = checkString(maybeUrl);
      runtime.pauseStack(function(restarter) {
        var rawImage = new Image();
        rawImage.onload = function() {
          restarter.resume(makeImage(image.makeFileImage(String(url), rawImage)));
        };
        rawImage.onerror = function(e) {
          restarter.error(runtime.makeMessageException("unable to load " + url + ": " + e.message));
        };
        rawImage.src = String(url);
      });
    });
    return runtime.makeObject({
      provide: runtime.makeObject({
        "circle": f(function(maybeRadius, maybeMode, maybeColor) {
          checkArity(3, arguments);
          var radius = checkNonNegativeReal(maybeRadius);
          var mode = checkMode(maybeMode);
          var color = checkColor(maybeColor);
          return makeImage(image.makeCircleImage(jsnums.toFixnum(radius), String(mode), color));
        }),
        "is-image-color": f(function(maybeImage) {
          checkArity(1, arguments);
          return runtime.wrap(image.isColorOrColorString(maybeImage));
        }),
        "is-mode": f(function(maybeMode) {
          checkArity(1, arguments);
          return runtime.wrap(isMode(maybeMode));
        }),
        "is-x-place": f(function(maybeXPlace) {
          checkArity(1, arguments);
          return runtime.wrap(isPlaceX(maybeXPlace));
        }),
        "is-y-place": f(function(maybeYPlace) {
          checkArity(1, arguments);
          return runtime.wrap(isPlaceY(maybeYPlace));
        }),
        "is-angle": f(function(maybeAngle) {
          checkArity(1, arguments);
          return runtime.wrap(image.isAngle(maybeAngle));
        }),
        "is-side-count": f(function(maybeSideCount) {
          checkArity(1, arguments);
          return runtime.wrap(image.isSideCount(maybeSideCount));
        }),
        "is-step-count": f(function(maybeStepCount) {
          checkArity(1, arguments);
          return runtime.wrap(image.isStepCount(maybeStepCount));
        }),        
        "is-image": f(function(maybeImage) {
          checkArity(1, arguments);
          runtime.confirm(maybeImage, runtime.isOpaque);
          return runtime.wrap(image.isImage(maybeImage.val));
        }),
        "bitmap-url": bitmapURL,
        "open-image-url": bitmapURL,
        "image-url": bitmapURL,
        "image-equals": f(function(maybeImage1, maybeImage2) {
          checkArity(2, arguments);
          // TODO: The original version of the image library's equals function passes a union-find datastructure
          // for some reason.  Our runtime.same method doesn't.  Could be a problem...
          var img1 = checkImage(maybeImage1);
          var img2 = checkImage(maybeImage2);
          return runtime.wrap(image.imageEquals(img1, img2));
        }),
        "text": f(function(maybeString, maybeSize, maybeColor) {
          checkArity(3, arguments);
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
          checkArity(2, arguments);
          var img1 = checkImage(maybeImg1);
          var img2 = checkImage(maybeImg2);
          return makeImage(image.makeOverlayImage(img1, img2, "middle", "middle"));
        }),

        "overlay-xy": f(function(maybeImg1, maybeDx, maybeDy, maybeImg2) {
          checkArity(4, arguments);
          var img1 = checkImage(maybeImg1);
          var dx = checkReal(maybeDx);
          var dy = checkReal(maybeDy);
          var img2 = checkImage(maybeImg2);
          return makeImage(
            image.makeOverlayImage(img1, img2, jsnums.toFixnum(dx), jsnums.toFixnum(dy)));
        }),

        "overlay-align": f(function(maybePlaceX, maybePlaceY, maybeImg1, maybeImg2) {
          checkArity(4, arguments);
          var placeX = checkPlaceX(maybePlaceX);
          var placeY = checkPlaceY(maybePlaceY);
          var img1 = checkImage(maybeImg1);
          var img2 = checkImage(maybeImg2);
          return makeImage(image.makeOverlayImage(img1, img2, String(placeX), String(placeY)));
        }),

        "underlay": f(function(maybeImg1, maybeImg2) {
          checkArity(2, arguments);
          var img1 = checkImage(maybeImg1);
          var img2 = checkImage(maybeImg2);
          return makeImage(image.makeOverlayImage(img2, img1, "middle", "middle"));
        }),

        "underlay-xy": f(function(maybeImg1, maybeDx, maybeDy, maybeImg2) {
          checkArity(4, arguments);
          var img1 = checkImage(maybeImg1);
          var dx = checkReal(maybeDx);
          var dy = checkReal(maybeDy);
          var img2 = checkImage(maybeImg2);
          return makeImage(
            image.makeOverlayImage(img2, img1, jsnums.toFixnum(dx), jsnums.toFixnum(dy)));
        }),

        "underlay-align": f(function(maybePlaceX, maybePlaceY, maybeImg1, maybeImg2) {
          checkArity(4, arguments);
          var placeX = checkPlaceX(maybePlaceX);
          var placeY = checkPlaceY(maybePlaceY);
          var img1 = checkImage(maybeImg1);
          var img2 = checkImage(maybeImg2);
          return makeImage(image.makeOverlayImage(img2, img1, String(placeX), String(placeY)));
        }),

        "beside": f(function(maybeImg1, maybeImg2) {
          checkArity(2, arguments);
          var img1 = checkImage(maybeImg1);
          var img2 = checkImage(maybeImg2);
          return makeImage(image.makeOverlayImage(img1, img2, "beside", "middle"));
        }),

        "beside-align": f(function(maybePlaceY, maybeImg1, maybeImg2) {
          checkArity(3, arguments);
          var placeY = checkPlaceY(maybePlaceY);
          var img1 = checkImage(maybeImg1);
          var img2 = checkImage(maybeImg2);
          return makeImage(image.makeOverlayImage(img1, img2, "beside", String(placeY)));
        }),

        "above": f(function(maybeImg1, maybeImg2) {
          checkArity(2, arguments);
          var img1 = checkImage(maybeImg1);
          var img2 = checkImage(maybeImg2);
          return makeImage(image.makeOverlayImage(img1, img2, "middle", "above"));
        }),

        "above-align": f(function(maybePlaceX, maybeImg1, maybeImg2) {
          checkArity(3, arguments);
          var placeX = checkPlaceX(maybePlaceX);
          var img1 = checkImage(maybeImg1);
          var img2 = checkImage(maybeImg2);
          return makeImage(image.makeOverlayImage(img1, img2, String(placeX), "above"));
        }),

        "empty-scene": f(function(maybeWidth, maybeHeight) {
          checkArity(2, arguments);
          var width = checkNonNegativeReal(maybeWidth);
          var height = checkNonNegativeReal(maybeHeight);
          return makeImage(
            image.makeSceneImage(jsnums.toFixnum(width), jsnums.toFixnum(height), [], true));
        }),
        "put-image": f(function(maybePicture, maybeX, maybeY, maybeBackground) {
          checkArity(4, arguments);
          var picture = checkImage(maybePicture);
          var x = checkReal(maybeX);
          var y = checkReal(maybeY);
          var background = checkImageOrScene(maybeBackground);
          if (image.isScene(background)) {
            return makeImage(background.add(picture, jsnums.toFixnum(x), jsnums.toFixnum(y)));
          } else {
            var newScene = makeSceneImage(background.getWidth(), background.getHeight(), [], false);
            newScene = newScene.add(background, background.getWidth()/2, background.getHeight()/2);
            newScene = newScene.add(picture, jsnums.toFixnum(x), background.getHeight() - jsnums.toFixnum(y));
            return makeImage(newScene);
          }
        }),
        "place-image": f(function(maybePicture, maybeX, maybeY, maybeBackground) {
          checkArity(4, arguments);
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
          checkArity(6, arguments);
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
            var newScene = makeSceneImage(background.getWidth(),
                                          background.getHeight(),
                                          [], 
                                          false);
            newScene = newScene.add(background, background.getWidth()/2, background.getHeight()/2);
            newScene = newScene.add(img, jsnums.toFixnum(x), jsnums.toFixnum(y));
            return makeImage(newScene);
          }
        }),

        "rotate": f(function(maybeAngle, maybeImg) {
          checkArity(2, arguments);
          var angle = checkAngle(maybeAngle);
          var img = checkImage(maybeImg);
          return makeImage(image.makeRotateImage(jsnums.toFixnum(-angle), img));
        }),

        "scale": f(function(maybeFactor, maybeImg) {
          checkArity(2, arguments);
          var factor = checkReal(maybeFactor);
          var img = checkImage(maybeImg);
          return makeImage(image.makeScaleImage(jsnums.toFixnum(factor), jsnums.toFixnum(factor), img));
        }),
          
        "scale-xy": f(function(maybeXFactor, maybeYFactor, maybeImg) {
          checkArity(3, arguments);
          var xFactor = checkReal(maybeXFactor);
          var yFactor = checkReal(maybeYFactor);
          var img = checkImage(maybeImg);
          return makeImage(image.makeScaleImage(jsnums.toFixnum(xFactor), jsnums.toFixnum(yFactor), img));
        }),

        "flip-horizontal": f(function(maybeImg) {
          checkArity(1, arguments);
          var img = checkImage(maybeImg);
          return makeImage(image.makeFlipImage(img, "horizontal"));
        }),

        "flip-vertical": f(function(maybeImg) {
          checkArity(1, arguments);
          var img = checkImage(maybeImg);
          return makeImage(image.makeFlipImage(img, "vertical"));
        }),

        "frame": f(function(maybeImg) {
          checkArity(1, arguments);
          var img = checkImage(maybeImg);
          return makeImage(image.makeFrameImage(img));
        }),
        
        "crop": f(function(maybeX, maybeY, maybeWidth, maybeHeight, maybeImg) {
          checkArity(5, arguments);
          var x = checkReal(maybeX);
          var y = checkReal(maybeY);
          var width = checkNonNegativeReal(maybeWidth);
          var height = checkNonNegativeReal(maybeHeight);
          var img = checkImage(maybeImg);
          return makeImage(image.makeCropImage(jsnums.toFixnum(x), jsnums.toFixnum(y),
                                                        jsnums.toFixnum(width), jsnums.toFixnum(height), img));
        }),

        "line": f(function(maybeX, maybeY, maybeC) {
          checkArity(3, arguments);
          var x = checkReal(maybeX);
          var y = checkReal(maybeY);
          var c = checkColor(maybeC);
          return makeImage(
            image.makeLineImage(jsnums.toFixnum(x), jsnums.toFixnum(y), c, true));
        }),

        "add-line": f(function(maybeImg, maybeX1, maybeY1, maybeX2, maybeY2, maybeC) {
          checkArity(6, arguments);
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
          checkArity(6, arguments);
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
          checkArity(3, arguments);
          var side = checkNonNegativeReal(maybeSide);
          var mode = checkMode(maybeMode);
          var color = checkColor(maybeColor);
          return makeImage(image.makeSquareImage(jsnums.toFixnum(side), String(mode), color));
        }),

        "rectangle": f(function(maybeWidth, maybeHeight, maybeMode, maybeColor) {
          checkArity(4, arguments);
          var width = checkNonNegativeReal(maybeWidth);
          var height = checkNonNegativeReal(maybeHeight);
          var mode = checkMode(maybeMode);
          var color = checkColor(maybeColor);
          return makeImage(
            image.makeRectangleImage(jsnums.toFixnum(width), jsnums.toFixnum(height), String(mode), color));
        }),

        "regular-polygon": f(function(maybeLength, maybeCount, maybeMode, maybeColor) {
          checkArity(4, arguments);
          var length = checkNonNegativeReal(maybeLength);
          var count = checkNonNegativeReal(maybeCount);
          var mode = checkMode(maybeMode);
          var color = checkColor(maybeColor);
          return makeImage(
            image.makePolygonImage(jsnums.toFixnum(length), jsnums.toFixnum(count), String(mode), color));
        }),

        "ellipse": f(function(maybeWidth, maybeHeight, maybeMode, maybeColor) {
          checkArity(4, arguments);
          var width = checkNonNegativeReal(maybeWidth);
          var height = checkNonNegativeReal(maybeHeight);
          var mode = checkMode(maybeMode);
          var color = checkColor(maybeColor);
          return makeImage(
            image.makeEllipseImage(jsnums.toFixnum(width), jsnums.toFixnum(height), String(mode), color));
        }),

        "triangle": f(function(maybeSide, maybeMode, maybeColor) {
          checkArity(3, arguments);
          var side = checkNonNegativeReal(maybeSide);
          var mode = checkMode(maybeMode);
          var color = checkColor(maybeColor);
          return makeImage(
            // Angle makes triangle point up
            image.makeTriangleImage(jsnums.toFixnum(side), jsnums.toFixnum(60+180), jsnums.toFixnum(side),
                                    String(mode), color));
        }),

        "triangle-sas": f(function(maybeBase, maybeAngleC, maybeSideB, maybeMode, maybeColor) {
          checkArity(5, arguments);
          var base = checkNonNegativeReal(maybeBase);
          var angleC = checkAngle(maybeAngleC);
          var sideB = checkNonNegativeReal(maybeSideB);
          var mode = checkMode(maybeMode);
          var color = checkColor(maybeColor);
          if (colorDb.get(color)) { color = colorDb.get(color); }
          return makeImage(
            image.makeTriangleImage(jsnums.toFixnum(base), jsnums.toFixnum(angleC), jsnums.toFixnum(sideB), 
                                    String(mode), color));
        }),

        "triangle-sss": f(function(maybeBase, maybeSideB, maybeSideC, maybeMode, maybeColor) {
          checkArity(5, arguments);
          var base = checkNonNegativeReal(maybeBase);
          var sideB = checkNonNegativeReal(maybeSideB);
          var sideC = checkNonNegativeReal(maybeSideC);
          var mode = checkMode(maybeMode);
          var color = checkColor(maybeColor);
          if (colorDb.get(color)) { color = colorDb.get(color); }
          var angleC = (Math.acos((base*base + sideB*sideB - sideC*sideC) / (2*base*sideB)))*180/Math.PI;
          return makeImage(
            image.makeTriangleImage(jsnums.toFixnum(base), jsnums.toFixnum(angleC), jsnums.toFixnum(sideB), 
                                    String(mode), color));
        }),

        "triangle-ass": f(function(maybeAngle, maybeBase, maybeSideB, maybeMode, maybeColor) {
          checkArity(5, arguments);
          var angle = checkAngle(maybeAngle);
          var base = checkNonNegativeReal(maybeBase);
          var sideB = checkNonNegativeReal(maybeSideB);
          var mode = checkMode(maybeMode);
          var color = checkColor(maybeColor);
          if (colorDb.get(color)) { color = colorDb.get(color); }
          return makeImage(
            image.makeTriangleImage(jsnums.toFixnum(base), jsnums.toFixnum(angle), jsnums.toFixnum(sideB), 
                                    String(mode), color));
        }),

        "triangle-ssa": f(function(maybeBase, maybeSideB, maybeAngleA, maybeMode, maybeColor) {
          checkArity(5, arguments);
          var base = checkNonNegativeReal(maybeBase);
          var sideB = checkNonNegativeReal(maybeSideB);
          var angleA = checkAngle(maybeAngleA);
          var mode = checkMode(maybeMode);
          var color = checkColor(maybeColor);
          if (colorDb.get(color)) { color = colorDb.get(color); }
          var angleB = Math.asin(Math.sin(angleA*Math.PI/180)*sideB/base)*180/Math.PI;
          var angleC = (180 - angleA - angleB);
          return makeImage(
            image.makeTriangleImage(jsnums.toFixnum(base), jsnums.toFixnum(angleC), jsnums.toFixnum(sideB), 
                                    String(mode), color));
        }),

        "triangle-aas": f(function(maybeAngleA, maybeAngleB, maybeBase, maybeMode, maybeColor) {
          checkArity(5, arguments);
          var angleA = checkAngle(maybeAngleA);
          var angleB = checkAngle(maybeAngleB);
          var base = checkNonNegativeReal(maybeBase);
          var mode = checkMode(maybeMode);
          var color = checkColor(maybeColor);
          if (colorDb.get(color)) { color = colorDb.get(color); }
          var angleC = (180 - angleA - angleB);
          var sideB = (base * Math.sin(angleB*Math.PI/180)) / (Math.sin(angleC*Math.PI/180));
          return makeImage(
            image.makeTriangleImage(jsnums.toFixnum(base), jsnums.toFixnum(angleC), jsnums.toFixnum(sideB), 
                                    String(mode), color));
        }),

        "triangle-asa": f(function(maybeAngleA, maybeSideC, maybeAngleB, maybeMode, maybeColor) {
          checkArity(5, arguments);
          var angleA = checkAngle(maybeAngleA);
          var sideC = checkNonNegativeReal(maybeSideC);
          var angleB = checkAngle(maybeAngleB);
          var mode = checkMode(maybeMode);
          var color = checkColor(maybeColor);
          if (colorDb.get(color)) { color = colorDb.get(color); }
          var angleC = (180 - angleA - angleB);
          var base = (sideC * Math.sin(angleA*Math.PI/180)) / (Math.sin(angleC*Math.PI/180));
          var sideB = (sideC * Math.sin(angleB*Math.PI/180)) / (Math.sin(angleC*Math.PI/180));
          return makeImage(
            image.makeTriangleImage(jsnums.toFixnum(base), jsnums.toFixnum(angleC), jsnums.toFixnum(sideB), 
                                    String(mode), color));
        }),

        "triangle-saa": f(function(maybeBase, maybeAngleC, maybeAngleA, maybeMode, maybeColor) {
          checkArity(5, arguments);
          var base = checkNonNegativeReal(maybeBase);
          var angleC = checkAngle(maybeAngleC);
          var angleA = checkAngle(maybeAngleA);
          var mode = checkMode(maybeMode);
          var color = checkColor(maybeColor);
          if (colorDb.get(color)) { color = colorDb.get(color); }
          var angleB = (180 - angleA - angleC);
          var sideB = (base * Math.sin(angleB*Math.PI/180)) / (Math.sin(angleA*Math.PI/180));
          return makeImage(
            image.makeTriangleImage(jsnums.toFixnum(base), jsnums.toFixnum(angleC), jsnums.toFixnum(sideB), 
                                    String(mode), color));
        }),

        "right-triangle": f(function(maybeSide1, maybeSide2, maybeMode, maybeColor) {
          checkArity(4, arguments);
          var side1 = checkNonNegativeReal(maybeSide1);
          var side2 = checkNonNegativeReal(maybeSide2);
          var mode = checkMode(maybeMode);
          var color = checkColor(maybeColor);
          return makeImage(
            // add 180 to make the triangle point up
            image.makeTriangleImage(jsnums.toFixnum(side1), jsnums.toFixnum(90+180), jsnums.toFixnum(side2),
                                   String(mode), color));
        }),

        "isosceles-triangle": f(function(maybeSide, maybeAngleC, maybeMode, maybeColor) {
          checkArity(4, arguments);
          var side = checkNonNegativeReal(maybeSide);
          var angleC = checkAngle(maybeAngleC);
          var mode = checkMode(maybeMode);
          var color = checkColor(maybeColor);
          var angleAB = (180-angleC)/2;
          var base = 2*side*Math.sin((angleC*Math.PI/180)/2);
          return makeImage(
            // add 180 to make the triangle point up
            image.makeTriangleImage(jsnums.toFixnum(base), jsnums.toFixnum(angleAB+180), jsnums.toFixnum(side), 
                                    String(mode), color));
        }),

        "star": f(function(maybeSide, maybeMode, maybeColor) {
          checkArity(3, arguments);
          var side = checkNonNegativeReal(maybeSide);
          var mode = checkMode(maybeMode);
          var color = checkColor(maybeColor);
          return makeImage(
            image.makePolygonImage(jsnums.toFixnum(side), jsnums.toFixnum(5), jsnums.toFixnum(2),
                                String(mode), color));
        }),
        // TODO: This was split from the variable-arity case in the original whalesong "star" function
        "star-sized": f(function(maybeSideCount, maybeOuter, maybeInner, maybeMode, maybeColor) {
          checkArity(5, arguments);
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
          checkArity(5, arguments);
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
          checkArity(5, arguments);
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
          checkArity(4, arguments);
          var length = checkNonNegativeReal(maybeLength);
          var angle = checkAngle(maybeAngle); // TODO: This was originally checkNonNegativeReal, seemed like a bug
          var mode = checkMode(maybeMode);
          var color = checkColor(maybeColor);
          return makeImage(
            image.makeRhombusImage(jsnums.toFixnum(length), jsnums.toFixnum(angle), String(mode), color));
        }),

        "image-to-color-list": f(function(maybeImage) {
          checkArity(1, arguments);
          var img = checkImage(maybeImage);
          return makeImage(image.imageToColorList(img));
        }),

        "color-list-to-image": f(function(maybeList, maybeWidth, maybeHeight, maybePinholeX, maybePinholeY) {
          checkArity(5, arguments);
          var loc = checkListofColor(maybeList);
          var width = checkNatural(maybeWidth);
          var height = checkNatural(maybeHeight);
          var pinholeX = checkNatural(maybePinholeX);
          var pinholeY = checkNatural(maybePinholeY);
          // TODO: why no jsnums.toFixnum here?
          return makeImage(image.colorListToImage(loc, width, height, pinholeX, pinholeY));
        }),

        "color-list-to-bitmap": f(function(maybeList, maybeWidth, maybeHeight) {
          checkArity(3, arguments);
          var loc = checkListofColor(maybeList);
          var width = checkNatural(maybeWidth);
          var height = checkNatural(maybeHeight);
          return makeImage(image.colorListToImage(loc, width, height, 0, 0));
        }),
        
        "image-width": f(function(maybeImg) {
          checkArity(1, arguments);
          var img = checkImage(maybeImg);
          return runtime.wrap(img.getWidth());
        }),

        "image-height": f(function(maybeImg) {
          checkArity(1, arguments);
          var img = checkImage(maybeImg);
          return runtime.wrap(img.getHeight());
        }),

        "image-baseline": f(function(maybeImg) {
          checkArity(1, arguments);
          var img = checkImage(maybeImg);
          return runtime.wrap(img.getBaseline());
        }),

        "name-to-color": f(function(maybeName) {
          checkArity(1, arguments);
          var name = checkString(maybeName);
          return runtime.wrap(colorDb.get(String(name)) || false);
        })
      }),
      answer: runtime.namespace.get("nothing")
    });
  }; // end rt/ns fun
});
