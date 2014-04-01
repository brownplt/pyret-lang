define([
    "./image-lib",
    "../../../lib/js-numbers/src/js-numbers",
    "js/ffi-helpers"
  ], function(imageLib, jsnums, ffiLib) {

  return function(runtime, namespace) {

    var image = imageLib(runtime, namespace);
    var colorDb = image.colorDb;
    var ffi = ffiLib(runtime, namespace);

    //var PAUSE = plt.runtime.PAUSE;


    var isString = runtime.isString;
    var isEqual = runtime.same;

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

    var checkImage = p(image.isImage);
    var checkImageOrScene = p(function(x) {
        return image.isImage(x) || image.isScene(x);
      });

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


    //////////////////////////////////////////////////////////////////////
    var f = runtime.makeFunction;
/*

    EXPORTS['circle'] = 
        makePrimitiveProcedure(
            'circle',
            3,
            function(MACHINE) {
                var aRadius = checkNonNegativeReal(MACHINE, "circle", 0);
          var aMode = checkMode(MACHINE, "circle", 1);
          var aColor = checkColor(MACHINE, "circle", 2);
          return makeCircleImage(jsnums.toFixnum(aRadius), aMode.toString(), aColor);
            });
    EXPORTS['image-color?'] =
        makePrimitiveProcedure(
            'image-color?',
            1,
            function(MACHINE) {
                var elt = MACHINE.e[MACHINE.e.length - 1];
                return (isColorOrColorString(elt));
            });
      
    EXPORTS['mode?'] = 
        makePrimitiveProcedure(
            'mode?',
            1,
            function(MACHINE) {
                return isMode(MACHINE.e[MACHINE.e.length - 1]);
            });
    EXPORTS['image?'] = 
        makePrimitiveProcedure(
            'image?',
                1,
            function(MACHINE) {
                return isImage(MACHINE.e[MACHINE.e.length - 1]);
            });


*/
    return runtime.makeObject({
      provide: runtime.makeObject({
        circle: f(function(radius, mode, color) {
          var r = checkNonNegativeReal(radius);
          var m = checkMode(mode);
          var c = checkColor(color);
          return runtime.makeOpaque(image.makeCircleImage(jsnums.toFixnum(r), String(m), c));
        }),
        "is-image-color": f(function(maybeImage) {
          return runtime.wrap(image.isColorOrColorString(maybeImage));
        }),
        "is-mode": f(function(maybeMode) {
          return runtime.wrap(isMode(maybeMode));
        }),
        "is-x-place": f(function(maybeXPlace) {
          return runtime.wrap(isPlaceX(maybeXPlace));
        }),
        "is-y-place": f(function(maybeYPlace) {
          return runtime.wrap(isPlaceY(maybeYPlace));
        }),
        "is-angle": f(function(maybeAngle) {
          return runtime.wrap(image.isAngle(maybeAngle));
        }),
        "is-side-count": f(function(maybeSideCount) {
          return runtime.wrap(image.isSideCount(maybeSideCount));
        }),
        "is-step-count": f(function(maybeStepCount) {
          return runtime.wrap(image.isStepCount(maybeStepCount));
        }),        
        "is-image": f(function(maybeImage) {
          return runtime.wrap(image.isImage(maybeImage));
        }),
        "bitmap-url": f(function(url) {
          var urlString = checkString(url);
          runtime.pauseStack(function(restarter) {
            var rawImage = new Image();
            rawImage.onload = function() {
              restarter.resume(runtime.makeOpaque(image.makeFileImage(url.toString(), rawImage)));
            };
            rawImage.onerror = function(e) {
              restarter.error(runtime.makeMessageException("unable to load " + url + ": " + e.message));
            };
            rawImage.src = url.toString();
          });
        })
      })
    });
/*



    EXPORTS['image=?'] =
        makePrimitiveProcedure(
           'image=?',
           2,
           function(MACHINE) {
             var img1 = checkImage(MACHINE, 'image=?', 0);
             var img2 = checkImage(MACHINE, 'image=?', 1);
             return isEqual(img1, img2);
           });


    EXPORTS['text'] =
        makePrimitiveProcedure(
            'text',
            3,
            function(MACHINE) {
          var aString = checkString(MACHINE,'text', 0);
          // Unlike 2htdp, we'll allow this to be a positive integer
          var aSize = checkPositiveInteger(MACHINE, 'text', 1); 
          var aColor = checkColor(MACHINE, 'text', 2);
          return makeTextImage(aString.toString(), 
                                     jsnums.toFixnum(aSize),
                                     aColor,
                                     "normal",
                                     "Optimer",
                                     "",
                                     "",
                                     false);
            });


    EXPORTS['text/font'] = 
        makePrimitiveProcedure(
            'text/font',
            8,
            function(MACHINE) {
                var aString = checkString(MACHINE, "text/font", 0);
          var aSize = checkByte(MACHINE, "text/font", 1);
          var aColor = checkColor(MACHINE, "text/font", 2);
          var aFace = checkStringOrFalse(MACHINE, "text/font", 3);
          var aFamily = checkFontFamily(MACHINE, "text/font", 4);
          var aStyle = checkFontStyle(MACHINE, "text/font", 5);
          var aWeight = checkFontWeight(MACHINE, "text/font", 6);
          var aUnderline = checkBoolean(MACHINE, "text/font", 7);
          return makeTextImage(aString.toString(),
                               jsnums.toFixnum(aSize),
                               aColor,
                               aFace.toString(),
                               aFamily.toString(),
                               aStyle.toString(),
                               aWeight.toString(),
                               aUnderline);
            });


    EXPORTS['bitmap/url'] = 
        makeClosure(
            'bitmap/url',
            1,
            function(MACHINE) {
                var url = checkString(MACHINE, 'bitmap/url', 0);
                PAUSE(
                    function(restart) {
                        var rawImage = new Image();
                        rawImage.onload = function() {
                            restart(function(MACHINE) {
                                finalizeClosureCall(
                                    MACHINE, 
                                    makeFileImage(url.toString(),
                                                  rawImage));
                            });
                        };
                        rawImage.onerror = function(e) {
                            restart(function(MACHINE) {
                                plt.baselib.exceptions.raiseFailure(
                                    MACHINE, 
                                    plt.baselib.format.format(
                                        "unable to load ~a: ~a",
                                        [url,
                                         e.message]));
                            });
                        }
                        rawImage.src = url.toString();
                    }
                );
            });

    EXPORTS['open-image-url'] = 
        plt.baselib.functions.renameProcedure(EXPORTS['bitmap/url'],
                                              'open-image-url');

    EXPORTS['image-url'] = 
        plt.baselib.functions.renameProcedure(EXPORTS['bitmap/url'],
                                              'image-url');




    EXPORTS['overlay'] = 
        makePrimitiveProcedure(
            'overlay',
            plt.baselib.arity.makeArityAtLeast(2),
            function(MACHINE) {
          var img1 = checkImage(MACHINE, "overlay", 0);
          var img2 = checkImage(MACHINE, "overlay", 1);
          var restImages = [];
          for (var i = 2; i < MACHINE.a; i++) {
              restImages.push(checkImage(MACHINE, "overlay", i));
          }
                
          var img = makeOverlayImage(img1, img2, "middle", "middle");
          for (var i = 0; i < restImages.length; i++) {
              img = makeOverlayImage(img, restImages[i], "middle", "middle");
          }
          return img;
      });



    EXPORTS['overlay/xy'] = 
        makePrimitiveProcedure(
            'overlay/xy',
            4,
            function(MACHINE) {
          var img1 = checkImage(MACHINE, "overlay/xy", 0);
          var deltaX = checkReal(MACHINE, "overlay/xy", 1);
          var deltaY = checkReal(MACHINE, "overlay/xy", 2);
          var img2 = checkImage(MACHINE, "overlay/xy", 3);
          return makeOverlayImage(img1,
                                  img2,
                                  jsnums.toFixnum(deltaX),
                                  jsnums.toFixnum(deltaY));
            });



     EXPORTS['overlay/align'] = 
         makePrimitiveProcedure(
             'overlay/align',
             plt.baselib.arity.makeArityAtLeast(4),
             function(MACHINE) {
           var placeX = checkPlaceX(MACHINE, "overlay/align", 0);
           var placeY = checkPlaceY(MACHINE, "overlay/align", 1);
           var img1 = checkImage(MACHINE, "overlay/align", 2);
           var img2 = checkImage(MACHINE, "overlay/align", 3);
           var restImages = [];
           for (var i = 4; i < MACHINE.a; i++) {
                restImages.push(checkImage(MACHINE, "overlay/align", i));
           }
           var img = makeOverlayImage(img1,
                                      img2,
                                      placeX.toString(),
                                      placeY.toString());
           for (var i = 0; i < restImages.length; i++)
             img = makeOverlayImage(img,
                                    restImages[i],
                                    placeX.toString(), 
                                    placeY.toString());
           return img;
       });


    EXPORTS['underlay'] = 
        makePrimitiveProcedure(
            'underlay',
            plt.baselib.arity.makeArityAtLeast(2),
            function(MACHINE) {
          var img1 = checkImage(MACHINE, "underlay", 0);
          var img2 = checkImage(MACHINE, "underlay", 1);
          var restImages = [];
          for (var i = 2; i < MACHINE.a; i++) {
              restImages.push(checkImage(MACHINE, "underlay", i));
          }

          var img = makeOverlayImage(img2, img1, "middle", "middle");
          for (var i = 0; i < restImages.length; i++) {
              img = makeOverlayImage(restImages[i], img, "middle", "middle");
          }
          return img;
      });


    EXPORTS['underlay/xy'] = 
        makePrimitiveProcedure(
            'underlay/xy',
            4,
            function(MACHINE) {
          var img1 = checkImage(MACHINE, "underlay/xy", 0);
          var deltaX = checkReal(MACHINE, "underlay/xy", 1);
          var deltaY = checkReal(MACHINE, "underlay/xy", 2);
          var img2 = checkImage(MACHINE, "underlay/xy", 3);
          return makeOverlayImage(img2,
                                  img1,
                                  -jsnums.toFixnum(deltaX),
                                  -jsnums.toFixnum(deltaY));
            });

    EXPORTS['underlay/align'] = 
        makePrimitiveProcedure(
            'underlay/align',
            plt.baselib.arity.makeArityAtLeast(4),
            function(MACHINE) {
          var placeX = checkPlaceX(MACHINE, "underlay/align", 0);
          var placeY = checkPlaceY(MACHINE, "underlay/align", 1);
          var img1 = checkImage(MACHINE, "underlay/align", 2);
          var img2 = checkImage(MACHINE, "underlay/align", 3);
          var restImages = [];
          for (var i = 4; i < MACHINE.a; i++) {
              restImages.push(checkImage(MACHINE, "underlay/align", i));
          }
          
          var img = makeOverlayImage(img2,
                                     img1,
                                     placeX.toString(),
                                     placeY.toString());
          
          for (var i = 0; i < restImages.length; i++) {
            img = makeOverlayImage(restImages[i],
                                   img,
                                   placeX.toString(), 
                                   placeY.toString());
          }
          return img;
      });



    EXPORTS['beside'] = 
        makePrimitiveProcedure(
            'beside',
            plt.baselib.arity.makeArityAtLeast(2),
            function(MACHINE) {
          var img1 = checkImage(MACHINE, "beside", 0);
          var img2 = checkImage(MACHINE, "beside", 1);
                var restImages = [];
          for (var i = 2; i < MACHINE.a; i++) {
                    restImages.push(checkImage(MACHINE, "beside", i));
                }
          
          var img = makeOverlayImage(img1,
                                     img2,
                                     "beside",
                                     "middle");
          
          for (var i = 0; i < restImages.length; i++) {
              img = makeOverlayImage(img, restImages[i], "beside", "middle");
          }
          return img;
      });


    EXPORTS['beside/align'] = 
        makePrimitiveProcedure(
            'beside/align',
            plt.baselib.arity.makeArityAtLeast(3),
            function(MACHINE) {
          var placeY = checkPlaceY(MACHINE, "beside/align", 0);
          var img1 = checkImage(MACHINE, "beside/align", 1);
          var img2 = checkImage(MACHINE, "beside/align", 2);
                var restImages = [];
                for (var i = 3; i < MACHINE.a; i++) {
                    restImages.push(checkImage(MACHINE, "beside/align", i));
                }

          var img = makeOverlayImage(img1,
                   img2,
                   "beside",
                   placeY.toString());
          
          for (var i = 0; i < restImages.length; i++) {
                img = makeOverlayImage(img,
                                       restImages[i],
                                       "beside",
                                       placeY.toString());
          }
          
          return img;
        });

    EXPORTS['above'] = 
        makePrimitiveProcedure(
            'above',
            plt.baselib.arity.makeArityAtLeast(2),
            function(MACHINE) {
          var img1 = checkImage(MACHINE, "above", 0);
          var img2 = checkImage(MACHINE, "above", 1);
          var restImages = [];
                for (var i = 2; i < MACHINE.a; i++) {
                    restImages.push(checkImage(MACHINE, "above", i));
                }
          
          var img = makeOverlayImage(img1,
                                     img2,
                                     "middle",
                                     "above");
          
          for (var i = 0; i < restImages.length; i++)
              img = makeOverlayImage(img,
                                     restImages[i],
                                     "middle",
                                     "above");
          return img;
        });

    EXPORTS['above/align'] = 
        makePrimitiveProcedure(
            'above/align',
            plt.baselib.arity.makeArityAtLeast(3),
            function(MACHINE) {
          var placeX = checkPlaceX(MACHINE, "above/align", 0);
          var img1 = checkImage(MACHINE, "above/align", 1);
          var img2 = checkImage(MACHINE, "above/align", 2);
          var restImages = [];
          for (var i = 3; i < MACHINE.a; i++) {
              restImages.push(checkImage(MACHINE, "above/align", i));
          }

          
          var img = makeOverlayImage(img1,
                                     img2,
                                     placeX.toString(),
                                     "above");
          
          for (var i = 0; i < restImages.length; i++)
              img = makeOverlayImage(img,
                                     restImages[i],
                                     placeX.toString(),
                                     "above");
          
          return img;
        });




    EXPORTS['empty-scene'] =
        makePrimitiveProcedure(
            'empty-scene',
      2,
      function(MACHINE) {
          var width = checkNonNegativeReal(MACHINE, 'empty-scene', 0);
          var height = checkNonNegativeReal(MACHINE, 'empty-scene', 1);
          return makeSceneImage(jsnums.toFixnum(width), 
                                      jsnums.toFixnum(height),
                                      [],
                                      true);
      });

    EXPORTS['put-image'] =
    makePrimitiveProcedure(
                           'put-image',
                           4,
                           function(MACHINE) {
                           var picture = checkImage(MACHINE, "put-image", 0);
                           var x = checkReal(MACHINE, "put-image", 1);
                           var y = checkReal(MACHINE, "put-image", 2);
                           var background = checkImageOrScene(MACHINE, "put-image", 3);
                           if (isScene(background)) {
                           return background.add(picture, jsnums.toFixnum(x), jsnums.toFixnum(y));
                           } else {
                           var newScene = makeSceneImage(background.getWidth(),
                                                         background.getHeight(),
                                                         [],
                                                         false);
                           newScene = newScene.add(background, background.getWidth()/2, background.getHeight()/2);
                           newScene = newScene.add(picture, jsnums.toFixnum(x), background.getHeight() - jsnums.toFixnum(y));
                           return newScene;
                           }
                           });


    EXPORTS['place-image'] = 
        makePrimitiveProcedure(
            'place-image',
            4,
            function(MACHINE) {
          var picture = checkImage(MACHINE, "place-image", 0);
          var x = checkReal(MACHINE, "place-image", 1);
          var y = checkReal(MACHINE, "place-image", 2);
                var background = checkImageOrScene(MACHINE, "place-image", 3);
          if (isScene(background)) {
            return background.add(picture, jsnums.toFixnum(x), jsnums.toFixnum(y));
          } else {
            var newScene = makeSceneImage(background.getWidth(),
                                          background.getHeight(),
                                          [], 
                                          false);
            newScene = newScene.add(background, background.getWidth()/2, background.getHeight()/2);
            newScene = newScene.add(picture, jsnums.toFixnum(x), jsnums.toFixnum(y));
            return newScene;
          }
      });



    EXPORTS['place-image/align'] = 
        makePrimitiveProcedure(
            'place-image/align',
            6,
            function(MACHINE) {
           var img = checkImage(MACHINE, "place-image/align", 0);
           var x = checkReal(MACHINE, "place-image/align", 1);
           var y = checkReal(MACHINE, "place-image/align", 2);
           var placeX = checkPlaceX(MACHINE, "place-image/align", 3);
           var placeY = checkPlaceY(MACHINE, "place-image/align", 4);
           var background = checkImageOrScene(MACHINE, "place-image/align", 5);
          
           // calculate x and y based on placeX and placeY
           if		 (placeX == "left"  )  x = x + img.getWidth()/2;
           else if (placeX == "right" ) x = x - img.getWidth()/2;
           if		 (placeY == "top"   )  y = y + img.getHeight()/2;
           else if (placeY == "bottom") y = y - img.getHeight()/2;

          if (isScene(background)) {
              return background.add(img, jsnums.toFixnum(x), jsnums.toFixnum(y));
          } else {
              var newScene = makeSceneImage(background.getWidth(),
                                            background.getHeight(),
                                            [], 
                                            false);
             newScene = newScene.add(background, background.getWidth()/2, background.getHeight()/2);
             newScene = newScene.add(img, jsnums.toFixnum(x), jsnums.toFixnum(y));
             return newScene;
          }
      });


    EXPORTS['rotate'] = 
        makePrimitiveProcedure(
            'rotate',
            2,
            function(MACHINE) {
          var angle = checkAngle(MACHINE, "rotate", 0);
          var img = checkImage(MACHINE, "rotate", 1);
          return makeRotateImage(jsnums.toFixnum(-angle), img);
            });



    EXPORTS['scale'] = 
        makePrimitiveProcedure(
            'scale',
            2,
            function(MACHINE) {
          var factor = checkReal(MACHINE, "scale", 0);
          var img = checkImage(MACHINE, "image", 1);
          
          return makeScaleImage(jsnums.toFixnum(factor),
              jsnums.toFixnum(factor),
              img);
            });


    EXPORTS['scale/xy'] = 
        makePrimitiveProcedure(
            'scale/xy',
            3,
            function(MACHINE) {
          var xFactor = checkReal(MACHINE, "scale/xy", 0);
          var yFactor = checkReal(MACHINE, "scale/xy", 1);
          var img = checkImage(MACHINE, "scale/xy", 2);
          return makeScaleImage(jsnums.toFixnum(xFactor), 
              jsnums.toFixnum(yFactor),
              img);
          
            });


    EXPORTS['flip-horizontal'] = 
        makePrimitiveProcedure(
            'flip-horizontal',
            1,
            function(MACHINE) {
          var img = checkImage(MACHINE, "flip-horizontal", 0);
          return makeFlipImage(img, "horizontal");
            });


    EXPORTS['flip-vertical'] = 
        makePrimitiveProcedure(
            'flip-vertical',
            1,
            function(MACHINE) {
          var img = checkImage(MACHINE, "flip-vertical", 0);
          return makeFlipImage(img, "vertical");
            });


    EXPORTS['frame'] = 
        makePrimitiveProcedure(
            'frame',
            1,
            function(MACHINE) {
          var img = checkImage(MACHINE, "frame", 0);
          return makeFrameImage(img);            
            });


    EXPORTS['crop'] = 
        makePrimitiveProcedure(
            'crop',
            5,
            function(MACHINE) {
                var x = checkReal(MACHINE, "crop", 0);
          var y = checkReal(MACHINE, "crop", 1);
          var width = checkNonNegativeReal(MACHINE, "crop", 2);
          var height = checkNonNegativeReal(MACHINE, "crop", 3);
          var img = checkImage(MACHINE, "crop", 4);
          return makeCropImage(jsnums.toFixnum(x),
             jsnums.toFixnum(y),
             jsnums.toFixnum(width),
             jsnums.toFixnum(height),
             img);
            });



    EXPORTS['line'] = 
        makePrimitiveProcedure(
            'line',
            3,
            function(MACHINE) {
          var x = checkReal(MACHINE, 'line', 0);
          var y = checkReal(MACHINE, 'line', 1);
          var c = checkColor(MACHINE, 'line', 2);
          return makeLineImage(jsnums.toFixnum(x),
             jsnums.toFixnum(y),
             c,
             true);
            });




    EXPORTS['add-line'] = 
        makePrimitiveProcedure(
            'add-line',
            6,
            function(MACHINE) {
          var img = checkImage(MACHINE, "add-line", 0);
          var x1 = checkReal(MACHINE, "add-line", 1);
          var y1 = checkReal(MACHINE, "add-line", 2);
          var x2 = checkReal(MACHINE, "add-line", 3);
          var y2 = checkReal(MACHINE, "add-line", 4);
          var c = checkColor(MACHINE, "add-line", 5);
          var line = makeLineImage(jsnums.toFixnum(x2-x1),
                 jsnums.toFixnum(y2-y1),
                 c,
                 true),
               leftMost = Math.min(x1,x2),
               topMost = Math.min(y1,y2);
          return makeOverlayImage(line, img, -leftMost, -topMost);
            });



    EXPORTS['scene+line'] = 
        makePrimitiveProcedure(
            'scene+line',
            6,
            function(MACHINE) {
                var img = checkImage(MACHINE, "scene+line", 0);
          var x1 = checkReal(MACHINE, "scene+line", 1);
          var y1 = checkReal(MACHINE, "scene+line", 2);
          var x2 = checkReal(MACHINE, "scene+line", 3);
          var y2 = checkReal(MACHINE,	"scene+line", 4);
          var c = checkColor(MACHINE, "scene+line", 5);
          // make a scene containing the image
          var newScene = makeSceneImage(jsnums.toFixnum(img.getWidth()), 
                                        jsnums.toFixnum(img.getHeight()),
                                        [],
                                        true);
          newScene = newScene.add(img, img.getWidth()/2, img.getHeight()/2);
          // make an image containing the line
          var line = makeLineImage(jsnums.toFixnum(x2-x1),
                                   jsnums.toFixnum(y2-y1),
                                   c,
                                   false),
          leftMost = Math.min(x1,x2),
          topMost = Math.min(y1,y2);
          return newScene.add(line, line.getWidth()/2+leftMost, line.getHeight()/2+topMost);
        });


    EXPORTS['circle'] = 
        makePrimitiveProcedure(
            'circle',
            3,
            function(MACHINE) {
                var aRadius = checkNonNegativeReal(MACHINE, "circle", 0);
          var aMode = checkMode(MACHINE, "circle", 1);
          var aColor = checkColor(MACHINE, "circle", 2);
          return makeCircleImage(jsnums.toFixnum(aRadius), aMode.toString(), aColor);
            });


    EXPORTS['square'] = 
        makePrimitiveProcedure(
            'square',
            3,
            function(MACHINE) {
          var l = checkNonNegativeReal(MACHINE, "square", 0);
          var s = checkMode(MACHINE, "square", 1);
          var c = checkColor(MACHINE, "square", 2);
          return makeSquareImage(jsnums.toFixnum(l), s.toString(), c);
            });


    EXPORTS['rectangle'] = 
        makePrimitiveProcedure(
            'rectangle',
            4,
            function(MACHINE) {
          var w = checkNonNegativeReal(MACHINE, "rectangle", 0);
          var h = checkNonNegativeReal(MACHINE, "rectangle", 1);
          var s = checkMode(MACHINE, "rectangle", 2);
          var c = checkColor(MACHINE, "rectangle", 3);
          return makeRectangleImage(jsnums.toFixnum(w),
                  jsnums.toFixnum(h),
                  s.toString(), 
                  c);
            });


    EXPORTS['regular-polygon'] = 
        makePrimitiveProcedure(
            'regular-polygon',
            4,
            function(MACHINE) {
          var length = checkNonNegativeReal(MACHINE, "regular-polygon", 0);
          var count = checkSideCount(MACHINE, "regular-polygon", 1);
          var s = checkMode(MACHINE, "regular-polygon", 2);
          var c = checkColor(MACHINE, "regular-polygon", 3);
          return makePolygonImage(jsnums.toFixnum(length), 
                jsnums.toFixnum(count), 
                jsnums.toFixnum(1), 
                s.toString(), 
                c);            
            });


    EXPORTS['ellipse'] = 
        makePrimitiveProcedure(
            'ellipse',
            4,
            function(MACHINE) {
                var w = checkNonNegativeReal(MACHINE, "ellipse", 0);
          var h = checkNonNegativeReal(MACHINE, "ellipse", 1);
          var s = checkMode(MACHINE, "ellipse", 2);
          var c = checkColor(MACHINE, MACHINE, 3);
          return makeEllipseImage(jsnums.toFixnum(w),
                jsnums.toFixnum(h),
                s.toString(),
                c);
            });



    EXPORTS['triangle'] = 
        makePrimitiveProcedure(
            'triangle',
            3,
            function(MACHINE) {
              var s = checkNonNegativeReal(MACHINE, "triangle", 0);
              var m = checkMode(MACHINE, "triangle", 1);
              var c = checkColor(MACHINE, "triangle", 2);
              return makeTriangleImage(jsnums.toFixnum(s), 
                                       jsnums.toFixnum(60+180), // add 180 to make the triangle point up
                                       jsnums.toFixnum(s),
                                       m.toString(),
                                       c);
            });

    EXPORTS['triangle/sas'] =
        makePrimitiveProcedure(
            'triangle/sas',
             5,
             function(MACHINE) {
               var base = checkNonNegativeReal(MACHINE, "triangle/sas", 0);
               var angleC = checkAngle(MACHINE, "triangle/sas", 1);
               var sideB = checkNonNegativeReal(MACHINE, "triangle/sas", 2);
               var style = checkMode(MACHINE, "triangle/sas", 3);
               var color = checkColor(MACHINE, "triangle/sas", 4);
               if (colorDb.get(color)) { color = colorDb.get(color); }
               return makeTriangleImage(jsnums.toFixnum(base),
                                        jsnums.toFixnum(angleC),
                                        jsnums.toFixnum(sideB),
                                        style.toString(),
                                        color);
             });

    EXPORTS['triangle/sss'] =
        makePrimitiveProcedure(
             'triangle/sss',
             5,
             function(MACHINE) {
               var base = checkNonNegativeReal(MACHINE, "triangle/sss", 0);
               var sideB = checkNonNegativeReal(MACHINE, "triangle/sss", 1);
               var sideC = checkNonNegativeReal(MACHINE, "triangle/sss", 2);
               var style = checkMode(MACHINE, "triangle/sss", 3);
               var color = checkColor(MACHINE, "triangle/sss", 4);
               if (colorDb.get(color)) { color = colorDb.get(color); }
               var angleC = (Math.acos((base*base + sideB*sideB - sideC*sideC) / (2*base*sideB)))*180/Math.PI;
               return makeTriangleImage(jsnums.toFixnum(base),
                                       jsnums.toFixnum(angleC),
                                       jsnums.toFixnum(sideB),
                                       style.toString(),
                                       color);
             });

    EXPORTS['triangle/ass'] =
        makePrimitiveProcedure(
             'triangle/ass',
             5,
             function(MACHINE) {
             var angle = checkAngle(MACHINE, "triangle/ass", 0);
             var base = checkNonNegativeReal(MACHINE, "triangle/ass", 1);
             var sideB = checkNonNegativeReal(MACHINE, "triangle/ass", 2);
             var style = checkMode(MACHINE, "triangle/ass", 3);
             var color = checkColor(MACHINE, "triangle/ass", 4);
             if (colorDb.get(color)) { color = colorDb.get(color); }
             return makeTriangleImage(jsnums.toFixnum(base),
                                     jsnums.toFixnum(angle),
                                     jsnums.toFixnum(sideB),
                                     style.toString(),
                                     color);
                           });

    EXPORTS['triangle/ssa'] =
        makePrimitiveProcedure(
             'triangle/ssa',
             5,
             function(MACHINE) {
             var base = checkNonNegativeReal(MACHINE, "triangle/ssa", 0);
             var sideB = checkNonNegativeReal(MACHINE, "triangle/ssa", 1);
             var angleA = checkAngle(MACHINE, "triangle/ssa", 2);
             var style = checkMode(MACHINE, "triangle/ssa", 3);
             var color = checkColor(MACHINE, "triangle/ssa", 4);
             if (colorDb.get(color)) { color = colorDb.get(color); }
             var angleB = Math.asin(Math.sin(angleA*Math.PI/180)*sideB/sideA)*180/Math.PI;
             var angleC = (180 - angleA - angleB);
             return makeTriangleImage(jsnums.toFixnum(base),
                                     jsnums.toFixnum(angleC),
                                     jsnums.toFixnum(sideB),
                                     style.toString(),
                                     color);
                           });

    EXPORTS['triangle/aas'] =
        makePrimitiveProcedure(
             'triangle/aas',
             5,
             function(MACHINE) {
               var angleA = checkAngle(MACHINE, "triangle/aas", 0);
               var angleB = checkAngle(MACHINE, "triangle/aas", 1);
               var base = checkNonNegativeReal(MACHINE, "triangle/aas", 2);
               var style = checkMode(MACHINE, "triangle/aas", 3);
               var color = checkColor(MACHINE, "triangle/aas", 4);
               if (colorDb.get(color)) { color = colorDb.get(color); }
               var angleC = (180 - angleA - angleB);
               var sideB = (base * Math.sin(angleB*Math.PI/180)) / (Math.sin(angleC*Math.PI/180));
               return makeTriangleImage(jsnums.toFixnum(base),
                                       jsnums.toFixnum(angleC),
                                       jsnums.toFixnum(sideB),
                                       style.toString(),
                                       color);
                           });

    EXPORTS['triangle/asa'] =
        makePrimitiveProcedure(
             'triangle/asa',
             5,
             function(MACHINE) {
               var angleA = checkAngle(MACHINE, "triangle/asa", 0);
               var sideC  = checkNonNegativeReal(MACHINE, "triangle/asa", 1);
               var angleB = checkAngle(MACHINE, "triangle/asa", 2);
               var style = checkMode(MACHINE, "triangle/asa", 3);
               var color = checkColor(MACHINE, "triangle/asa", 4);
               if (colorDb.get(color)) { color = colorDb.get(color); }
               var angleC = (180 - angleA - angleB);
               var base = (sideC * Math.sin(angleA*Math.PI/180)) / (Math.sin(angleC*Math.PI/180));
               var sideB = (sideC * Math.sin(angleB*Math.PI/180)) / (Math.sin(angleC*Math.PI/180));
               return makeTriangleImage(jsnums.toFixnum(base),
                                       jsnums.toFixnum(angleC),
                                       jsnums.toFixnum(sideB),
                                       style.toString(),
                                       color);
             });

    EXPORTS['triangle/saa'] =
        makePrimitiveProcedure(
               'triangle/saa',
               5,
               function(MACHINE) {
               var base = checkNonNegativeReal(MACHINE, "triangle/saa", 0);
               var angleC  = checkAngle(MACHINE, "triangle/saa", 1);
               var angleA = checkAngle(MACHINE, "triangle/saa", 2);
               var style = checkMode(MACHINE, "triangle/saa", 3);
               var color = checkColor(MACHINE, "triangle/saa", 4);
               if (colorDb.get(color)) { color = colorDb.get(color); }
               var angleB = (180 - angleA - angleC);
               var sideB = (base * Math.sin(angleB*Math.PI/180)) / (Math.sin(angleA*Math.PI/180));
               return makeTriangleImage(jsnums.toFixnum(base),
                                       jsnums.toFixnum(angleC),
                                       jsnums.toFixnum(sideB),
                                       style.toString(),
                                       color);
               });


    EXPORTS['right-triangle'] =
        makePrimitiveProcedure(
            'right-triangle',
            4,
            function(MACHINE) {
          var side1 = checkNonNegativeReal(MACHINE, "right-triangle", 0);
          var side2 = checkNonNegativeReal(MACHINE, "right-triangle", 1);
          var s = checkMode(MACHINE, "right-triangle", 2);
          var c = checkColor(MACHINE, "right-triangle", 3);
          return makeTriangleImage(jsnums.toFixnum(side1),
                                   jsnums.toFixnum(90+180), // add 180 to make the triangle point up
                                   jsnums.toFixnum(side2),
                                   s.toString(),
                                   c);
            });


    EXPORTS['isosceles-triangle'] = 
        makePrimitiveProcedure(
            'isosceles-triangle',
            4,
            function(MACHINE) {
          var side = checkNonNegativeReal(MACHINE, "isosceles-triangle", 0);
          var angleC = checkAngle(MACHINE, "isosceles-triangle", 1);
          var style = checkMode(MACHINE, "isosceles-triangle", 2);
          var color = checkColor(MACHINE, "isosceles-triangle", 3);
          var angleAB = (180-angleC)/2;
          var base = 2*side*Math.sin((angleC*Math.PI/180)/2);
          return makeTriangleImage(jsnums.toFixnum(base),
                                  jsnums.toFixnum(angleAB+180),// add 180 to make the triangle point up
                                  jsnums.toFixnum(side),
                                  style.toString(),
                                  color);
         });

    EXPORTS['star'] = 
        makePrimitiveProcedure(
            'star',
            plt.baselib.lists.makeList(3, 5),
            function(MACHINE) {
                if (MACHINE.a === 3) {
                    var sideLength = checkNonNegativeReal(MACHINE, "star", 0);
        var mode = checkMode(MACHINE, "star", 1);
        var color = checkColor(MACHINE, "star", 2);
        return makePolygonImage(jsnums.toFixnum(sideLength), 
              jsnums.toFixnum(5), 
              jsnums.toFixnum(2), 
              mode.toString(), 
              color);
                } else if (MACHINE.a === 5) {
        var n = checkSideCount(MACHINE, "star", 0);
        var outer = checkNonNegativeReal(MACHINE, "star", 1);
        var inner = checkNonNegativeReal(MACHINE, "star", 2);
        var m = checkMode(MACHINE, "star", 3);
        var c = checkColor(MACHINE, "star", 4);
        return makeStarImage(jsnums.toFixnum(n),
                 jsnums.toFixnum(outer),
                 jsnums.toFixnum(inner),
                 m.toString(),
                 c);
                }
            });

    EXPORTS['radial-star'] = 
        makePrimitiveProcedure(
            'radial-star',
            5,
            function(MACHINE) {
                var aPoints = checkPointsCount(MACHINE, 'radial-star', 0);
          var anOuter = checkNonNegativeReal(MACHINE, 'radial-star', 1);
          var anInner = checkNonNegativeReal(MACHINE, 'radial-star', 2);
          var aStyle = checkMode(MACHINE, "radial-star", 3);
          var aColor = checkColor(MACHINE, "radial-star", 4);
          return makeStarImage(jsnums.toFixnum(aPoints),
             jsnums.toFixnum(anOuter),
             jsnums.toFixnum(anInner),
             aStyle.toString(),
             aColor);
            });



    EXPORTS['star-polygon'] = 
        makePrimitiveProcedure(
            'star-polygon',
            5,
            function(MACHINE) {
                var length = checkNonNegativeReal(MACHINE, "star-polygon", 0);
          var count = checkNonNegativeReal(MACHINE, "star-polygon", 1);
          var step = checkStepCount(MACHINE, "star-polygon", 2);
          var s = checkMode(MACHINE, "star-polygon", 3);
          var c = checkColor(MACHINE, "star-polygon", 4);
          return makePolygonImage(jsnums.toFixnum(length), 
                jsnums.toFixnum(count), 
                jsnums.toFixnum(step), 
                s.toString(), 
                c);
            });


    EXPORTS['rhombus'] = 
        makePrimitiveProcedure(
            'rhombus',
            4,
            function(MACHINE) {
                var l = checkNonNegativeReal(MACHINE, "rhombus", 0);
          var a = checkNonNegativeReal(MACHINE, "rhombus", 1);
          var s = checkMode(MACHINE, "rhombus", 2);
          var c = checkColor(MACHINE, "rhombus", 3);
          return makeRhombusImage(jsnums.toFixnum(l),
                                        jsnums.toFixnum(a),
                                        s.toString(),
                                        c);
          
            });


    EXPORTS['image->color-list'] = 
        makePrimitiveProcedure(
            'image->color-list',
            1,
            function(MACHINE) {
                var img = checkImage(MACHINE, 'image->color-list', 0);
                return imageToColorList(img);
            });



    EXPORTS['color-list->image'] = 
        makePrimitiveProcedure(
            'color-list->image',
            5,
            function(MACHINE) {
                var listOfColors = checkListofColor(MACHINE, 'color-list->image', 0);
          var width = checkNatural(MACHINE, 'color-list->image', 1);
          var height = checkNatural(MACHINE, 'color-list->image', 2);
          var pinholeX = checkNatural(MACHINE, 'color-list->image', 3);
          var pinholeY = checkNatural(MACHINE, 'color-list->image', 4);

                return colorListToImage(listOfColors,
                                        width,
                                        height,
                                        pinholeX,
                                        pinholeY);
            });

    EXPORTS['color-list->bitmap'] = 
        makePrimitiveProcedure(
            'color-list->image',
            3,
            function(MACHINE) {
                var listOfColors = checkListofColor(MACHINE, 'color-list->image', 0);
          var width = checkNatural(MACHINE, 'color-list->image', 1);
          var height = checkNatural(MACHINE, 'color-list->image', 2);
                return colorListToImage(listOfColors,
                                        width,
                                        height,
                                        0,
                                        0);
            });


    EXPORTS['image-width'] = 
        makePrimitiveProcedure(
            'image-width',
            1,
            function(MACHINE) {
          var img = checkImage(MACHINE, 'image-width', 0);
          return img.getWidth();
            });

    EXPORTS['image-height'] = 
        makePrimitiveProcedure(
            'image-height',
            1,
            function(MACHINE) {
          var img = checkImage(MACHINE, 'image-height', 0);
          return img.getHeight();
            });

    EXPORTS['image-baseline'] = 
        makePrimitiveProcedure(
            'image-baseline',
            1,
            function(MACHINE) {
          var img = checkImage(MACHINE, 'image-baseline', 0);
          return img.getBaseline();
            });


    EXPORTS['name->color'] = 
        makePrimitiveProcedure(
            'name->color',
            1,
            function(MACHINE) {
                var name = checkSymbolOrString(MACHINE, 'name->color', 0);
                var result = colorDb.get('' + name) || false;
                return result;
            });

        
      }),
      answer: runtime.namespace.get("nothing")
    });

*/
  }; // end rt/ns fun
});
