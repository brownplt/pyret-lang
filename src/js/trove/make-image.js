({
  requires: [
    { "import-type": "builtin", "name": "image-lib" },
    { "import-type": "builtin", "name": "ffi" },
    { "import-type": "builtin", "name": "filesystem-internal" }
  ],
  nativeRequires: [
    "pyret-base/js/js-numbers",
    "fs",
    "canvas",
  ],
  provides: {},
  theModule: function(runtime, namespace, uri, imageLib, ffi, fsInternal, jsnums, fs, canvas) {
    var image = runtime.getField(imageLib, "internal");
    if(canvas.Image) {
      var Image = canvas.Image; // The polyfill for the browser Image API (passes through raw Image on CPO)
    }
    else {
      var Image = window.Image;
    }
    
    function makeImageLib(moduleName, annots) {
      const colorDb = image.colorDb;

      const annListImage = annots.annListImage;
      const unwrapListofImage = annots.unwrapListofImage;
      const annString = annots.annString;
      const annNumber = annots.annNumber;
      const annPositive = annots.annPositive;
      const annNumNonNegative = annots.annNumNonNegative;
      const annByte = annots.annByte;
      const annReal = annots.annReal;
      const annNatural = annots.annNatural;
      const annColor = annots.annColor;
      const unwrapColor = annots.unwrapColor;
      const annMode = annots.annMode;
      const unwrapMode = annots.unwrapMode;
      const annFontFamily = annots.annFontFamily;
      const unwrapFontFamily = annots.unwrapFontFamily;
      const annFontStyle = annots.annFontStyle;
      const unwrapFontStyle = annots.unwrapFontStyle;
      const annFontWeight = annots.annFontWeight;
      const unwrapFontWeight = annots.unwrapFontWeight;
      const annPlaceX = annots.annPlaceX;
      const unwrapPlaceX = annots.unwrapPlaceX;
      const annPlaceY = annots.annPlaceY;
      const unwrapPlaceY = annots.unwrapPlaceY;
      const annImage = annots.annImage;
      const unwrapImage = annots.unwrapImage;
      const annImageOrScene = annots.annImageOrScene;
      const unwrapImageOrScene = annots.unwrapImageOrScene;
      const annAngle = annots.annAngle;
      const annListColor = annots.annListColor;
      const unwrapListofColor = annots.unwrapListofColor;
      const annSideCount = annots.annSideCount;
      const annStepCount = annots.annStepCount;
      const annPointCount = annots.annPointCount;
      const annListPoint2D = annots.annListPoint2D;
      const unwrapListofPoint2D = annots.unwrapListofPoint2D;

      const checkArity = ffi.checkArity;

      const throwMessage = ffi.throwMessageException;

      // [Image int Image -> Image] [Listof PyretImage] Image -> Image
      var imageListFoldIndex = function(func, lst, base) {
        var cur = lst;
        var ans = base;
        var gf = runtime.getField;
        var index = 0;
        while (runtime.unwrap(ffi.isLink(cur))) {
          var f = gf(cur, "first");
          ans = func(ans, index++, unwrapImage(f));
          cur = gf(cur, "rest");
        }
        return ans;
      };


      function less(n, m) {
        return jsnums.lessThan(n, m, runtime.NumberErrbacks);
      }


      function canonicalizeAngle(angle) {
        angle = jsnums.remainder(angle, 360, runtime.NumberErrbacks);
        if (jsnums.lessThan(angle, 0, runtime.NumberErrbacks)) {
          angle = jsnums.add(angle, 360, runtime.NumberErrbacks);
        }
        return angle;
      };
      
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

      const c = function(name, ...argsAndAnns) {
        runtime.checkArgsInternalInline(moduleName, name, ...argsAndAnns);
      };
      const c1 = function(name, arg, ann) {
        runtime.checkArgsInternal1(moduleName, name, arg, ann);
      };
      const c2 = function(name, arg1, ann1, arg2, ann2) {
        runtime.checkArgsInternal2(moduleName, name, arg1, ann1, arg2, ann2);
      };
      const c3 = function(name, arg1, ann1, arg2, ann2, arg3, ann3) {
        runtime.checkArgsInternal3(moduleName, name, arg1, ann1, arg2, ann2, arg3, ann3);
      };
      //////////////////////////////////////////////////////////////////////
      var bitmapURL = function(url) {
        return runtime.pauseStack(function(restarter) {
          var rawImage = new Image();
          var originalUrl = url;
          if(runtime.hasParam("imgUrlProxy")) {
            url = runtime.getParam("imgUrlProxy")(url);
          }
          rawImage.onload = function() {
            restarter.resume(makeImage(image.makeFileImage(String(url), rawImage)));
          };
          rawImage.onerror = function(e) {
            restarter.error(runtime.ffi.makeMessageException("Unable to load " + originalUrl + ". If that URL loads when you open it in your browser, there may be an issue with how that website serves images for programs. One workaround is to download the image, put it in your Google Drive, and use the 'Insert' button to add it to your program."));
          };
          rawImage.setAttribute("crossorigin", "anonymous");
          rawImage.src = String(url);
        });
      };


      // Some discussion of this code at https://stackoverflow.com/a/66046176/2718315
      async function bufferToBase64(buffer, mime) {
        if(typeof FileReader !== 'undefined') {
          const base64url = await new Promise(r => {
            const reader = new FileReader()
            reader.onload = () => r(reader.result)
            reader.readAsDataURL(new Blob([buffer]))
          });
          const data = base64url.slice(base64url.indexOf(",") + 1);
          return `data:${mime};base64, ${data}`;
        }
        else {
          const data = buffer.toString('base64');
          return `data:${mime};base64, ${data}`;
        }
      }
      const extensiontypes = {
        "png": "image/png",
        "jpg": "image/jpeg",
        "jpeg": "image/jpeg"
      };
      const allowedExtensions = Object.keys(extensiontypes);
      var imageFile = function(path) {
        const lastDot = path.lastIndexOf(".");
        if(lastDot === -1) { throw runtime.ffi.makeMessageException("Path to image-file did not have an extension, must be one of " + allowedExtensions.join(", ")); }
        const extension = path.slice(lastDot + 1).toLowerCase();
        const mime = extensiontypes[extension];
        if(!mime) { throw runtime.ffi.makeMessageException(`Path to image-file did not have a valid extension (got ${extension}), must be one of ${allowedExtensions.join(", ")}`); }
        return runtime.pauseStack(async function(restarter) {
          if(fsInternal.init) {
            try {
              const contentsBuffer = await fsInternal.readFile(path);
              const dataURL = await bufferToBase64(contentsBuffer, mime);
              var rawImage = new Image();
              rawImage.onload = function() {
                restarter.resume(makeImage(image.makeFileImage(dataURL, rawImage)));
              };
              rawImage.onerror = function(e) {
                restarter.error(runtime.ffi.makeMessageException("Unable to load " + path + " " + String(e)));
              };
              rawImage.src = dataURL;
            }
            catch(err) {
              restarter.error(runtime.ffi.makeMessageException(String(err))); 
            }
          }
          else {
            fs.readFile(path, {}, async (err, result) => {
              if(err) { restarter.error(runtime.ffi.makeMessageException(String(err))); }
              else {
                // create a data url from the result from readFile stored in result:
                var dataURL = await bufferToBase64(result, mime);
                var rawImage = new Image();
                rawImage.onload = function() {
                  restarter.resume(makeImage(image.makeFileImage(dataURL, rawImage)));
                };
                rawImage.onerror = function(e) {
                  restarter.error(runtime.ffi.makeMessageException("Unable to load " + path));
                };
                rawImage.src = dataURL;
              }
            });
          }
        })
      }

      async function getBuffer(canvas) {
        if(canvas.toBuffer) { return canvas.toBuffer("image/png"); }
        else {
          return new Promise((resolve, reject) => {
            try {
              canvas.toBlob(async (blob) => {
                const buffer = new Uint8Array(await blob.arrayBuffer());
                resolve(buffer);
              }, "image/png");
            }
            catch(e) {
              reject(e);
            }
          });
        }
      }

      function saveImage(img, path) {
        return runtime.pauseStack(async function(restarter) {
          const canvas = image.makeCanvas(img.width, img.height);
          img.render(canvas.getContext("2d"));
          const buffer = await getBuffer(canvas);
          if(fsInternal.init) {
            try {
              await fsInternal.writeFile(path, buffer);
              restarter.resume(runtime.nothing);
            }
            catch(err) {
              restarter.error(runtime.ffi.makeMessageException(String(err)));
            }
          }
          else {
            fs.writeFile(path, buffer, function(err) {
              if(err) { restarter.error(runtime.ffi.makeMessageException(String(err))); }
              else { restarter.resume(runtime.nothing); }
            });
          }
        });
      }

      var values = {};
      function f(name, fun) {
        values[name] = runtime.makeFunction(fun, name);
      }

      f("circle", function(radius, maybeMode, maybeColor) {
        checkArity(3, arguments, "image", false);
        c3("circle", radius, annNumNonNegative, maybeMode, annMode, maybeColor, annColor);
        var color = unwrapColor(maybeColor);
        var mode = unwrapMode(maybeMode)
        return makeImage(image.makeCircleImage(jsnums.toFixnum(radius), mode, color));
      });
      f("is-angle", function(maybeAngle) {
        checkArity(1, arguments, "is-angle", false);
        return runtime.wrap(image.isAngle(maybeAngle));
      });
      f("is-side-count", function(maybeSideCount) {
        checkArity(1, arguments, "is-side-count", false);
        return runtime.wrap(image.isSideCount(maybeSideCount));
      });
      f("is-step-count", function(maybeStepCount) {
        checkArity(1, arguments, "is-step-count", false);
        return runtime.wrap(image.isStepCount(maybeStepCount));
      });
      f("is-image", function(maybeImage) {
        checkArity(1, arguments, "is-image", false);
        return runtime.wrap(runtime.isOpaque(maybeImage) && image.isImage(maybeImage.val));
      });
      f("bitmap-url", function(maybeURL) {
        checkArity(1, arguments, "bitmap-url", false);
        return bitmapURL(maybeURL);
      }),
      f("image-url", function(maybeURL) {
        checkArity(1, arguments, "image-url", false);
        return bitmapURL(maybeURL);
      }),
      f("image-file", function(path) {
        checkArity(1, arguments, "image-file", false);
        c1("image-file", path, annString);
        return imageFile(path);
      }),
      f("save-image", function(maybeImage, maybePath) {
        checkArity(2, arguments, "image", false);
        c2("save-image", maybeImage, annImage, maybePath, annString);
        return saveImage(unwrapImage(maybeImage), maybePath);
      })
      f("images-difference", function(maybeImage1, maybeImage2) {
        checkArity(2, arguments, "image", false);
        c2("images-difference", maybeImage1, annImage, maybeImage2, annImage);
        var img1 = unwrapImage(maybeImage1);
        var img2 = unwrapImage(maybeImage2);
        return runtime.wrap(image.imageDifference(img1, img2));
      });
      f("images-equal", function(maybeImage1, maybeImage2) {
        checkArity(2, arguments, "image", false);
        c2("images-equal", maybeImage1, annImage, maybeImage2, annImage);
        var img1 = unwrapImage(maybeImage1);
        var img2 = unwrapImage(maybeImage2);
        return runtime.wrap(image.imageEquals(img1, img2));
      });
      f("text", function(maybeString, maybeSize, maybeColor) {
        checkArity(3, arguments, "image", false);
        c3("text", maybeString, runtime.String, maybeSize, annByte, maybeColor, annColor);
        var string = maybeString;
        var size = jsnums.toFixnum(maybeSize);
        var color = unwrapColor(maybeColor);
        return makeImage(
          image.makeTextImage(String(string), size, color,
                              "normal", "Optimer", "", "", false));
      });
      f("text-font", function(maybeString, maybeSize, maybeColor, maybeFace,
                              maybeFamily, maybeStyle, maybeWeight, maybeUnderline) {
        checkArity(8, arguments, "image", false);
        c("text-font", 
          maybeString, runtime.String,
          maybeSize, annByte,
          maybeColor, annColor,
          maybeFace, runtime.String,
          maybeFamily, annFontFamily,
          maybeStyle, annFontStyle,
          maybeWeight, annFontWeight,
          maybeUnderline, runtime.Boolean);
        var string = maybeString;
        var size = jsnums.toFixnum(maybeSize);
        var color = unwrapColor(maybeColor);
        var face = maybeFace;
        var family = unwrapFontFamily(maybeFamily);
        var style = unwrapFontStyle(maybeStyle);
        var weight = unwrapFontWeight(maybeWeight);
        var underline = maybeUnderline;
        return makeImage(image.makeTextImage(string, size, color, face, family, style, weight, underline));
      }),

      f("overlay", function(maybeImg1, maybeImg2) {
        checkArity(2, arguments, "overlay", false);
        c2("overlay", maybeImg1, annImage, maybeImg2, annImage);
        var img1 = unwrapImage(maybeImg1);
        var img2 = unwrapImage(maybeImg2);
        return makeImage(image.makeOverlayImage(img1, "pinhole", "pinhole", 0, 0, img2, "pinhole", "pinhole"));
      });

      f("overlay-list", function(maybeImgs) {
        checkArity(1, arguments, "overlay-list", false);
        c1("overlay-list", maybeImgs, annListImage);
        var imgs = unwrapListofImage(maybeImgs);
        return makeImage(imageListFoldIndex(function(acc, idx, img) {
          if (idx == 0) { return img; }
          else { return image.makeOverlayImage(acc, "pinhole", "pinhole", 0, 0, img, "pinhole", "pinhole"); }
        }, imgs, image.makeSceneImage(0, 0, [], false, colorDb.get("transparent"))));
      });

      f("overlay-xy", function(maybeImg1, maybeDx, maybeDy, maybeImg2) {
        checkArity(4, arguments, "overlay-xy", false);
        c("overlay-xy",
          maybeImg1, annImage,
          maybeDx, annReal,
          maybeDy, annReal,
          maybeImg2, annImage);
        var img1 = unwrapImage(maybeImg1);
        var dx = jsnums.toFixnum(maybeDx);
        var dy = jsnums.toFixnum(maybeDy);
        var img2 = unwrapImage(maybeImg2);
        return makeImage(
          image.makeOverlayImage(img1, "left", "top", dx, dy, img2, "left", "top"));
      });

      f("overlay-align", function(maybePlaceX, maybePlaceY, maybeImg1, maybeImg2) {
        checkArity(4, arguments, "overlay-align", false);
        c("overlay-align",
          maybePlaceX, annPlaceX,
          maybePlaceY, annPlaceY,
          maybeImg1, annImage,
          maybeImg2, annImage);
        var placeX = unwrapPlaceX(maybePlaceX);
        var placeY = unwrapPlaceY(maybePlaceY);
        var img1 = unwrapImage(maybeImg1);
        var img2 = unwrapImage(maybeImg2);
        return makeImage(image.makeOverlayImage(img1, placeX, placeY, 0, 0, img2, placeX, placeY));
      });

      f("overlay-align-list", function(maybePlaceX, maybePlaceY, maybeImgs) {
        checkArity(3, arguments, "overlay-align-list", false);
        c3("overlay-align-list", maybePlaceX, annPlaceX, maybePlaceY, annPlaceY, maybeImgs, annListImage);
        var placeX = unwrapPlaceX(maybePlaceX);
        var placeY = unwrapPlaceY(maybePlaceY);
        var imgs = unwrapListofImage(maybeImgs);
        return makeImage(imageListFoldIndex(function(acc, idx, img) {
          if (idx == 0) { return img; }
          else { return image.makeOverlayImage(acc, placeX, placeY, 0, 0, img, placeX, placeY); }
        }, imgs, image.makeSceneImage(0, 0, [], false, colorDb.get("transparent"))));
      });
      
      f("overlay-onto-offset", function(maybeImg1, maybePlaceX1, maybePlaceY1,
                                        maybeOffsetX, maybeOffsetY,
                                        maybeImg2, maybePlaceX2, maybePlaceY2) {
        checkArity(8, arguments, "overlay-onto-offset", false);
        c("overlay-onto-offset",
          maybeImg1, annImage,
          maybePlaceX1, annPlaceX,
          maybePlaceY1, annPlaceY,
          maybeOffsetX, annReal,
          maybeOffsetY, annReal,
          maybeImg2, annImage,
          maybePlaceX2, annPlaceX,
          maybePlaceY2, annPlaceY);
        var placeX1 = unwrapPlaceX(maybePlaceX1);
        var placeY1 = unwrapPlaceY(maybePlaceY1);
        var placeX2 = unwrapPlaceX(maybePlaceX2);
        var placeY2 = unwrapPlaceY(maybePlaceY2);
        var img1 = unwrapImage(maybeImg1);
        var img2 = unwrapImage(maybeImg2);
        var offsetX = jsnums.toFixnum(maybeOffsetX);
        var offsetY = jsnums.toFixnum(maybeOffsetY);
        return makeImage(image.makeOverlayImage(img1, placeX1, placeY1, offsetX, offsetY, img2, placeX2, placeY2));
      });

      f("underlay", function(maybeImg1, maybeImg2) {
        checkArity(2, arguments, "underlay", false);
        c2("underlay", maybeImg1, annImage, maybeImg2, annImage);
        var img1 = unwrapImage(maybeImg1);
        var img2 = unwrapImage(maybeImg2);
        return makeImage(image.makeOverlayImage(img2, "pinhole", "pinhole", 0, 0, img1, "pinhole", "pinhole"));
      });

      f("underlay-list", function(maybeImgs) {
        checkArity(1, arguments, "underlay-list", false);
        c1("underlay-list", maybeImgs, annListImage);
        var imgs = unwrapListofImage(maybeImgs);
        return makeImage(imageListFoldIndex(function(acc, idx, img) {
          if (idx == 0) { return img; }
          else { return image.makeOverlayImage(img, "pinhole", "pinhole", 0, 0, acc, "pinhole", "pinhole"); }
        }, imgs, image.makeSceneImage(0, 0, [], false, colorDb.get("transparent"))));
      });

      f("underlay-xy", function(maybeImg1, maybeDx, maybeDy, maybeImg2) {
        checkArity(4, arguments, "underlay-xy", false);
        c("underlay-xy",
          maybeImg1, annImage,
          maybeDx, annReal,
          maybeDy, annReal,
          maybeImg2, annImage);
        var img1 = unwrapImage(maybeImg1);
        var dx = jsnums.toFixnum(maybeDx);
        var dy = jsnums.toFixnum(maybeDy);
        var img2 = unwrapImage(maybeImg2);
        return makeImage(
          image.makeOverlayImage(img2, "left", "top", -dx, -dy, img1, "left", "top"));
      });

      f("underlay-align-list", function(maybePlaceX, maybePlaceY, maybeImgs) {
        checkArity(3, arguments, "underlay-align-list", false);
        c3("underlay-align-list", maybePlaceX, annPlaceX, maybePlaceY, annPlaceY, maybeImgs, annListImage);
        var placeX = unwrapPlaceX(maybePlaceX);
        var placeY = unwrapPlaceY(maybePlaceY);
        var imgs = unwrapListofImage(maybeImgs);
        return makeImage(imageListFoldIndex(function(acc, idx, img) {
          if (idx == 0) { return img; }
          else { return image.makeOverlayImage(img, placeX, placeY, 0, 0, acc, placeX, placeY); }
        }, imgs, image.makeSceneImage(0, 0, [], false, colorDb.get("transparent"))));
      });

      f("underlay-align", function(maybePlaceX, maybePlaceY, maybeImg1, maybeImg2) {
        checkArity(4, arguments, "underlay-align", false);
        c("underlay-align",
          maybePlaceX, annPlaceX,
          maybePlaceY, annPlaceY,
          maybeImg1, annImage,
          maybeImg2, annImage);
        var placeX = unwrapPlaceX(maybePlaceX);
        var placeY = unwrapPlaceY(maybePlaceY);
        var img1 = unwrapImage(maybeImg1);
        var img2 = unwrapImage(maybeImg2);
        return makeImage(image.makeOverlayImage(img2, placeX, placeY, 0, 0, img1, placeX, placeY));
      });

      f("beside", function(maybeImg1, maybeImg2) {
        checkArity(2, arguments, "beside", false);
        c2("beside", maybeImg1, annImage, maybeImg2, annImage);
        var img1 = unwrapImage(maybeImg1);
        var img2 = unwrapImage(maybeImg2);
        return makeImage(image.makeOverlayImage(img1, "right", "center", 0, 0, img2, "left", "center"));
      });

      f("beside-list", function(maybeImgs) {
        checkArity(1, arguments, "beside-list", false);
        c1("beside-list", maybeImgs, annListImage);
        var imgs = unwrapListofImage(maybeImgs);
        return makeImage(imageListFoldIndex(function(acc, idx, img) {
          if (idx == 0) { return img; }
          else { return image.makeOverlayImage(acc, "right", "center", 0, 0, img, "left", "center"); }
        }, imgs, image.makeSceneImage(0, 0, [], false, colorDb.get("transparent"))));
      });

      f("beside-align", function(maybePlaceY, maybeImg1, maybeImg2) {
        checkArity(3, arguments, "beside-align", false);
        c3("beside-align", maybePlaceY, annPlaceY, maybeImg1, annImage, maybeImg2, annImage);
        var placeY = unwrapPlaceY(maybePlaceY);
        var img1 = unwrapImage(maybeImg1);
        var img2 = unwrapImage(maybeImg2);
        return makeImage(image.makeOverlayImage(img1, "right", placeY, 0, 0, img2, "left", placeY));
      });

      f("beside-align-list", function(maybePlaceY, maybeImgs) {
        checkArity(2, arguments, "beside-align-list", false);
        c2("beside-align-list", maybePlaceY, annPlaceY, maybeImgs, annListImage);
        var placeY = unwrapPlaceY(maybePlaceY);
        var imgs = unwrapListofImage(maybeImgs);
        return makeImage(imageListFoldIndex(function(acc, idx, img) {
          if (idx == 0) { return img; }
          else { return image.makeOverlayImage(acc, "right", placeY, 0, 0, img, "left", placeY); }
        }, imgs, image.makeSceneImage(0, 0, [], false, colorDb.get("transparent"))));
      });

      f("above", function(maybeImg1, maybeImg2) {
        checkArity(2, arguments, "above", false);
        c2("above", maybeImg1, annImage, maybeImg2, annImage);
        var img1 = unwrapImage(maybeImg1);
        var img2 = unwrapImage(maybeImg2);
        return makeImage(image.makeOverlayImage(img1, "middle", "bottom", 0, 0, img2, "middle", "top"));
      });

      f("above-list", function(maybeImgs) {
        checkArity(1, arguments, "above-list", false);
        c1("above-list", maybeImgs, annListImage);
        var imgs = unwrapListofImage(maybeImgs);
        return makeImage(imageListFoldIndex(function(acc, idx, img) {
          if (idx == 0) { return img; }
          else { return image.makeOverlayImage(acc, "middle", "bottom", 0, 0, img, "middle", "top"); }
        }, imgs, image.makeSceneImage(0, 0, [], false, colorDb.get("transparent"))));
      });

      f("above-align", function(maybePlaceX, maybeImg1, maybeImg2) {
        checkArity(3, arguments, "above-align", false);
        c3("above-align", maybePlaceX, annPlaceX, maybeImg1, annImage, maybeImg2, annImage);
        var placeX = unwrapPlaceX(maybePlaceX);
        var img1 = unwrapImage(maybeImg1);
        var img2 = unwrapImage(maybeImg2);
        return makeImage(image.makeOverlayImage(img1, placeX, "bottom", 0, 0, img2, placeX, "top"));
      });

      f("above-align-list", function(maybePlaceX, maybeImgs) {
        checkArity(2, arguments, "above-align-list", false);
        c2("above-list", maybePlaceX, annPlaceX, maybeImgs, annListImage);
        var placeX = unwrapPlaceX(maybePlaceX);
        var imgs = unwrapListofImage(maybeImgs);
        return makeImage(imageListFoldIndex(function(acc, idx, img) {
          if (idx == 0) { return img; }
          else { return image.makeOverlayImage(acc, placeX, "bottom", 0, 0, img, placeX, "top"); }
        }, imgs, image.makeSceneImage(0, 0, [], false, colorDb.get("transparent"))));
      });
      
      f("below", function(maybeImg1, maybeImg2) {
        checkArity(2, arguments, "below", false);
        c2("below", maybeImg1, annImage, maybeImg2, annImage);
        var img1 = unwrapImage(maybeImg1);
        var img2 = unwrapImage(maybeImg2);
        return makeImage(image.makeOverlayImage(img2, "middle", "bottom", 0, 0, img1, "middle", "top"));
      });

      f("below-list", function(maybeImgs) {
        checkArity(1, arguments, "below-list", false);
        c1("below-list", maybeImgs, annListImage);
        var imgs = unwrapListofImage(maybeImgs);
        return makeImage(imageListFoldIndex(function(acc, idx, img) {
          if (idx == 0) { return img; }
          else { return image.makeOverlayImage(img, "middle", "bottom", 0, 0, acc, "middle", "top"); }
        }, imgs, image.makeSceneImage(0, 0, [], false, colorDb.get("transparent"))));
      });

      f("below-align", function(maybePlaceX, maybeImg1, maybeImg2) {
        checkArity(3, arguments, "below-align", false);
        c3("below-align", maybePlaceX, annPlaceX, maybeImg1, annImage, maybeImg2, annImage);
        var placeX = unwrapPlaceX(maybePlaceX);
        var img1 = unwrapImage(maybeImg1);
        var img2 = unwrapImage(maybeImg2);
        return makeImage(image.makeOverlayImage(img1, placeX, "bottom", 0, 0, img2, placeX, "top"));
      });

      f("below-align-list", function(maybePlaceX, maybeImgs) {
        checkArity(2, arguments, "below-align-list", false);
        c2("below-list", maybePlaceX, annPlaceX, maybeImgs, annListImage);
        var placeX = unwrapPlaceX(maybePlaceX);
        var imgs = unwrapListofImage(maybeImgs);
        return makeImage(imageListFoldIndex(function(acc, idx, img) {
          if (idx == 0) { return img; }
          else { return image.makeOverlayImage(img, placeX, "bottom", 0, 0, acc, placeX, "top"); }
        }, imgs, image.makeSceneImage(0, 0, [], false, colorDb.get("transparent"))));
      });
      
      f("move-pinhole", function(maybeDx, maybeDy, maybeImg) {
        checkArity(3, arguments, "move-pinhole", false);
        c3("move-pinhole",
           maybeDx, annReal,
           maybeDy, annReal,
           maybeImg, annImage);
        var img = unwrapImage(maybeImg);
        var dx = jsnums.toFixnum(maybeDx);
        var dy = jsnums.toFixnum(maybeDy);
        return makeImage(img.offsetPinhole(dx, dy));
      });

      f("empty-scene", function(maybeWidth, maybeHeight) {
        checkArity(2, arguments, "empty-scene", false);
        c2("empty-scene", maybeWidth, annNumNonNegative, maybeHeight, annNumNonNegative);
        var width = jsnums.toFixnum(maybeWidth);
        var height = jsnums.toFixnum(maybeHeight);
        return makeImage(
          image.makeSceneImage(width, height, [], true, colorDb.get("transparent")));
      });
      f("empty-color-scene", function(maybeWidth, maybeHeight, maybeColor) {
        checkArity(3, arguments, "empty-color-scene", false);
        c3("empty-color-scene", maybeWidth, annNumNonNegative, maybeHeight, annNumNonNegative, maybeColor, annColor);
        var width = jsnums.toFixnum(maybeWidth);
        var height = jsnums.toFixnum(maybeHeight);
        var color = unwrapColor(maybeColor);
        return makeImage(
          image.makeSceneImage(width, height, [], true, color));
      });
      f("put-image", function(maybePicture, maybeX, maybeY, maybeBackground) {
        checkArity(4, arguments, "put-image", false);
        c("put-image",
          maybePicture, annImage,
          maybeX, annReal,
          maybeY, annReal,
          maybeBackground, annImageOrScene);
        var picture = unwrapImage(maybePicture);
        var x = jsnums.toFixnum(maybeX);
        var y = jsnums.toFixnum(maybeY);
        var background = unwrapImageOrScene(maybeBackground);
        if (image.isScene(background)) {
          return makeImage(background.add(picture, x, background.getHeight() - y));
        } else {
          var newScene = image.makeSceneImage(background.getWidth(), background.getHeight(), [], false, colorDb.get("transparent"));
          newScene = newScene.add(background, background.getWidth()/2, background.getHeight()/2);
          newScene = newScene.add(picture, x, background.getHeight() - y);
          return makeImage(newScene);
        }
      });
      f("place-image", function(maybePicture, maybeX, maybeY, maybeBackground) {
        checkArity(4, arguments, "place-image", false);
        c("place-image",
          maybePicture, annImage,
          maybeX, annReal,
          maybeY, annReal,
          maybeBackground, annImageOrScene);
        var picture = unwrapImage(maybePicture);
        var x = jsnums.toFixnum(maybeX);
        var y = jsnums.toFixnum(maybeY);
        var background = unwrapImageOrScene(maybeBackground);
        if (image.isScene(background)) {
          return makeImage(background.add(picture, x, y));
        } else {
          var newScene = image.makeSceneImage(background.getWidth(), background.getHeight(), [], false, colorDb.get("transparent"));
          newScene = newScene.add(background, background.getWidth()/2, background.getHeight()/2);
          newScene = newScene.add(picture, x, y);
          return makeImage(newScene);
        }
      });
      f("translate", values["place-image"].app);

      f("place-pinhole", function(maybeX, maybeY, maybeImg) {
        checkArity(3, arguments, "place-pinhole", false);
        c3("place-pinhole",
           maybeX, annReal,
           maybeY, annReal,
           maybeImg, annImage);
        var img = unwrapImage(maybeImg);
        var x = jsnums.toFixnum(maybeX);
        var y = jsnums.toFixnum(maybeY);
        return makeImage(img.updatePinhole(x, y));
      });
      f("center-pinhole", function(maybeImg) {
        checkArity(1, arguments, "center-pinhole", false);
        c1("center-pinhole", maybeImg, annImage);
        var img = unwrapImage(maybeImg);
        return makeImage(img.updatePinhole(img.getWidth() / 2, img.getHeight() / 2));
      });

      f("place-image-align", function(maybeImg, maybeX, maybeY, maybePlaceX, maybePlaceY, maybeBackground) {
        checkArity(6, arguments, "place-image-align", false);
        c("place-image-align",
          maybeImg, annImage,
          maybeX, annReal,
          maybeY, annReal,
          maybePlaceX, annPlaceX,
          maybePlaceY, annPlaceY,
          maybeBackground, annImageOrScene);
        var img = unwrapImage(maybeImg);
        var x = jsnums.toFixnum(maybeX);
        var y = jsnums.toFixnum(maybeY);
        var placeX = unwrapPlaceX(maybePlaceX);
        var placeY = unwrapPlaceY(maybePlaceY);
        var background = unwrapImageOrScene(maybeBackground);
        if      (placeX == "left"   ) { x = x + img.getWidth()/2; }
        else if (placeX == "right"  ) { x = x - img.getWidth()/2; }
        else if (placeX == "pinhole") { x = x + img.getWidth()/2 - img.getPinholeX();    }
        if      (placeY == "top"    ) { y = y + img.getHeight()/2; }
        else if (placeY == "bottom" ) { y = y - img.getHeight()/2; }
        else if (placeY == "pinhole") { y = y + img.getHeight()/2 - img.getPinholeY();    }

        if (image.isScene(background)) {
          return makeImage(background.add(img, x, y));
        } else {
          var newScene = image.makeSceneImage(background.getWidth(),
                                              background.getHeight(),
                                              [],
                                              false,
                                              colorDb.get("transparent"));
          newScene = newScene.add(background, background.getWidth()/2, background.getHeight()/2);
          newScene = newScene.add(img, x, y);
          return makeImage(newScene);
        }
      });

      f("rotate", function(maybeAngle, maybeImg) {
        checkArity(2, arguments, "rotate", false);
        c2("rotate", maybeAngle, annReal, maybeImg, annImage);
        var angle = jsnums.toFixnum(canonicalizeAngle(maybeAngle));
        var img = unwrapImage(maybeImg);
        return makeImage(image.makeRotateImage(-angle, img));
      });

      f("scale", function(maybeFactor, maybeImg) {
        checkArity(2, arguments, "scale", false);
        c2("scale", maybeFactor, annReal, maybeImg, annImage);
        var factor = jsnums.toFixnum(maybeFactor);
        var img = unwrapImage(maybeImg);
        return makeImage(image.makeScaleImage(factor, factor, img));
      });

      f("scale-xy", function(maybeXFactor, maybeYFactor, maybeImg) {
        checkArity(3, arguments, "scale-xy", false);
        c3("scale-xy", maybeXFactor, annReal, maybeYFactor, annReal, maybeImg, annImage);
        var xFactor = jsnums.toFixnum(maybeXFactor);
        var yFactor = jsnums.toFixnum(maybeYFactor);
        var img = unwrapImage(maybeImg);
        return makeImage(image.makeScaleImage(xFactor, yFactor, img));
      });

      f("flip-horizontal", function(maybeImg) {
        checkArity(1, arguments, "flip-horizontal", false);
        c1("flip-horizontal", maybeImg, annImage);
        var img = unwrapImage(maybeImg);
        return makeImage(image.makeFlipImage(img, "horizontal"));
      });

      f("flip-vertical", function(maybeImg) {
        checkArity(1, arguments, "flip-vertical", false);
        c1("flip-vertical", maybeImg, annImage);
        var img = unwrapImage(maybeImg);
        return makeImage(image.makeFlipImage(img, "vertical"));
      });
      // aliases
      f("reflect-y", values["flip-horizontal"].app);
      f("reflect-x", values["flip-vertical"].app);

      f("frame", function(maybeImg) {
        checkArity(1, arguments, "frame", false);
        c1("frame", maybeImg, annImage);
        var img = unwrapImage(maybeImg);
        return makeImage(image.makeFrameImage(img));
      });

      f("draw-pinhole", function(maybeImg) {
        checkArity(1, arguments, "draw-pinhole", false);
        c1("draw-pinhole", maybeImg, annImage);
        var img = unwrapImage(maybeImg);
        return makeImage(image.makePinholeImage(img));
      });

      f("crop", function(maybeX, maybeY, maybeWidth, maybeHeight, maybeImg) {
        checkArity(5, arguments, "crop", false);
        c("crop",
          maybeX, annReal,
          maybeY, annReal,
          maybeWidth, annNumNonNegative,
          maybeHeight, annNumNonNegative,
          maybeImg, annImage);
        var x = jsnums.toFixnum(maybeX);
        var y = jsnums.toFixnum(maybeY);
        var width = jsnums.toFixnum(maybeWidth);
        var height = jsnums.toFixnum(maybeHeight);
        var img = unwrapImage(maybeImg);
        return makeImage(image.makeCropImage(x, y, width, height, img));
      });

      f("line", function(maybeX, maybeY, maybeC) {
        checkArity(3, arguments, "line", false);
        c3("line", maybeX, annReal, maybeY, annReal, maybeC, annColor);
        var x = jsnums.toFixnum(maybeX);
        var y = jsnums.toFixnum(maybeY);
        var color = unwrapColor(maybeC);
        return makeImage(
          image.makeLineImage(x, y, color));
      });

      f("add-line", function(maybeImg, maybeX1, maybeY1, maybeX2, maybeY2, maybeC) {
        checkArity(6, arguments, "add-line", false);
        c("add-line",
          maybeImg, annImage,
          maybeX1, annReal,
          maybeY1, annReal,
          maybeX2, annReal,
          maybeY2, annReal,
          maybeC, annColor);
        var x1 = jsnums.toFixnum(maybeX1);
        var y1 = jsnums.toFixnum(maybeY1);
        var x2 = jsnums.toFixnum(maybeX2);
        var y2 = jsnums.toFixnum(maybeY2);
        var color = unwrapColor(maybeC);
        var img   = unwrapImage(maybeImg);
        var line  = image.makeLineImage(x2 - x1, y2 - y1, color);
        return makeImage(image.makeOverlayImage(line, "left", "top", -Math.min(x1,x2), -Math.min(y1,y2), img, "left", "top"));
      });

      f("scene-line", function(maybeImg, maybeX1, maybeY1, maybeX2, maybeY2, maybeC) {
        checkArity(6, arguments, "scene-line", false);
        c("scene-line",
          maybeImg, annImage,
          maybeX1, annReal,
          maybeY1, annReal,
          maybeX2, annReal,
          maybeY2, annReal,
          maybeC, annColor);
        var x1 = jsnums.toFixnum(maybeX1);
        var y1 = jsnums.toFixnum(maybeY1);
        var x2 = jsnums.toFixnum(maybeX2);
        var y2 = jsnums.toFixnum(maybeY2);
        var color = unwrapColor(maybeC);
        var img = unwrapImage(maybeImg);
        var line = image.makeLineImage(x2 - x1, y2 - y1, color);

        var newScene = image.makeSceneImage(img.getWidth(),
                                            img.getHeight(),
                                            [],
                                            true,
                                            colorDb.get("transparent"));
        newScene = newScene.add(img, img.getWidth()/2, img.getHeight()/2);
        var leftMost = Math.min(x1,x2);
        var topMost = Math.min(y1,y2);
        return makeImage(newScene.add(line, line.getWidth()/2+leftMost, line.getHeight()/2+topMost));
      });

      f("square", function(maybeSide, maybeMode, maybeColor) {
        checkArity(3, arguments, "square", false);
        c3("square", maybeSide, annNumNonNegative, maybeMode, annMode, maybeColor, annColor);
        var side = jsnums.toFixnum(maybeSide);
        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        return makeImage(image.makeSquareImage(side, mode, color));
      });

      f("rectangle", function(maybeWidth, maybeHeight, maybeMode, maybeColor) {
        checkArity(4, arguments, "rectangle", false);
        c("rectangle",
          maybeWidth, annNumNonNegative,
          maybeHeight, annNumNonNegative,
          maybeMode, annMode,
          maybeColor, annColor);
        var width = jsnums.toFixnum(maybeWidth);
        var height = jsnums.toFixnum(maybeHeight);
        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        return makeImage(
          image.makeRectangleImage(width, height, mode, color));
      });

      f("regular-polygon", function(maybeLength, maybeCount, maybeMode, maybeColor) {
        checkArity(4, arguments, "regular-polygon", false);
        c("regular-polygon",
          maybeLength, annNumNonNegative,
          maybeCount, annSideCount,
          maybeMode, annMode,
          maybeColor, annColor);
        var length = jsnums.toFixnum(maybeLength);
        var count = jsnums.toFixnum(maybeCount);
        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        return makeImage(
          image.makeRegularPolygonImage(length, count, 1, mode, color, true));
      });

      f("point-polygon", function(maybePoints, maybeMode, maybeColor) {
        checkArity(3, arguments, "point-polygon", false);
        c("point-polygon",
          maybePoints, annListPoint2D,
          maybeMode, annMode,
          maybeColor, annColor);
        var points = unwrapListofPoint2D(maybePoints);
        if (points.length < 3) {
          throwMessage("There must be at least three points to make a polygon.");
        } else {
          var mode = unwrapMode(maybeMode);
          var color = unwrapColor(maybeColor);
          return makeImage(
            image.makePointPolygonImage(points, mode, color));
        }
      });

      f("ellipse", function(maybeWidth, maybeHeight, maybeMode, maybeColor) {
        checkArity(4, arguments, "ellipse", false);
        c("ellipse",
          maybeWidth, annNumNonNegative,
          maybeHeight, annNumNonNegative,
          maybeMode, annMode,
          maybeColor, annColor);
        var width = jsnums.toFixnum(maybeWidth);
        var height = jsnums.toFixnum(maybeHeight);
        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        return makeImage(
          image.makeEllipseImage(width, height, mode, color));
      });

      f("wedge", function(maybeRadius, maybeAngle, maybeMode, maybeColor) {
        checkArity(4, arguments, "wedge", false);
        c("wedge",
          maybeRadius, annNumNonNegative,
          maybeAngle, annAngle,
          maybeMode, annMode,
          maybeColor, annColor);
        var radius = jsnums.toFixnum(maybeRadius);
        var angle = jsnums.toFixnum(maybeAngle);
        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        return makeImage(
          image.makeWedgeImage(radius, angle, mode, color));
      });

      f("triangle", function(maybeSide, maybeMode, maybeColor) {
        checkArity(3, arguments, "triangle", false);
        c3("triangle", maybeSide, annNumNonNegative, maybeMode, annMode, maybeColor, annColor);
        var side = jsnums.toFixnum(maybeSide);
        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        return makeImage(
          // Angle makes triangle point up
          image.makeTriangleImage(side, 360-60, side, mode, color));
      });

      f("triangle-sas", function(maybeSideA, maybeAngleB, maybeSideC, maybeMode, maybeColor) {
        checkArity(5, arguments, "triangle-sas", false);
        c("triangle-sas",
          maybeSideA, annNumNonNegative,
          maybeAngleB, annAngle,
          maybeSideC, annNumNonNegative,
          maybeMode, annMode,
          maybeColor, annColor);
        var sideA = jsnums.toFixnum(maybeSideA);
        var angleB = jsnums.toFixnum(maybeAngleB);
        var sideC = jsnums.toFixnum(maybeSideC);

        var sideB2 = cosRel(sideA, sideC, angleB);
        var sideB  = Math.sqrt(sideB2);

        if (sideB2 <= 0) {
          throwMessage("The given side, angle and side will not form a triangle: "
                       + maybeSideA + ", " + maybeAngleB + ", " + maybeSideC);
        } else {
          if (less(sideA + sideC, sideB) ||
              less(sideB + sideC, sideA) ||
              less(sideA + sideB, sideC)) {
            throwMessage("The given side, angle and side will not form a triangle: "
                         + maybeSideA + ", " + maybeAngleB + ", " + maybeSideC);
          } else {
            if (less(sideA + sideC, sideB) ||
                less(sideB + sideC, sideA) ||
                less(sideA + sideB, sideC)) {
              throwMessage("The given side, angle and side will not form a triangle: "
                           + maybeSideA + ", " + maybeAngleB + ", " + maybeSideC);
            }
          }
        }

        var angleA = Math.acos(excess(sideB, sideC, sideA) / (2 * sideB * sideC)) * (180 / Math.PI);

        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        return makeImage(
          image.makeTriangleImage(sideC, angleA, sideB, mode, color));
      });

      f("triangle-sss", function(maybeSideA, maybeSideB, maybeSideC, maybeMode, maybeColor) {
        checkArity(5, arguments, "triangle-sss", false);
        c("triangle-sss",
          maybeSideA, annNumNonNegative,
          maybeSideB, annNumNonNegative,
          maybeSideC, annNumNonNegative,
          maybeMode, annMode,
          maybeColor, annColor);
        var sideA = jsnums.toFixnum(maybeSideA);
        var sideB = jsnums.toFixnum(maybeSideB);
        var sideC = jsnums.toFixnum(maybeSideC);
        if (less(sideA + sideB, sideC) ||
            less(sideC + sideB, sideA) ||
            less(sideA + sideC, sideB)) {
          throwMessage("The given sides will not form a triangle: "
                       + maybeSideA + ", " + maybeSideB + ", " + maybeSideC);
        }

        var angleA = Math.acos(excess(sideB, sideC, sideA) / (2 * sideB * sideC)) * (180 / Math.PI);

        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        return makeImage(
          image.makeTriangleImage(sideC, angleA, sideB, mode, color));
      });

      f("triangle-ass", function(maybeAngleA, maybeSideB, maybeSideC, maybeMode, maybeColor) {
        checkArity(5, arguments, "triangle-ass", false);
        c("triangle-ass",
          maybeAngleA, annAngle,
          maybeSideB, annNumNonNegative,
          maybeSideC, annNumNonNegative,
          maybeMode, annMode,
          maybeColor, annColor);
        var angleA = jsnums.toFixnum(maybeAngleA);
        var sideB = jsnums.toFixnum(maybeSideB);
        var sideC = jsnums.toFixnum(maybeSideC);
        if (less(180, angleA)) {
          throwMessage("The given angle, side and side will not form a triangle: "
                       + maybeAngleA + ", " + maybeSideB + ", " + maybeSideC);
        }
        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        return makeImage(
          image.makeTriangleImage(sideC, angleA, sideB, mode, color));
      });

      f("triangle-ssa", function(maybeSideA, maybeSideB, maybeAngleC, maybeMode, maybeColor) {
        checkArity(5, arguments, "triangle-ssa", false);
        c("triangle-ssa",
          maybeSideA, annNumNonNegative,
          maybeSideB, annNumNonNegative,
          maybeAngleC, annAngle,
          maybeMode, annMode,
          maybeColor, annColor);
        var sideA  = jsnums.toFixnum(maybeSideA);
        var sideB  = jsnums.toFixnum(maybeSideB);
        var angleC = jsnums.toFixnum(maybeAngleC);
        if (less(180, angleC)) {
          throwMessage("The given side, side and angle will not form a triangle: "
                       + sideA + ", " + sideB + ", " + angleC);
        }
        var sideC2 = cosRel(sideA, sideB, angleC);
        var sideC  = Math.sqrt(sideC2);

        if (sideC2 <= 0) {
          throwMessage("The given side, side and angle will not form a triangle: "
                       + maybeSideA + ", " + maybeSideB + ", " + maybeAngleC);
        } else {
          if (less(sideA + sideB, sideC) ||
              less(sideC + sideB, sideA) ||
              less(sideA + sideC, sideB)) {
            throwMessage("The given side, side and angle will not form a triangle: "
                         + maybeSideA + ", " + maybeSideB + ", " + maybeAngleC);
          }
        }

        var angleA = Math.acos(excess(sideB, sideC, sideA) / (2 * sideB * sideC)) * (180 / Math.PI);

        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        return makeImage(
          image.makeTriangleImage(sideC, angleA, sideB, mode, color));
      });

      f("triangle-aas", function(maybeAngleA, maybeAngleB, maybeSideC, maybeMode, maybeColor) {
        checkArity(5, arguments, "triangle-aas", false);
        c("triangle-aas",
          maybeAngleA, annAngle,
          maybeAngleB, annAngle,
          maybeSideC, annNumNonNegative,
          maybeMode, annMode,
          maybeColor, annColor);
        var angleA = jsnums.toFixnum(maybeAngleA);
        var angleB = jsnums.toFixnum(maybeAngleB);
        var sideC = jsnums.toFixnum(maybeSideC);
        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        var angleC = (180 - angleA - angleB);
        if (less(angleC, 0)) {
          throwMessage("The given angle, angle and side will not form a triangle: "
                       + maybeAngleA + ", " + maybeAngleB + ", " + maybeSideC);
        }
        var hypotenuse = sideC / (Math.sin(angleC*Math.PI/180))
        var sideB = hypotenuse * Math.sin(angleB*Math.PI/180);
        return makeImage(
          image.makeTriangleImage(sideC, angleA, sideB, mode, color));
      });

      f("triangle-asa", function(maybeAngleA, maybeSideB, maybeAngleC, maybeMode, maybeColor) {
        checkArity(5, arguments, "triangle-asa", false);
        c("triangle-asa",
          maybeAngleA, annAngle,
          maybeSideB, annNumNonNegative,
          maybeAngleC, annAngle,
          maybeMode, annMode,
          maybeColor, annColor);
        var angleA = jsnums.toFixnum(maybeAngleA);
        var sideB = jsnums.toFixnum(maybeSideB);
        var angleC = jsnums.toFixnum(maybeAngleC);
        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        var angleB = 180 - angleA - angleC;
        if (less(angleB, 0)) {
          throwMessage("The given angle, side and angle will not form a triangle: "
                       + maybeAngleA + ", " + maybeSideB + ", " + maybeAngleC);
        }
        var base = (sideB * Math.sin(angleA*Math.PI/180)) / (Math.sin(angleB*Math.PI/180));
        var sideC = (sideB * Math.sin(angleC*Math.PI/180)) / (Math.sin(angleB*Math.PI/180));
        return makeImage(
          image.makeTriangleImage(sideC, angleA, sideB, mode, color));
      });

      f("triangle-saa", function(maybeSideA, maybeAngleB, maybeAngleC, maybeMode, maybeColor) {
        checkArity(5, arguments, "triangle-saa", false);
        c("triangle-saa",
          maybeSideA, annNumNonNegative,
          maybeAngleB, annAngle,
          maybeAngleC, annAngle,
          maybeMode, annMode,
          maybeColor, annColor);
        var sideA = jsnums.toFixnum(maybeSideA);
        var angleB = jsnums.toFixnum(maybeAngleB);
        var angleC = jsnums.toFixnum(maybeAngleC);
        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        var angleA = (180 - angleC - angleB);
        var hypotenuse = sideA / (Math.sin(angleA*Math.PI/180));
        var sideC = hypotenuse * Math.sin(angleC*Math.PI/180);
        var sideB = hypotenuse * Math.sin(angleB*Math.PI/180);
        return makeImage(
          image.makeTriangleImage(sideC, angleA, sideB, mode, color));
      });

      f("right-triangle", function(maybeSide1, maybeSide2, maybeMode, maybeColor) {
        checkArity(4, arguments, "right-triangle", false);
        c("right-triangle",
          maybeSide1, annNumNonNegative,
          maybeSide2, annNumNonNegative,
          maybeMode, annMode,
          maybeColor, annColor);
        var side1 = jsnums.toFixnum(maybeSide1);
        var side2 = jsnums.toFixnum(maybeSide2);
        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        return makeImage(
          // add 180 to make the triangle point up
          image.makeTriangleImage(side1, 360 - 90, side2, mode, color));
      });

      f("isosceles-triangle", function(maybeSide, maybeAngleC, maybeMode, maybeColor) {
        checkArity(4, arguments, "isosceles-triangle", false);
        c("isosceles-triangle",
          maybeSide, annNumNonNegative,
          maybeAngleC, annAngle,
          maybeMode, annMode,
          maybeColor, annColor);
        var side = jsnums.toFixnum(maybeSide);
        var angleC = jsnums.toFixnum(maybeAngleC);
        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        var angleAB = (180-angleC)/2;
        var base = 2*side*Math.sin((angleC*Math.PI/180)/2);
        return makeImage(
          // add 180 to make the triangle point up
          image.makeTriangleImage(base, 360 - angleAB, side, mode, color));
      });

      f("star", function(maybeSide, maybeMode, maybeColor) {
        checkArity(3, arguments, "star", false);
        c3("star", maybeSide, annNumNonNegative, maybeMode, annMode, maybeColor, annColor);
        var side = jsnums.toFixnum(maybeSide);
        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        return makeImage(
          image.makeRegularPolygonImage(side, 5, 2, mode, color, false));
      });
      // TODO: This was split from the variable-arity case in the original whalesong "star" function
      f("star-sized", function(maybePointCount, maybeOuter, maybeInner, maybeMode, maybeColor) {
        checkArity(5, arguments, "star-sized", false);
        c("star-sized",
          maybePointCount, annPointCount,
          maybeOuter, annNumNonNegative,
          maybeInner, annNumNonNegative,
          maybeMode, annMode,
          maybeColor, annColor);
        var pointCount = jsnums.toFixnum(maybePointCount);
        var outer = jsnums.toFixnum(maybeOuter);
        var inner = jsnums.toFixnum(maybeInner);
        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        return makeImage(
          image.makeStarImage(pointCount, inner, outer, mode, color));
      });
      // alias
      f("radial-star", values["star-sized"].app);

      f("star-polygon", function(maybeLength, maybeCount, maybeStep, maybeMode, maybeColor) {
        checkArity(5, arguments, "star-polygon", false);
        c("star-polygon",
          maybeLength, annNumNonNegative,
          maybeCount, annSideCount,
          maybeStep, annStepCount,
          maybeMode, annMode,
          maybeColor, annColor);
        var length = jsnums.toFixnum(maybeLength);
        var count = jsnums.toFixnum(maybeCount);
        var step = jsnums.toFixnum(maybeStep);
        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        return makeImage(
          image.makeRegularPolygonImage(length, count, step, mode, color, false));
      });

      f("rhombus", function(maybeLength, maybeAngle, maybeMode, maybeColor) {
        checkArity(4, arguments, "rhombus", false);
        c("rhombus",
          maybeLength, annNumNonNegative,
          maybeAngle, annAngle,
          maybeMode, annMode,
          maybeColor, annColor);
        var length = jsnums.toFixnum(maybeLength);
        var angle = jsnums.toFixnum(maybeAngle);
        var mode = unwrapMode(maybeMode);
        var color = unwrapColor(maybeColor);
        return makeImage(
          image.makeRhombusImage(length, angle, mode, color));
      });

      f("trim-image", function(maybeImage) {
        checkArity(1, arguments, "trim-image", false);
        c1("trim-image", maybeImage, annImage);
        var img = unwrapImage(maybeImage);
        var canvas = image.trimImageToCanvas(img);
        if (canvas.width === 0 || canvas.height === 0) {
          return makeImage(
            image.makeRectangleImage(canvas.width, canvas.height, "solid", colorDb.get("transparent")));
        }
        return makeImage(
          image.makeImageDataImage(canvas.getContext('2d').getImageData(0, 0, canvas.width, canvas.height)));
      });

      f("image-to-color-list", function(maybeImage) {
        checkArity(1, arguments, "image-to-color-list", false);
        c1("image-to-color-list", maybeImage, annImage);
        var img = unwrapImage(maybeImage);
        return image.imageToColorList(img);
      });
      
      f("image-width", function(maybeImg) {
        checkArity(1, arguments, "image-width", false);
        c1("image-width", maybeImg, annImage);
        var img = unwrapImage(maybeImg);
        return runtime.wrap(img.getWidth());
      });

      f("image-height", function(maybeImg) {
        checkArity(1, arguments, "image-height", false);
        c1("image-height", maybeImg, annImage);
        var img = unwrapImage(maybeImg);
        return runtime.wrap(img.getHeight());
      });

      f("image-baseline", function(maybeImg) {
        checkArity(1, arguments, "image-baseline", false);
        c1("image-baseline", maybeImg, annImage);
        var img = unwrapImage(maybeImg);
        return runtime.wrap(img.getBaseline());
      });

      f("image-pinhole-x", function(maybeImg) {
        checkArity(1, arguments, "image-pinhole-x", false);
        c1("image-pinhole-x", maybeImg, annImage);
        var img = unwrapImage(maybeImg);
        return runtime.wrap(img.getPinholeX());
      });

      f("image-pinhole-y", function(maybeImg) {
        checkArity(1, arguments, "image-pinhole-y", false);
        c1("image-pinhole-y", maybeImg, annImage);
        var img = unwrapImage(maybeImg);
        return runtime.wrap(img.getPinholeY());
      });

      f("color-at-position", function(maybeImage, maybeX, maybeY) {
        checkArity(3, arguments, "color-at-position", false);
        c3("color-at-position", maybeImage, annImage, maybeX, annNatural, maybeY, annNatural);
        var img = unwrapImage(maybeImage);
        var width = img.getWidth();
        var height = img.getHeight();
        if(maybeX >= width) {
          throwMessage("color-at-position: The given x coordinate, " + maybeX + ", must be between 0 (inclusive) and the image width (exclusive), which is " + img.getWidth());
        }
        if(maybeY >= height) {
          throwMessage("color-at-position: The given y coordinate, " + maybeY + ", must be between 0 (inclusive) and the image height (exclusive), which is " + img.getHeight());

        }
        return image.colorAtPosition(img, maybeX, maybeY);
      });

      f("color-list-to-image", function(maybeList, maybeWidth, maybeHeight, maybePinholeX, maybePinholeY) {
        checkArity(5, arguments, "color-list-to-image", false);
        c("color-list-to-image",
          maybeList, annListColor,
          maybeWidth, annNatural,
          maybeHeight, annNatural,
          maybePinholeX, annNatural,
          maybePinholeY, annNatural);
        var len = ffi.listLength(maybeList);
        var loc = unwrapListofColor(maybeList);
        var width = jsnums.toFixnum(maybeWidth);
        var height = jsnums.toFixnum(maybeHeight);
        if (len != width * height) {
          throwMessage("The color list does not have the right number of elements: " +
                       "expected " + (width * height) + " but got " + len);
        }
        var pinholeX = jsnums.toFixnum(maybePinholeX);
        var pinholeY = jsnums.toFixnum(maybePinholeY);
        if (width === 0 || height === 0) {
          return makeImage(image.makeRectangleImage(width, height, "solid", colorDb.get("transparent")));
        }
        return makeImage(image.colorListToImage(loc, width, height, pinholeX, pinholeY));
      });

      f("color-list-to-bitmap", function(maybeList, maybeWidth, maybeHeight) {
        checkArity(3, arguments, "color-list-to-bitmap", false);
        c3("color-list-to-image", maybeList, annListColor, maybeWidth, annNatural, maybeHeight, annNatural);
        var len = ffi.listLength(maybeList);
        var loc = unwrapListofColor(maybeList);
        var width = jsnums.toFixnum(maybeWidth);
        var height = jsnums.toFixnum(maybeHeight);
        if (len != width * height) {
          throwMessage("The color list does not have the right number of elements: " +
                       "expected " + (width * height) + " but got " + len);
        }
        return makeImage(image.colorListToImage(loc, width, height, width / 2, height / 2));
      });

      f("name-to-color", function(maybeName) {
        checkArity(1, arguments, "name-to-color", false);
        c1("name-to-color", maybeName, runtime.String);
        var name = maybeName;
        var c = colorDb.get(String(name));
        if (c) {
          return runtime.ffi.makeSome(c);
        } else {
          return runtime.ffi.makeNone();
        }
      });
      f("color-named", function(maybeName) {
        checkArity(1, arguments, "color-named", false);
        c1("color-named", maybeName, runtime.String);
        var name = maybeName;
        var val = colorDb.get(String(name));
        if (val) {
          return runtime.wrap(val);
        }
        throwMessage("Unknown color name '" + String(name) + "'");
      });
      
      values["empty-image"] = runtime.makeOpaque(image.makeSceneImage(0, 0, [], true, colorDb.get("transparent")));

      return values;
    }
    return runtime.makeJSModuleReturn({
      makeImageLib: makeImageLib
    });
  }
})
