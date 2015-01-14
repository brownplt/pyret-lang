define([
    "trove/image-structs",
    "js/ffi-helpers",
    "js/js-numbers"
  ], function(
      imageLib,
      ffiLib,
      jsnums
    ) {
  // Basic implementation of the image library.
  //
  // This should mimic the implementation of 2htdp/image.

  return function(runtime, namespace) {
    var gf = runtime.getField;
    return runtime.loadJSModules(namespace, [ffiLib], function(ffi) {
      return runtime.loadModulesNew(namespace, [imageLib], function(imageImp) {
        var image = gf(imageImp, "values");
        var color = gf(image, "color");
        var colorPred = gf(image, "Color");
        var isNum = function(n) { return typeof n === "number"; }
        var unwrap = runtime.unwrap;

        var hasOwnProperty = {}.hasOwnProperty;

        // clone: object -> object
        // Copies an object.  The new object should respond like the old
        // object, including to things like instanceof.

        // NOTE(joe): There are much better ways to do this.  This is from
        // whalesong/whalesong/js-assembler/runtime-src/baselib.js
        // and we're keeping it for now (March 31, 2014) to avoid changing
        // potentially fragile prototype semantics
        var clone = function (obj) {
            var property;
            var C = function () {};
            C.prototype = obj;
            var c = new C();
            for (property in obj) {
                if (hasOwnProperty.call(obj, property)) {
                    c[property] = obj[property];
                }
            }
            return c;
        };

        //////////////////////////////////////////////////////////////////////
        var makeColor = function(r,g,b,a) {
          if (a === undefined) { a = 255; }
          if ([r,g,b,a].filter(isNum).length !== 4) {
            throw new Error("Internal error: non-number in makeColor argList ", [r, g, b, a]);
          }
          return color.app(
              runtime.wrap(r),
              runtime.wrap(g),
              runtime.wrap(b),
              runtime.wrap(a)
            );
        };
        var isColor = function(c) { return unwrap(colorPred.app(c)); };
        var colorRed = function(c) { return unwrap(gf(c, "red")); }
        var colorGreen = function(c) { return unwrap(gf(c, "green")); };
        var colorBlue = function(c) { return unwrap(gf(c, "blue")); };
        var colorAlpha = function(c) { return unwrap(gf(c, "alpha")); };
        var equals = runtime.equal_always;

        var imageEquals = function(left, right) {
          if (!isImage(left) || !isImage(right)) { return false; }
          return left.equals(right);
        }
        //////////////////////////////////////////////////////////////////////

        var heir = Object.create;

        var isAngle = function(x) {
            return jsnums.isReal(x) &&
                jsnums.greaterThanOrEqual(x, 0) &&
                jsnums.lessThan(x, 360);
        };

        // Produces true if the value is a color or a color string.
        // On the Racket side of things, this is exposed as image-color?.
        var isColorOrColorString = function(thing) {
            return (isColor(thing) ||
                    ((runtime.isString(thing) &&
                      typeof(colorDb.get(thing)) != 'undefined')));
        };

        //////////////////////////////////////////////////////////////////////
        // colorString : hexColor Style -> rgba
        // Style can be "solid" (1.0), "outline" (1.0), a number (0-1.0) or null (1.0)
        var colorString = function(aColor, aStyle) {
          var alpha = isNaN(aStyle)? 1.0 : aStyle/255;
          return "rgba(" + colorRed(aColor) + "," +
                          colorGreen(aColor) + ", " +
                          colorBlue(aColor) + ", " +
                          alpha + ")";
        };

        var isSideCount = function(x) {
            return jsnums.isInteger(x) && jsnums.greaterThanOrEqual(x, 3);
        };

        var isStepCount = function(x) {
            return jsnums.isInteger(x) && jsnums.greaterThanOrEqual(x, 1);
        };

        var isPointsCount = function(x) {
            return jsnums.isInteger(x) && jsnums.greaterThanOrEqual(x, 2);
        };

        // Produces true if thing is an image-like object.
        var isImage = function(thing) {
            if (typeof(thing.getHeight) !== 'function')
                return false;
            if (typeof(thing.getWidth) !== 'function')
                return false;
            if (typeof(thing.getBaseline) !== 'function')
                return false;
            if (typeof(thing.updatePinhole) !== 'function')
                return false;
            if (typeof(thing.render) !== 'function')
                return false;
            return true;
        };

        // given two arrays of {x,y} structs, determine their equivalence
        var verticesEqual = function(v1, v2){
          if(v1.length !== v2.length){ return false; }
          for(var i=0; i< v1.length; i++){
            if(v1[i].x !== v2[i].x || v1[i].y !== v2[i].y){ return false; }
          }
          return true;
        };
        // given two arrays of xs and ys, zip them into a vertex array
        var zipVertices = function(xs, ys){
          if(xs.length !== ys.length){throw new Error('failure in zipVertices');}
          var vertices = [];
          for(var i=0; i<xs.length;i++){
            vertices.push({x: xs[i], y: ys[i]});
          }
          return vertices;
        };

        // Base class for all images.
        var BaseImage = function() {};

        BaseImage.prototype.updatePinhole = function(x, y) {
            var aCopy = clone(this);
            aCopy.pinholeX = x;
            aCopy.pinholeY = y;
            return aCopy;
        };

        BaseImage.prototype.getHeight = function(){
            return this.height;
        };

        BaseImage.prototype.getWidth = function(){
            return this.width;
        };

        BaseImage.prototype.getBaseline = function(){
            return this.height;
        };

        // return the vertex array if it exists, otherwise make one using height and width
        BaseImage.prototype.getVertices = function(){
          if(this.vertices){ return this.vertices; }
          else{ return [{x:0 , y: 0},
                        {x: this.width, y: 0},
                        {x: 0, y: this.height},
                        {x: this.width, y: this.height}]; }
        };

        // render: context fixnum fixnum: -> void
        // Render the image, where the upper-left corner of the image is drawn at
        // (x, y).
        // If the image isn't vertex-based, throw an error
        // Otherwise, stroke and fill the vertices.
        BaseImage.prototype.render = function(ctx, x, y) {
          if(!this.vertices){
            throw new Error('BaseImage.render is not implemented for this type!');
          }
          ctx.save();
          ctx.beginPath();
          ctx.moveTo(x+this.vertices[0].x, y+this.vertices[0].y);
          for(var i=1; i < this.vertices.length; i++){
            ctx.lineTo(x+this.vertices[i].x, y+this.vertices[i].y);
          }
          ctx.closePath();

          if (this.style.toString().toLowerCase() === "outline") {
            ctx.strokeStyle = colorString(this.color);
            ctx.stroke();
          } else {
            ctx.fillStyle = colorString(this.color, this.style);
            ctx.fill();
          }
          ctx.restore();
        };

        // makeCanvas: number number -> canvas
        // Constructs a canvas object of a particular width and height.
        var makeCanvas = function(width, height) {
            var canvas = document.createElement("canvas");
            canvas.width = width;
            canvas.height = height;

            jQuery(canvas).css('width', canvas.width + "px");
            jQuery(canvas).css('height', canvas.height + "px");
            jQuery(canvas).css('padding', '0px');

            // KLUDGE: IE compatibility uses /js/excanvas.js, and dynamic
            // elements must be marked this way.
            if (window.G_vmlCanvasManager) {
          canvas = window.G_vmlCanvasManager.initElement(canvas);
            }
            return canvas;
        };

        var withIeHack = function(canvas, f) {
            // 	canvas.style.display = 'none';
            // 	document.body.appendChild(canvas);
            // 	try {
            var result = f(canvas);
            // 	} catch(e) {
            // 	    document.body.removeChild(canvas);
            // 	    canvas.style.display = '';
            // 	    throw e;
            // 	}
            // 	document.body.removeChild(canvas);
            // 	canvas.style.display = '';
            return result;
        };

        // Images are expected to define a render() method, which is used
        // here to draw to the canvas.
        BaseImage.prototype.toDomNode = function(params) {
            var that = this;
            var width = that.getWidth();
            var height = that.getHeight();
            var canvas = makeCanvas(width, height);
            var ctx;

            // // Try best effort to render to screen at this point.
            // try {
            //     ctx = canvas.getContext("2d");
            //     that.render(ctx, 0, 0);
            // } catch (e) {
            // }
            // KLUDGE: on IE, the canvas rendering functions depend on a
            // context where the canvas is attached to the DOM tree.
            // We initialize an afterAttach hook; the client's responsible
            // for calling this after the dom node is attached to the
            // document.
            var onAfterAttach = function(event) {
                // jQuery(canvas).unbind('afterAttach', onAfterAttach);
                var ctx = this.getContext("2d");
              that.render(ctx, 0, 0);
            };
            jQuery(canvas).bind('afterAttach', onAfterAttach);

            // Canvases lose their drawn content on cloning.  data may help us to preserve it.
            jQuery(canvas).data('toRender', onAfterAttach);

            return canvas;
        };

        BaseImage.prototype.toWrittenString = function(cache) { return "<image>"; }
        BaseImage.prototype.toDisplayedString = function(cache) { return "<image>"; }

        // Best-Guess equivalence for images. If they're vertex-based we're in luck,
        // otherwise we go pixel-by-pixel. It's up to exotic image types to provide
        // more efficient ways of comparing one another
        BaseImage.prototype.equals = function(other) {
          if(this.width    !== other.getWidth()    ||
             this.height   !== other.getHeight()){ return false; }
          // if they're both vertex-based images, all we need to compare are
          // their styles, vertices and color
          if(this.vertices && other.vertices){
            return (this.style    === other.style &&
                    verticesEqual(this.vertices, other.vertices) &&
                    equals(this.color, other.color));
          }
          // if it's something more sophisticated, render both images to canvases
          // First check canvas dimensions, then go pixel-by-pixel
          var c1 = this.toDomNode(), c2 = other.toDomNode();
          if(c1.width !== c2.width || c1.height !== c2.height){ return false;}
          try{
            var ctx1 = c1.getContext('2d'), ctx2 = c2.getContext('2d'),
            data1 = ctx1.getImageData(0, 0, c1.width, c1.height),
            data2 = ctx2.getImageData(0, 0, c2.width, c2.height);
            var pixels1 = data1.data,
            pixels2 = data2.data;
            for(var i = 0; i < pixels1.length; i++){
              if(pixels1[i] !== pixels2[i]){ return false; }
            }
          } catch(e){
            // if we violate CORS, just bail
            return false;
          }
          // if, after all this, we're still good...then they're equal!
          return true;
        };

        // isScene: any -> boolean
        // Produces true when x is a scene.
        var isScene = function(x) {
            return ((x != undefined) && (x != null) && (x instanceof SceneImage));
        };

        //////////////////////////////////////////////////////////////////////
        // SceneImage: primitive-number primitive-number (listof image) -> Scene
        var SceneImage = function(width, height, children, withBorder) {
          BaseImage.call(this);
          this.width    = width;
          this.height   = height;
          this.children = children; // arrayof [image, number, number]
          this.withBorder = withBorder;
        };
        SceneImage.prototype = heir(BaseImage.prototype);

        // add: image primitive-number primitive-number -> Scene
        SceneImage.prototype.add = function(anImage, x, y) {
          return new SceneImage(this.width,
                                this.height,
                                this.children.concat([[anImage,
                                                       x - anImage.getWidth()/2,
                                                       y - anImage.getHeight()/2]]),
                                this.withBorder);
        };

        // render: 2d-context primitive-number primitive-number -> void
        SceneImage.prototype.render = function(ctx, x, y) {
          var i;
          var childImage, childX, childY;
          // create a clipping region around the boundaries of the Scene
          ctx.save();
          ctx.fillStyle = "rgba(0,0,0,0)";
          ctx.fillRect(x, y, this.width, this.height);
          ctx.restore();
          // save the context, reset the path, and clip to the path around the scene edge
          ctx.save();
          ctx.beginPath();
          ctx.rect(x, y, this.width, this.height);
          ctx.clip();
          ctx.clearRect(x, y, this.width, this.height);
          // Ask every object to render itself inside the region
          for(i = 0; i < this.children.length; i++) {
            // then, render the child images
            childImage = this.children[i][0];
            childX = this.children[i][1];
            childY = this.children[i][2];
            childImage.render(ctx, childX + x, childY + y);
          }
          // unclip
          ctx.restore();

          if (this.withBorder) {
            ctx.strokeStyle = 'black';
            ctx.strokeRect(x, y, this.width, this.height);
          }
        };

        SceneImage.prototype.equals = function(other) {
          if (!(other instanceof SceneImage)) {
            return BaseImage.prototype.equals.call(this, other);
          }
          if (this.width    !== other.width ||
              this.height   !== other.height ||
              this.children.length !== other.children.length) {
            return false;
          }

          for (var i = 0; i < this.children.length; i++) {
            var rec1 = this.children[i];
            var rec2 = other.children[i];
            if (rec1[1] !== rec2[1] ||
                rec1[2] !== rec2[2] ||
                !equals(rec1[0], rec2[0])) {
              return false;
            }
          }
          return true;
        };

        //////////////////////////////////////////////////////////////////////
        // FileImage: string node -> Image
        var FileImage = function(src, rawImage) {
          BaseImage.call(this);
          var self = this;
          this.src = src;
          this.isLoaded = false;

          // animationHack: see installHackToSupportAnimatedGifs() for details.
          this.animationHackImg = undefined;

          if (rawImage && rawImage.complete) {
            this.img = rawImage;
            this.isLoaded = true;
            self.width = self.img.width;
            self.height = self.img.height;
          } else {
            // fixme: we may want to do something blocking here for
            // onload, since we don't know at this time what the file size
            // should be, nor will drawImage do the right thing until the
            // file is loaded.
            this.img = new Image();
            this.img.onload = function() {
              self.isLoaded = true;
              self.width = self.img.width;
              self.height = self.img.height;
            };
            this.img.onerror = function(e) {
              self.img.onerror = "";
              self.img.src = "http://www.wescheme.org/images/broken.png";
            }
            this.img.src = src;
          }
        }
        FileImage.prototype = heir(BaseImage.prototype);

        var imageCache = {};
        FileImage.makeInstance = function(path, rawImage) {
          if (! (path in imageCache)) {
            imageCache[path] = new FileImage(path, rawImage);
          }
          return imageCache[path];
        };

        FileImage.installInstance = function(path, rawImage) {
          imageCache[path] = new FileImage(path, rawImage);
        };

        FileImage.installBrokenImage = function(path) {
          imageCache[path] = new TextImage("Unable to load " + path, 10, colorDb.get("red"),
                                           "normal", "Optimer","","",false);
        };

        FileImage.prototype.render = function(ctx, x, y) {
          this.installHackToSupportAnimatedGifs();
          ctx.drawImage(this.animationHackImg, x, y);
        };

        // The following is a hack that we use to allow animated gifs to show
        // as animating on the canvas.
        FileImage.prototype.installHackToSupportAnimatedGifs = function() {
          if (this.animationHackImg) { return; }
          this.animationHackImg = this.img.cloneNode(true);
          document.body.appendChild(this.animationHackImg);
          this.animationHackImg.style.position = 'absolute';
          this.animationHackImg.style.top = '-2000px';
        };

        FileImage.prototype.getWidth = function() {
          return this.img.width;
        };

        FileImage.prototype.getHeight = function() {
          return this.img.height;
        };

        // Override toDomNode: we don't need a full-fledged canvas here.
        FileImage.prototype.toDomNode = function(params) {
          return this.img.cloneNode(true);
        };

        FileImage.prototype.equals = function(other) {
          if (!(other instanceof FileImage)) {
            return BaseImage.prototype.equals.call(this, other);
          }
          return (this.src === other.src);
        };

        //////////////////////////////////////////////////////////////////////
        // FileVideoe: String Node -> Video
        var FileVideo = function(src, rawVideo) {
            BaseImage.call(this);
            var self = this;
            this.src = src;
            if (rawVideo) {
          this.video			= rawVideo;
          this.width			= self.video.videoWidth;
          this.height			= self.video.videoHeight;
          this.video.volume	= 1;
          this.video.poster	= "http://www.wescheme.org/images/broken.png";
          this.video.autoplay	= true;
          this.video.autobuffer=true;
          this.video.loop		= true;
          this.video.play();
            } else {
          // fixme: we may want to do something blocking here for
          // onload, since we don't know at this time what the file size
          // should be, nor will drawImage do the right thing until the
          // file is loaded.
          this.video = document.createElement('video');
          this.video.src = src;
          this.video.addEventListener('canplay', function() {
              this.width			= self.video.videoWidth;
              this.height			= self.video.videoHeight;
              this.video.poster	= "http://www.wescheme.org/images/broken.png";
              this.video.autoplay	= true;
              this.video.autobuffer=true;
              this.video.loop		= true;
              this.video.play();
          });
          this.video.addEventListener('error', function(e) {
              self.video.onerror = "";
              self.video.poster = "http://www.wescheme.org/images/broken.png";
          });
            }
        }
        FileVideo.prototype = heir(BaseImage.prototype);

        var videos = {};
        FileVideo.makeInstance = function(path, rawVideo) {
            if (! (path in FileVideo)) {
          videos[path] = new FileVideo(path, rawVideo);
            }
            return videos[path];
        };

        FileVideo.prototype.render = function(ctx, x, y) {
            ctx.drawImage(this.video, x, y);
        };
        FileVideo.prototype.equals = function(other) {
          return (other instanceof FileVideo) && (this.src === other.src);
        };

        //////////////////////////////////////////////////////////////////////
        // ImageDataImage: imageData -> image
        // Given an array of pixel data, create an image
        var ImageDataImage = function(imageData) {
          BaseImage.call(this);
          this.imageData= imageData;
          this.width    = imageData.width;
          this.height   = imageData.height;
        };

        ImageDataImage.prototype = heir(BaseImage.prototype);

        ImageDataImage.prototype.render = function(ctx, x, y) {
          ctx.putImageData(this.imageData, x, y);
        };

        //////////////////////////////////////////////////////////////////////
        // OverlayImage: image image placeX placeY -> image
        // Creates an image that overlays img1 on top of the
        // other image img2.
        var OverlayImage = function(img1, img2, placeX, placeY) {
          BaseImage.call(this);

          // An overlay image consists of width, height, x1, y1, x2, and
          // y2.  We need to compute these based on the inputs img1,
          // img2, placex, and placey.

          // placeX and placeY may be non-numbers, in which case their values
          // depend on the img1 and img2 geometry.

          var x1, y1, x2, y2;

          if (placeX === "left") {
            x1 = 0;
            x2 = 0;
          } else if (placeX === "right") {
            x1 = Math.max(img1.getWidth(), img2.getWidth()) - img1.getWidth();
            x2 = Math.max(img1.getWidth(), img2.getWidth()) - img2.getWidth();
          } else if (placeX === "beside") {
            x1 = 0;
            x2 = img1.getWidth();
          } else if (placeX === "middle" || placeX === "center") {
            x1 = Math.max(img1.getWidth(), img2.getWidth())/2 - img1.getWidth()/2;
            x2 = Math.max(img1.getWidth(), img2.getWidth())/2 - img2.getWidth()/2;
          } else {
            x1 = Math.max(placeX, 0) - placeX;
            x2 = Math.max(placeX, 0);
          }

          if (placeY === "top") {
            y1 = 0;
            y2 = 0;
          } else if (placeY === "bottom") {
            y1 = Math.max(img1.getHeight(), img2.getHeight()) - img1.getHeight();
            y2 = Math.max(img1.getHeight(), img2.getHeight()) - img2.getHeight();
          } else if (placeY === "above") {
            y1 = 0;
            y2 = img1.getHeight();
          } else if (placeY === "baseline") {
            y1 = Math.max(img1.getBaseline(), img2.getBaseline()) - img1.getBaseline();
            y2 = Math.max(img1.getBaseline(), img2.getBaseline()) - img2.getBaseline();
          } else if (placeY === "middle" || placeY === "center") {
            y1 = Math.max(img1.getHeight(), img2.getHeight())/2 - img1.getHeight()/2;
            y2 = Math.max(img1.getHeight(), img2.getHeight())/2 - img2.getHeight()/2;
          } else {
            y1 = Math.max(placeY, 0) - placeY;
            y2 = Math.max(placeY, 0);
          }

          // calculate the vertices of this image by translating the verticies of the sub-images
          var i, v1 = img1.getVertices(), v2 = img2.getVertices(), xs = [], ys = [];

          for(i=0; i<v1.length; i++){
            xs.push(Math.round(v1[i].x + x1));
            ys.push(Math.round(v1[i].y + y1));
          }
          for(i=0; i<v2.length; i++){
            xs.push(Math.round(v2[i].x + x2));
            ys.push(Math.round(v2[i].y + y2));
          }

          // store the vertices as something private, so this.getVertices() will still return undefined
          this._vertices = zipVertices(xs, ys);
          this.width  = Math.max.apply(Math, xs) - Math.min.apply(Math, xs);
          this.height = Math.max.apply(Math, ys) - Math.min.apply(Math, ys);

          // store the offsets for rendering
          this.x1 = Math.floor(x1);
          this.y1 = Math.floor(y1);
          this.x2 = Math.floor(x2);
          this.y2 = Math.floor(y2);
          this.img1 = img1;
          this.img2 = img2;
        };

        OverlayImage.prototype = heir(BaseImage.prototype);

        OverlayImage.prototype.getVertices = function() { return this._vertices; };

        OverlayImage.prototype.render = function(ctx, x, y) {
          ctx.save();
          this.img2.render(ctx, x + this.x2, y + this.y2);
          this.img1.render(ctx, x + this.x1, y + this.y1);
          ctx.restore();
        };

        OverlayImage.prototype.equals = function(other) {
          if (!(other instanceof OverlayImage)) {
            return BaseImage.prototype.equals.call(this, other);
          }
          return (this.width     === other.width &&
                  this.height    === other.height &&
                  this.x1        === other.x1 &&
                  this.y1        === other.y1 &&
                  this.x2        === other.x2 &&
                  this.y2        === other.y2 &&
                  equals(this.img1, other.img1) &&
                  equals(this.img2, other.img2) );
        };

        //////////////////////////////////////////////////////////////////////
        // rotate: angle image -> image
        // Rotates image by angle degrees in a counter-clockwise direction.
        // TODO: special case for ellipse?
        var RotateImage = function(angle, img) {
          BaseImage.call(this);
          var sin   = Math.sin(angle * Math.PI / 180);
          var cos   = Math.cos(angle * Math.PI / 180);
          var width = img.getWidth();
          var height= img.getHeight();

          // rotate each point as if it were rotated about (0,0)
          var vertices = img.getVertices(), xs = [], ys = [];
          for(var i=0; i<vertices.length; i++){
            xs[i] = Math.round(vertices[i].x*cos - vertices[i].y*sin);
            ys[i] = Math.round(vertices[i].x*sin + vertices[i].y*cos);
          }
          // figure out what translation is necessary to shift the vertices back to 0,0
          var translateX = Math.floor(-Math.min.apply( Math, xs ));
          var translateY = Math.floor(-Math.min.apply( Math, ys ));
          for(var i=0; i<vertices.length; i++){
            xs[i] += translateX;
            ys[i] += translateY;
          }

          // store the vertices as something private, so this.getVertices() will still return undefined
          this._vertices = zipVertices(xs,ys);
          var rotatedWidth  = Math.max.apply( Math, xs ) - Math.min.apply( Math, xs );
          var rotatedHeight = Math.max.apply( Math, ys ) - Math.min.apply( Math, ys );

          this.img        = img;
          this.width      = Math.floor(rotatedWidth);
          this.height     = Math.floor(rotatedHeight);
          this.angle      = angle;
          this.translateX = translateX;
          this.translateY  = translateY;
        };

        RotateImage.prototype = heir(BaseImage.prototype);

        RotateImage.prototype.getVertices = function() { return this._vertices; };

        // translate the canvas using the calculated values, then draw at the rotated (x,y) offset.
        RotateImage.prototype.render = function(ctx, x, y) {
          ctx.save();
          ctx.translate(x+this.translateX, y + this.translateY);
          ctx.rotate(this.angle * Math.PI / 180);
          this.img.render(ctx, 0, 0);
          ctx.restore();
        };

        RotateImage.prototype.equals = function(other) {
          if (!(other instanceof RotateImage)) {
            return BaseImage.prototype.equals.call(this, other);
          }
          return (this.width     === other.width &&
                  this.height    === other.height &&
                  this.angle     === other.angle &&
                  this.translateX=== other.translateX &&
                  this.translateY=== other.translateY &&
                  equals(this.img, other.img) );
        };

        //////////////////////////////////////////////////////////////////////
        // ScaleImage: factor factor image -> image
        // Scale an image
        var ScaleImage = function(xFactor, yFactor, img) {
          BaseImage.call(this);
          var vertices = img.getVertices();
          var xs = [], ys = [];
          for(var i=0; i<vertices.length; i++){
            xs[i] = Math.round(vertices[i].x*xFactor);
            ys[i] = Math.round(vertices[i].y*yFactor);
          }
          // store the vertices as something private, so this.getVertices() will still return undefined
          this._vertices = zipVertices(xs,ys);

          this.img      = img;
          this.width    = Math.floor(img.getWidth() * xFactor);
          this.height   = Math.floor(img.getHeight() * yFactor);
          this.xFactor  = xFactor;
          this.yFactor  = yFactor;
        };

        ScaleImage.prototype = heir(BaseImage.prototype);

        ScaleImage.prototype.getVertices = function() { return this._vertices; };

        // scale the context, and pass it to the image's render function
        ScaleImage.prototype.render = function(ctx, x, y) {
          ctx.save();
          ctx.scale(this.xFactor, this.yFactor);
          this.img.render(ctx, x / this.xFactor, y / this.yFactor);
          ctx.restore();
        };

        ScaleImage.prototype.equals = function(other) {
          if (!(other instanceof ScaleImage)) {
            return BaseImage.prototype.equals.call(this, other);
          }
          return (this.width     === other.width &&
                  this.height    === other.height &&
                  this.xFactor   === other.xFactor &&
                  this.yFactor   === other.yFactor &&
                  equals(this.img, other.img) );
        };

        //////////////////////////////////////////////////////////////////////
        // CropImage: startX startY width height image -> image
        // Crop an image
        var CropImage = function(x, y, width, height, img) {
          BaseImage.call(this);
          this.x          = x;
          this.y          = y;
          this.width      = width;
          this.height     = height;
          this.img        = img;
        };

        CropImage.prototype = heir(BaseImage.prototype);

        CropImage.prototype.render = function(ctx, x, y) {
          ctx.save();
          ctx.beginPath();
          ctx.rect(x, y, this.width, this.height);
          ctx.clip();
          ctx.translate(-this.x, -this.y);
          this.img.render(ctx, x, y);
          ctx.restore();
        };

        CropImage.prototype.equals = function(other) {
          if (!(other instanceof CropImage)) {
            return BaseImage.prototype.equals.call(this, other);
          }
          return (this.width     === other.width &&
                  this.height    === other.height &&
                  this.x         === other.x &&
                  this.y         === other.y &&
                  equals(this.img, other.img) );
        };

        //////////////////////////////////////////////////////////////////////
        // FrameImage: factor factor image -> image
        // Stick a frame around the image
        var FrameImage = function(img) {
          BaseImage.call(this);
          this.img        = img;
          this.width      = img.getWidth();
          this.height     = img.getHeight();
        };

        FrameImage.prototype = heir(BaseImage.prototype);

        // scale the context, and pass it to the image's render function
        FrameImage.prototype.render = function(ctx, x, y) {
          ctx.save();
          this.img.render(ctx, x, y);
          ctx.beginPath();
          ctx.strokeStyle = "black";
          ctx.strokeRect(x, y, this.width, this.height);
          ctx.closePath();
          ctx.restore();
        };

        FrameImage.prototype.equals = function(other) {
          if (!(other instanceof FrameImage)) {
            return BaseImage.prototype.equals.call(this, other);
          }
          return equals(this.img, other.img);
        };

        //////////////////////////////////////////////////////////////////////
        // FlipImage: image string -> image
        // Flip an image either horizontally or vertically
        var FlipImage = function(img, direction) {
          BaseImage.call(this);
          this.img        = img;
          this.width      = img.getWidth();
          this.height     = img.getHeight();
          this.direction  = direction;
        };

        FlipImage.prototype = heir(BaseImage.prototype);

        FlipImage.prototype.render = function(ctx, x, y) {
          // when flipping an image of dimension M and offset by N across an axis,
          // we need to translate the canvas by M+2N in the opposite direction
          ctx.save();
          if(this.direction === "horizontal"){
            ctx.scale(-1, 1);
            ctx.translate(-(this.width+2*x), 0);
            this.img.render(ctx, x, y);
          }
          if (this.direction === "vertical"){
            ctx.scale(1, -1);
            ctx.translate(0, -(this.height+2*y));
            this.img.render(ctx, x, y);
          }
          ctx.restore();
        };

        FlipImage.prototype.getWidth = function() {
          return this.width;
        };

        FlipImage.prototype.getHeight = function() {
          return this.height;
        };

        FlipImage.prototype.equals = function(other) {
          if (!(other instanceof FlipImage)) {
            return BaseImage.prototype.equals.call(this, other);
          }
          return (this.width     === other.width &&
                  this.height    === other.height &&
                  this.direction === other.direction &&
                  equals(this.img, other.img) );
        };

        //////////////////////////////////////////////////////////////////////
        // RectangleImage: Number Number Mode Color -> Image
        var RectangleImage = function(width, height, style, color) {
          BaseImage.call(this);
          this.width  = width;
          this.height = height;
          this.style  = style;
          this.color  = color;
          this.vertices = [{x:0,y:height},{x:0,y:0},{x:width,y:0},{x:width,y:height}];
        };
        RectangleImage.prototype = heir(BaseImage.prototype);

        RectangleImage.prototype.getWidth = function() {
          return this.width;
        };

        RectangleImage.prototype.getHeight = function() {
          return this.height;
        };

        //////////////////////////////////////////////////////////////////////
        // RhombusImage: Number Number Mode Color -> Image
        var RhombusImage = function(side, angle, style, color) {
          BaseImage.call(this);
          // sin(angle/2-in-radians) * side = half of base
          // cos(angle/2-in-radians) * side = half of height
          this.width  = Math.sin(angle/2 * Math.PI / 180) * side * 2;
          this.height = Math.abs(Math.cos(angle/2 * Math.PI / 180)) * side * 2;
          this.side   = side;
          this.angle  = angle;
          this.style  = style;
          this.color  = color;
          this.vertices = [{x:this.width/2, y:0},
                           {x:this.width,   y:this.height/2},
                           {x:this.width/2, y:this.height},
                           {x:0,            y:this.height/2}];

        };
        RhombusImage.prototype = heir(BaseImage.prototype);

        RhombusImage.prototype.getWidth = function() {
          return this.width;
        };

        RhombusImage.prototype.getHeight = function() {
          return this.height;
        };

        //////////////////////////////////////////////////////////////////////
        // PolygonImage: Number Count Step Mode Color -> Image
        //
        // See http://www.algebra.com/algebra/homework/Polygons/Inscribed-and-circumscribed-polygons.lesson
        // the polygon is inscribed in a circle, whose radius is length/2sin(pi/count)
        // another circle is inscribed in the polygon, whose radius is length/2tan(pi/count)
        // rotate a 3/4 quarter turn plus half the angle length to keep bottom base level
        var PolygonImage = function(length, count, step, style, color) {
          BaseImage.call(this);
          this.outerRadius = Math.floor(length/(2*Math.sin(Math.PI/count)));
          this.innerRadius = Math.floor(length/(2*Math.tan(Math.PI/count)));
          var adjust = (3*Math.PI/2)+Math.PI/count;

          // rotate around outer circle, storing x and y coordinates
          var radians = 0, xs = [], ys = [];
          for(var i = 0; i < count; i++) {
            radians = radians + (step*2*Math.PI/count);
            xs.push(Math.round(this.outerRadius*Math.cos(radians-adjust)));
            ys.push(Math.round(this.outerRadius*Math.sin(radians-adjust)));
          }
          var vertices = zipVertices(xs, ys);

          this.width      = Math.max.apply(Math, xs) - Math.min.apply(Math, xs);
          this.height     = Math.max.apply(Math, ys) - Math.min.apply(Math, ys);
          this.length     = length;
          this.count      = count;
          this.step       = step;
          this.style      = style;
          this.color      = color;

          // shift the vertices by the calculated offsets, now that we know the width
          var xOffset = Math.round(this.width/2);
          var yOffset = ((this.count % 2)? this.outerRadius : this.innerRadius);
          for(i=0; i<vertices.length; i++){
            vertices[i].x += xOffset; vertices[i].y += yOffset;
          }
          this.vertices   = vertices;
        };

        PolygonImage.prototype = heir(BaseImage.prototype);

        var maybeQuote = function(s) {
          if (/ /.test(s)) {
            return "\"" + s + "\"";
          }
          return s;
        };

        //////////////////////////////////////////////////////////////////////
        // TextImage: String Number Color String String String String any/c -> Image
        var TextImage = function(msg, size, color, face, family, style, weight, underline) {
          BaseImage.call(this);
          var metrics;
          this.msg        = msg;
          this.size       = size;   // 18
          this.color      = color;  // red
          this.face       = face;   // Gill Sans
          this.family     = family; // 'swiss
          this.style      = (style === "slant")? "oblique" : style;  // Racket's "slant" -> CSS's "oblique"
          this.weight     = (weight=== "light")? "lighter" : weight; // Racket's "light" -> CSS's "lighter"
          this.underline  = underline;
          // example: "bold italic 20px 'Times', sans-serif".
          // Default weight is "normal", face is "Arial"

          // NOTE: we *ignore* font-family, as it causes a number of font bugs due the browser inconsistencies
          var canvas  = makeCanvas(0, 0),
              ctx     = canvas.getContext("2d");

          this.font = (this.style + " " +
                       this.weight + " " +
                       this.size + "px " +
                       '"'+this.face+'", '+
                       this.family);

          try {
            ctx.font    = this.font;
          } catch (e) {
            this.fallbackOnFont();
            ctx.font    = this.font;
          }

          // Defensive: on IE, this can break.
          try {
            metrics     = ctx.measureText(msg);
            this.width  = metrics.width;
            this.height = Number(this.size);
          } catch(e) {
            this.fallbackOnFont();
          }
        };

        TextImage.prototype = heir(BaseImage.prototype);

        TextImage.prototype.fallbackOnFont = function() {
          // Defensive: if the browser doesn't support certain features, we
          // reduce to a smaller feature set and try again.
          this.font       = this.size + "px " + maybeQuote(this.family);
          var canvas      = makeCanvas(0, 0);
          var ctx         = canvas.getContext("2d");
          ctx.font        = this.font;
          var metrics     = ctx.measureText(this.msg);
          this.width      = metrics.width;
          // KLUDGE: I don't know how to get at the height.
          this.height     = Number(this.size);//ctx.measureText("m").width + 20;
        };

        TextImage.prototype.render = function(ctx, x, y) {
          ctx.save();
          ctx.textAlign   = 'left';
          ctx.textBaseline= 'top';
          ctx.fillStyle   = colorString(this.color);
          ctx.font        = this.font;
          try {
            ctx.fillText(this.msg, x, y);
          } catch (e) {
            this.fallbackOnFont();
            ctx.font = this.font;
            ctx.fillText(this.msg, x, y);
          }
          if(this.underline){
            ctx.beginPath();
            ctx.moveTo(x, y+this.size);
            // we use this.size, as it is more accurate for underlining than this.height
            ctx.lineTo(x+this.width, y+this.size);
            ctx.closePath();
            ctx.strokeStyle = colorString(this.color);
            ctx.stroke();
          }
          ctx.restore();
        };

        TextImage.prototype.getBaseline = function() {
          return this.size;
        };

        TextImage.prototype.equals = function(other) {
          if (!(other instanceof TextImage)) {
            return BaseImage.prototype.equals.call(this, other);
          }
          return (this.msg      === other.msg &&
                  this.size     === other.size &&
                  this.face     === other.face &&
                  this.family   === other.family &&
                  this.style    === other.style &&
                  this.weight   === other.weight &&
                  this.underline === other.underline &&
                  equals(this.color, other.color) &&
                  this.font === other.font);
        };

        //////////////////////////////////////////////////////////////////////
        // StarImage: fixnum fixnum fixnum color -> image
        // Most of this code here adapted from the Canvas tutorial at:
        // http://developer.apple.com/safari/articles/makinggraphicswithcanvas.html
        var StarImage = function(points, outer, inner, style, color) {
          BaseImage.call(this);
          this.points     = points;
          this.outer      = outer;
          this.inner      = inner;
          this.style      = style;
          this.color      = color;
          this.radius     = Math.max(this.inner, this.outer);
          this.width      = this.radius*2;
          this.height     = this.radius*2;
          var vertices   = [];

          var oneDegreeAsRadian = Math.PI / 180;
          for(var pt = 0; pt < (this.points * 2) + 1; pt++ ) {
            var rads = ( ( 360 / (2 * this.points) ) * pt ) * oneDegreeAsRadian - 0.5;
            var radius = ( pt % 2 === 1 ) ? this.outer : this.inner;
            vertices.push({x:this.radius + ( Math.sin( rads ) * radius ),
                          y:this.radius + ( Math.cos( rads ) * radius )} );
          }
          this.vertices = vertices;
        };

        StarImage.prototype = heir(BaseImage.prototype);

        /////////////////////////////////////////////////////////////////////
        //TriangleImage: Number Number Number Mode Color -> Image
        // Draws a triangle with the base = sideC, and the angle between sideC
        // and sideB being angleA
        // See http://docs.racket-lang.org/teachpack/2htdpimage.html#(def._((lib._2htdp/image..rkt)._triangle))
        var TriangleImage = function(sideC, angleA, sideB, style, color) {
          BaseImage.call(this);
          var thirdX = sideB * Math.cos(angleA * Math.PI/180);
          var thirdY = sideB * Math.sin(angleA * Math.PI/180);

          var offsetX = 0 - Math.min(0, thirdX); // angleA could be obtuse

          this.width = Math.max(sideC, thirdX) + offsetX;
          this.height = Math.abs(thirdY);

          var vertices = [];
          // if angle < 180 start at the top of the canvas, otherwise start at the bottom
          if(thirdY > 0){
            vertices.push({x: offsetX + 0, y: 0});
            vertices.push({x: offsetX + sideC, y: 0});
            vertices.push({x: offsetX + thirdX, y: thirdY});
          } else {
            vertices.push({x: offsetX + 0, y: -thirdY});
            vertices.push({x: offsetX + sideC, y: -thirdY});
            vertices.push({x: offsetX + thirdX, y: 0});
          }
            console.log(vertices);
          this.vertices = vertices;

          this.style = style;
          this.color = color;
        };
        TriangleImage.prototype = heir(BaseImage.prototype);

        //////////////////////////////////////////////////////////////////////
        //Ellipse : Number Number Mode Color -> Image
        var EllipseImage = function(width, height, style, color) {
          BaseImage.call(this);
          this.width = width;
          this.height = height;
          this.style = style;
          this.color = color;
        };

        EllipseImage.prototype = heir(BaseImage.prototype);

        EllipseImage.prototype.render = function(ctx, aX, aY) {
          ctx.save();
          ctx.beginPath();

          // Most of this code is taken from:
          // http://webreflection.blogspot.com/2009/01/ellipse-and-circle-for-canvas-2d.html
          var hB = (this.width / 2) * 0.5522848,
          vB = (this.height / 2) * 0.5522848,
          eX = aX + this.width,
          eY = aY + this.height,
          mX = aX + this.width / 2,
          mY = aY + this.height / 2;
          ctx.moveTo(aX, mY);
          ctx.bezierCurveTo(aX, mY - vB, mX - hB, aY, mX, aY);
          ctx.bezierCurveTo(mX + hB, aY, eX, mY - vB, eX, mY);
          ctx.bezierCurveTo(eX, mY + vB, mX + hB, eY, mX, eY);
          ctx.bezierCurveTo(mX - hB, eY, aX, mY + vB, aX, mY);
          ctx.closePath();
          if (this.style.toString().toLowerCase() === "outline") {
            ctx.strokeStyle = colorString(this.color);
            ctx.stroke();
          }
          else {
            ctx.fillStyle = colorString(this.color, this.style);
            ctx.fill();
          }

          ctx.restore();
        };

        EllipseImage.prototype.equals = function(other) {
          if (!(other instanceof EllipseImage)) {
            return BaseImage.prototype.equals.call(this, other);
          }
          return (this.width    === other.width &&
                  this.height   === other.height &&
                  this.style    === other.style &&
                  equals(this.color, other.color));
        };

        //////////////////////////////////////////////////////////////////////
        // Line: Number Number Color Boolean -> Image
        var LineImage = function(x, y, color) {
          BaseImage.call(this);
          var vertices;
          if (x >= 0) {
            if (y >= 0) { vertices = [{x:  0, y:  0}, {x: x, y: y}]; }
            else        { vertices = [{x:  0, y: -y}, {x: x, y: 0}]; }
          } else {
            if (y >= 0) { vertices = [{x: -x, y:  0}, {x: 0, y: y}]; }
            else        { vertices = [{x: -x, y: -y}, {x: 0, y: 0}]; }
          }
          // preserve the invariant that all vertex-based images have a style
          this.style  = "outline";
          this.color  = color;
          this.width  = Math.abs(x);
          this.height = Math.abs(y);
          this.vertices = vertices;
        };

        LineImage.prototype = heir(BaseImage.prototype);

        var imageToColorList = function(img) {
            var width = img.getWidth(),
            height = img.getHeight(),
            canvas = makeCanvas(width, height),
            ctx = canvas.getContext("2d"),
            imageData,
            data,
            i,
            r, g, b, a;
            img.render(ctx, 0, 0);
            imageData = ctx.getImageData(0, 0, width, height);
            data = imageData.data;
            var colors = [];
            for (i = 0 ; i < data.length; i += 4) {
              r = data[i];
              g = data[i+1];
              b = data[i+2];
              a = data[i+3];
              colors.push(makeColor(r, g, b, a));
            }
            return ffi.makeList(colors);
        }

        var colorListToImage = function(listOfColors,
                                        width,
                                        height,
                                        pinholeX,
                                        pinholeY) {
            var canvas = makeCanvas(jsnums.toFixnum(width),
                  jsnums.toFixnum(height)),
            ctx = canvas.getContext("2d"),
            imageData = ctx.createImageData(jsnums.toFixnum(width),
                    jsnums.toFixnum(height)),
            aColor,
            data = imageData.data,
            jsLOC = ffi.toArray(listOfColors);
            for(var i = 0; i < jsLOC.length * 4; i += 4) {
              aColor = jsLOC[i / 4];
              data[i] = jsnums.toFixnum(colorRed(aColor));
              data[i+1] = jsnums.toFixnum(colorGreen(aColor));
              data[i+2] = jsnums.toFixnum(colorBlue(aColor));
              data[i+3] = jsnums.toFixnum(colorAlpha(aColor));
            }

            return makeImageDataImage(imageData);
        };

        var makeSceneImage = function(width, height, children, withBorder) {
            return new SceneImage(width, height, children, withBorder);
        };
        var makeCircleImage = function(radius, style, color) {
            return new EllipseImage(2*radius, 2*radius, style, color);
        };
        var makeStarImage = function(points, outer, inner, style, color) {
            return new StarImage(points, outer, inner, style, color);
        };
        var makeRectangleImage = function(width, height, style, color) {
            return new RectangleImage(width, height, style, color);
        };
        var makeRhombusImage = function(side, angle, style, color) {
            return new RhombusImage(side, angle, style, color);
        };
        var makePolygonImage = function(length, count, step, style, color) {
            return new PolygonImage(length, count, step, style, color);
        };
        var makeSquareImage = function(length, style, color) {
            return new RectangleImage(length, length, style, color);
        };
        var makeTriangleImage = function(sideA, angleC, sideB, style, color) {
            return new TriangleImage(sideA, angleC, sideB, style, color);
        };
        var makeEllipseImage = function(width, height, style, color) {
            return new EllipseImage(width, height, style, color);
        };
        var makeLineImage = function(x, y, color, normalPinhole) {
            return new LineImage(x, y, color, normalPinhole);
        };
        var makeOverlayImage = function(img1, img2, X, Y) {
            return new OverlayImage(img1, img2, X, Y);
        };
        var makeRotateImage = function(angle, img) {
            return new RotateImage(angle, img);
        };
        var makeScaleImage = function(xFactor, yFactor, img) {
            return new ScaleImage(xFactor, yFactor, img);
        };
        var makeCropImage = function(x, y, width, height, img) {
            return new CropImage(x, y, width, height, img);
        };
        var makeFrameImage = function(img) {
            return new FrameImage(img);
        };
        var makeFlipImage = function(img, direction) {
            return new FlipImage(img, direction);
        };
        var makeTextImage = function(msg, size, color, face, family, style, weight, underline) {
            return new TextImage(msg, size, color, face, family, style, weight, underline);
        };
        var makeImageDataImage = function(imageData) {
            return new ImageDataImage(imageData);
        };
        var makeFileImage = function(path, rawImage) {
            return FileImage.makeInstance(path, rawImage);
        };
        var makeFileVideo = function(path, rawVideo) {
            return FileVideo.makeInstance(path, rawVideo);
        };

        var isSceneImage = function(x) { return x instanceof SceneImage; };
        var isCircleImage = function(x) { return x instanceof EllipseImage &&
                                          x.width === x.height; };
        var isStarImage	= function(x) { return x instanceof StarImage; };
        var isRectangleImage=function(x) { return x instanceof RectangleImage; };
        var isPolygonImage = function(x) { return x instanceof PolygonImage; };
        var isRhombusImage = function(x) { return x instanceof RhombusImage; };
        var isSquareImage	= function(x) { return x instanceof SquareImage; };
        var isTriangleImage= function(x) { return x instanceof TriangleImage; };
        var isEllipseImage = function(x) { return x instanceof EllipseImage; };
        var isLineImage	= function(x) { return x instanceof LineImage; };
        var isOverlayImage = function(x) { return x instanceof OverlayImage; };
        var isRotateImage	= function(x) { return x instanceof RotateImage; };
        var isScaleImage	= function(x) { return x instanceof ScaleImage; };
        var isCropImage	= function(x) { return x instanceof CropImage; };
        var isFrameImage	= function(x) { return x instanceof FrameImage; };
        var isFlipImage	= function(x) { return x instanceof FlipImage; };
        var isTextImage	= function(x) { return x instanceof TextImage; };
        var isFileImage	= function(x) { return x instanceof FileImage; };
        var isFileVideo	= function(x) { return x instanceof FileVideo; };

        // Color database
        var ColorDb = function() {
            this.colors = {};
        };

        ColorDb.prototype.put = function(name, color) {
            this.colors[name] = color;
        };

        ColorDb.prototype.get = function(name) {
            return this.colors[name.toString().toUpperCase()];
        };

        // FIXME: update toString to handle the primitive field values.

        var colorDb = new ColorDb();
        colorDb.put("ORANGE", makeColor(255, 165, 0));
        colorDb.put("RED", makeColor(255, 0, 0));
        colorDb.put("ORANGERED", makeColor(255, 69, 0));
        colorDb.put("TOMATO", makeColor(255, 99, 71));
        colorDb.put("DARKRED", makeColor(139, 0, 0));
        colorDb.put("RED", makeColor(255, 0, 0));
        colorDb.put("FIREBRICK", makeColor(178, 34, 34));
        colorDb.put("CRIMSON", makeColor(220, 20, 60));
        colorDb.put("DEEPPINK", makeColor(255, 20, 147));
        colorDb.put("MAROON", makeColor(176, 48, 96));
        colorDb.put("INDIAN RED", makeColor(205, 92, 92));
        colorDb.put("INDIANRED", makeColor(205, 92, 92));
        colorDb.put("MEDIUM VIOLET RED", makeColor(199, 21, 133));
        colorDb.put("MEDIUMVIOLETRED", makeColor(199, 21, 133));
        colorDb.put("VIOLET RED", makeColor(208, 32, 144));
        colorDb.put("VIOLETRED", makeColor(208, 32, 144));
        colorDb.put("LIGHTCORAL", makeColor(240, 128, 128));
        colorDb.put("HOTPINK", makeColor(255, 105, 180));
        colorDb.put("PALEVIOLETRED", makeColor(219, 112, 147));
        colorDb.put("LIGHTPINK", makeColor(255, 182, 193));
        colorDb.put("ROSYBROWN", makeColor(188, 143, 143));
        colorDb.put("PINK", makeColor(255, 192, 203));
        colorDb.put("ORCHID", makeColor(218, 112, 214));
        colorDb.put("LAVENDERBLUSH", makeColor(255, 240, 245));
        colorDb.put("SNOW", makeColor(255, 250, 250));
        colorDb.put("CHOCOLATE", makeColor(210, 105, 30));
        colorDb.put("SADDLEBROWN", makeColor(139, 69, 19));
        colorDb.put("BROWN", makeColor(132, 60, 36));
        colorDb.put("DARKORANGE", makeColor(255, 140, 0));
        colorDb.put("CORAL", makeColor(255, 127, 80));
        colorDb.put("SIENNA", makeColor(160, 82, 45));
        colorDb.put("ORANGE", makeColor(255, 165, 0));
        colorDb.put("SALMON", makeColor(250, 128, 114));
        colorDb.put("PERU", makeColor(205, 133, 63));
        colorDb.put("DARKGOLDENROD", makeColor(184, 134, 11));
        colorDb.put("GOLDENROD", makeColor(218, 165, 32));
        colorDb.put("SANDYBROWN", makeColor(244, 164, 96));
        colorDb.put("LIGHTSALMON", makeColor(255, 160, 122));
        colorDb.put("DARKSALMON", makeColor(233, 150, 122));
        colorDb.put("GOLD", makeColor(255, 215, 0));
        colorDb.put("YELLOW", makeColor(255, 255, 0));
        colorDb.put("OLIVE", makeColor(128, 128, 0));
        colorDb.put("BURLYWOOD", makeColor(222, 184, 135));
        colorDb.put("TAN", makeColor(210, 180, 140));
        colorDb.put("NAVAJOWHITE", makeColor(255, 222, 173));
        colorDb.put("PEACHPUFF", makeColor(255, 218, 185));
        colorDb.put("KHAKI", makeColor(240, 230, 140));
        colorDb.put("DARKKHAKI", makeColor(189, 183, 107));
        colorDb.put("MOCCASIN", makeColor(255, 228, 181));
        colorDb.put("WHEAT", makeColor(245, 222, 179));
        colorDb.put("BISQUE", makeColor(255, 228, 196));
        colorDb.put("PALEGOLDENROD", makeColor(238, 232, 170));
        colorDb.put("BLANCHEDALMOND", makeColor(255, 235, 205));
        colorDb.put("MEDIUM GOLDENROD", makeColor(234, 234, 173));
        colorDb.put("MEDIUMGOLDENROD", makeColor(234, 234, 173));
        colorDb.put("PAPAYAWHIP", makeColor(255, 239, 213));
        colorDb.put("MISTYROSE", makeColor(255, 228, 225));
        colorDb.put("LEMONCHIFFON", makeColor(255, 250, 205));
        colorDb.put("ANTIQUEWHITE", makeColor(250, 235, 215));
        colorDb.put("CORNSILK", makeColor(255, 248, 220));
        colorDb.put("LIGHTGOLDENRODYELLOW", makeColor(250, 250, 210));
        colorDb.put("OLDLACE", makeColor(253, 245, 230));
        colorDb.put("LINEN", makeColor(250, 240, 230));
        colorDb.put("LIGHTYELLOW", makeColor(255, 255, 224));
        colorDb.put("SEASHELL", makeColor(255, 245, 238));
        colorDb.put("BEIGE", makeColor(245, 245, 220));
        colorDb.put("FLORALWHITE", makeColor(255, 250, 240));
        colorDb.put("IVORY", makeColor(255, 255, 240));
        colorDb.put("GREEN", makeColor(0, 255, 0));
        colorDb.put("LAWNGREEN", makeColor(124, 252, 0));
        colorDb.put("CHARTREUSE", makeColor(127, 255, 0));
        colorDb.put("GREEN YELLOW", makeColor(173, 255, 47));
        colorDb.put("GREENYELLOW", makeColor(173, 255, 47));
        colorDb.put("YELLOW GREEN", makeColor(154, 205, 50));
        colorDb.put("YELLOWGREEN", makeColor(154, 205, 50));
        colorDb.put("MEDIUM FOREST GREEN", makeColor(107, 142, 35));
        colorDb.put("OLIVEDRAB", makeColor(107, 142, 35));
        colorDb.put("MEDIUMFORESTGREEN", makeColor(107, 142, 35));
        colorDb.put("DARK OLIVE GREEN", makeColor(85, 107, 47));
        colorDb.put("DARKOLIVEGREEN", makeColor(85, 107, 47));
        colorDb.put("DARKSEAGREEN", makeColor(143, 188, 139));
        colorDb.put("LIME", makeColor(0, 255, 0));
        colorDb.put("DARK GREEN", makeColor(0, 100, 0));
        colorDb.put("DARKGREEN", makeColor(0, 100, 0));
        colorDb.put("LIME GREEN", makeColor(50, 205, 50));
        colorDb.put("LIMEGREEN", makeColor(50, 205, 50));
        colorDb.put("FOREST GREEN", makeColor(34, 139, 34));
        colorDb.put("FORESTGREEN", makeColor(34, 139, 34));
        colorDb.put("SPRING GREEN", makeColor(0, 255, 127));
        colorDb.put("SPRINGGREEN", makeColor(0, 255, 127));
        colorDb.put("MEDIUM SPRING GREEN", makeColor(0, 250, 154));
        colorDb.put("MEDIUMSPRINGGREEN", makeColor(0, 250, 154));
        colorDb.put("SEA GREEN", makeColor(46, 139, 87));
        colorDb.put("SEAGREEN", makeColor(46, 139, 87));
        colorDb.put("MEDIUM SEA GREEN", makeColor(60, 179, 113));
        colorDb.put("MEDIUMSEAGREEN", makeColor(60, 179, 113));
        colorDb.put("AQUAMARINE", makeColor(112, 216, 144));
        colorDb.put("LIGHTGREEN", makeColor(144, 238, 144));
        colorDb.put("PALE GREEN", makeColor(152, 251, 152));
        colorDb.put("PALEGREEN", makeColor(152, 251, 152));
        colorDb.put("MEDIUM AQUAMARINE", makeColor(102, 205, 170));
        colorDb.put("MEDIUMAQUAMARINE", makeColor(102, 205, 170));
        colorDb.put("TURQUOISE", makeColor(64, 224, 208));
        colorDb.put("LIGHTSEAGREEN", makeColor(32, 178, 170));
        colorDb.put("MEDIUM TURQUOISE", makeColor(72, 209, 204));
        colorDb.put("MEDIUMTURQUOISE", makeColor(72, 209, 204));
        colorDb.put("HONEYDEW", makeColor(240, 255, 240));
        colorDb.put("MINTCREAM", makeColor(245, 255, 250));
        colorDb.put("ROYALBLUE", makeColor(65, 105, 225));
        colorDb.put("DODGERBLUE", makeColor(30, 144, 255));
        colorDb.put("DEEPSKYBLUE", makeColor(0, 191, 255));
        colorDb.put("CORNFLOWERBLUE", makeColor(100, 149, 237));
        colorDb.put("STEEL BLUE", makeColor(70, 130, 180));
        colorDb.put("STEELBLUE", makeColor(70, 130, 180));
        colorDb.put("LIGHTSKYBLUE", makeColor(135, 206, 250));
        colorDb.put("DARK TURQUOISE", makeColor(0, 206, 209));
        colorDb.put("DARKTURQUOISE", makeColor(0, 206, 209));
        colorDb.put("CYAN", makeColor(0, 255, 255));
        colorDb.put("AQUA", makeColor(0, 255, 255));
        colorDb.put("DARKCYAN", makeColor(0, 139, 139));
        colorDb.put("TEAL", makeColor(0, 128, 128));
        colorDb.put("SKY BLUE", makeColor(135, 206, 235));
        colorDb.put("SKYBLUE", makeColor(135, 206, 235));
        colorDb.put("CADET BLUE", makeColor(96, 160, 160));
        colorDb.put("CADETBLUE", makeColor(95, 158, 160));
        colorDb.put("DARK SLATE GRAY", makeColor(47, 79, 79));
        colorDb.put("DARKSLATEGRAY", makeColor(47, 79, 79));
        colorDb.put("LIGHTSLATEGRAY", makeColor(119, 136, 153));
        colorDb.put("SLATEGRAY", makeColor(112, 128, 144));
        colorDb.put("LIGHT STEEL BLUE", makeColor(176, 196, 222));
        colorDb.put("LIGHTSTEELBLUE", makeColor(176, 196, 222));
        colorDb.put("LIGHT BLUE", makeColor(173, 216, 230));
        colorDb.put("LIGHTBLUE", makeColor(173, 216, 230));
        colorDb.put("POWDERBLUE", makeColor(176, 224, 230));
        colorDb.put("PALETURQUOISE", makeColor(175, 238, 238));
        colorDb.put("LIGHTCYAN", makeColor(224, 255, 255));
        colorDb.put("ALICEBLUE", makeColor(240, 248, 255));
        colorDb.put("AZURE", makeColor(240, 255, 255));
        colorDb.put("MEDIUM BLUE", makeColor(0, 0, 205));
        colorDb.put("MEDIUMBLUE", makeColor(0, 0, 205));
        colorDb.put("DARKBLUE", makeColor(0, 0, 139));
        colorDb.put("MIDNIGHT BLUE", makeColor(25, 25, 112));
        colorDb.put("MIDNIGHTBLUE", makeColor(25, 25, 112));
        colorDb.put("NAVY", makeColor(36, 36, 140));
        colorDb.put("BLUE", makeColor(0, 0, 255));
        colorDb.put("INDIGO", makeColor(75, 0, 130));
        colorDb.put("BLUE VIOLET", makeColor(138, 43, 226));
        colorDb.put("BLUEVIOLET", makeColor(138, 43, 226));
        colorDb.put("MEDIUM SLATE BLUE", makeColor(123, 104, 238));
        colorDb.put("MEDIUMSLATEBLUE", makeColor(123, 104, 238));
        colorDb.put("SLATE BLUE", makeColor(106, 90, 205));
        colorDb.put("SLATEBLUE", makeColor(106, 90, 205));
        colorDb.put("PURPLE", makeColor(160, 32, 240));
        colorDb.put("DARK SLATE BLUE", makeColor(72, 61, 139));
        colorDb.put("DARKSLATEBLUE", makeColor(72, 61, 139));
        colorDb.put("DARKVIOLET", makeColor(148, 0, 211));
        colorDb.put("DARK ORCHID", makeColor(153, 50, 204));
        colorDb.put("DARKORCHID", makeColor(153, 50, 204));
        colorDb.put("MEDIUMPURPLE", makeColor(147, 112, 219));
        colorDb.put("CORNFLOWER BLUE", makeColor(68, 64, 108));
        colorDb.put("MEDIUM ORCHID", makeColor(186, 85, 211));
        colorDb.put("MEDIUMORCHID", makeColor(186, 85, 211));
        colorDb.put("MAGENTA", makeColor(255, 0, 255));
        colorDb.put("FUCHSIA", makeColor(255, 0, 255));
        colorDb.put("DARKMAGENTA", makeColor(139, 0, 139));
        colorDb.put("VIOLET", makeColor(238, 130, 238));
        colorDb.put("PLUM", makeColor(221, 160, 221));
        colorDb.put("LAVENDER", makeColor(230, 230, 250));
        colorDb.put("THISTLE", makeColor(216, 191, 216));
        colorDb.put("GHOSTWHITE", makeColor(248, 248, 255));
        colorDb.put("WHITE", makeColor(255, 255, 255));
        colorDb.put("WHITESMOKE", makeColor(245, 245, 245));
        colorDb.put("GAINSBORO", makeColor(220, 220, 220));
        colorDb.put("LIGHT GRAY", makeColor(211, 211, 211));
        colorDb.put("LIGHTGRAY", makeColor(211, 211, 211));
        colorDb.put("SILVER", makeColor(192, 192, 192));
        colorDb.put("GRAY", makeColor(190, 190, 190));
        colorDb.put("DARK GRAY", makeColor(169, 169, 169));
        colorDb.put("DARKGRAY", makeColor(169, 169, 169));
        colorDb.put("DIM GRAY", makeColor(105, 105, 105));
        colorDb.put("DIMGRAY", makeColor(105, 105, 105));
        colorDb.put("BLACK", makeColor(0, 0, 0));

        ///////////////////////////////////////////////////////////////
        // Exports

        // These functions are available for direct access without the typechecks
        // of the Racket-exposed functions.
        return {
          makeCanvas: makeCanvas,

          BaseImage: BaseImage,
          SceneImage: SceneImage,
          FileImage: FileImage,
          VideoImage: FileVideo,
          OverlayImage: OverlayImage,
          RotateImage: RotateImage,
          ScaleImage: ScaleImage,
          CropImage: CropImage,
          FrameImage: FrameImage,
          FlipImage: FlipImage,
          RectangleImage: RectangleImage,
          RhombusImage: RhombusImage,
          ImageDataImage: ImageDataImage,
          PolygonImage: PolygonImage,
          TextImage: TextImage,
          StarImage: StarImage,
          TriangleImage: TriangleImage,
          EllipseImage: EllipseImage,
          LineImage: LineImage,
          StarImage: StarImage,

          imageEquals: imageEquals,

          colorDb: colorDb,

          makeSceneImage: makeSceneImage,
          makeCircleImage: makeCircleImage,
          makeStarImage: makeStarImage,
          makeRectangleImage: makeRectangleImage,
          makeRhombusImage: makeRhombusImage,
          makePolygonImage: makePolygonImage,
          makeSquareImage: makeSquareImage,
          makeTriangleImage: makeTriangleImage,
          makeEllipseImage: makeEllipseImage,
          makeLineImage: makeLineImage,
          makeOverlayImage: makeOverlayImage,
          makeRotateImage: makeRotateImage,
          makeScaleImage: makeScaleImage,
          makeCropImage: makeCropImage,
          makeFrameImage: makeFrameImage,
          makeFlipImage: makeFlipImage,
          makeTextImage: makeTextImage,
          makeImageDataImage: makeImageDataImage,
          makeFileImage: makeFileImage,
          makeVideoImage: makeFileVideo,

          imageToColorList: imageToColorList,
          colorListToImage: colorListToImage,

          isImage: isImage,
          isScene: isScene,
          isColorOrColorString: isColorOrColorString,
          isAngle: isAngle,
          isSideCount: isSideCount,
          isStepCount: isStepCount,
          isPointsCount: isPointsCount,

          isSceneImage: isSceneImage,
          isCircleImage: isCircleImage,
          isStarImage: isStarImage,
          isRectangleImage: isRectangleImage,
          isPolygonImage: isPolygonImage,
          isRhombusImage: isRhombusImage,
          isSquareImage: isSquareImage,
          isTriangleImage: isTriangleImage,
          isEllipseImage: isEllipseImage,
          isLineImage: isLineImage,
          isOverlayImage: isOverlayImage,
          isRotateImage: isRotateImage,
          isScaleImage: isScaleImage,
          isCropImage: isCropImage,
          isFrameImage: isFrameImage,
          isFlipImage: isFlipImage,
          isTextImage: isTextImage,
          isFileImage: isFileImage,
          isFileVideo: isFileVideo,

          makeColor: makeColor,
          isColor: isColor,
          colorRed: colorRed,
          colorGreen: colorGreen,
          colorBlue: colorBlue,
          colorAlpha: colorAlpha
        }
      });
    });
  };
});
