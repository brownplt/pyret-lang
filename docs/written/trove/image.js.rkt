#lang scribble/base
@(require "../../scribble-api.rkt"
          scribble/manual)
@(define Image (a-id "Image" (xref "image" "Image")))
@docmodule["image"]{
  The Pyret images library is based on the images teachpack in HtDP, and borrows much of the language for documentation. You can find documentation for the teachpack here:

  @url["http://docs.racket-lang.org/teachpack/2htdpimage.html"] 

  Differences in function names and their corresponding Racket equivalent
  are noted where appropriate.

  @section{Basic Images}
  @function[
    "circle"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("radius" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Constructs a circle with the given radius, mode and color. Corresponds
    to @racket[(circle ...)] in HtDP.
  }
  @function[
    "ellipse"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("width" "") 
                         '("height" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Constructs an ellipse with the given width, height, mode and
    color. Corresponds to @racket[(ellipse ...)] in HtDP.
  }
  @function[
    "line"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("x" "") 
                         '("y" "") 
                         '("color" ""))]{
    Draws an image of a line that connects the point (0,0) to the point
    (x,y). Corresponds to @racket[(line ...)] in HtDP.
  }
  @function[
    "add-line"
            #:contract (a-arrow Image
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("img" "") 
                         '("x1" "") 
                         '("y1" "") 
                         '("x2" "") 
                         '("y2" "") 
                         '("color" ""))]{
    Adds a line to the image @pyret["img"], starting from the point (x1,y1)
    and going to the point (x2,y2). Unlike @secref[(tag-name "image" "scene-line")],
    if the line passes outside of @pyret["img"], the image gets larger to
    accommodate the line.
  }
  @function[
    "text"
            #:contract (a-arrow (a-id "String" (xref "<global>" "String"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("string" "Text to draw.") 
                         '("size" "Font size in pixels.") 
                         '("color" "Color of text."))]{
    Constructs an image of @pyret["string"], using the given font size
    and color.
  }
  @function[
    "text-font"
            #:contract (a-arrow (a-id "String" (xref "<global>" "String"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Boolean" (xref "<global>" "Boolean"))
                                Image)
            #:args (list '("string" "Text to draw") 
                         '("size" "Font size in pixels.") 
                         '("color" "Color of text.") 
                         '("font-face" "Font face to use for text.") 
                         '("font-family" "Font family to use for text.") 
                         '("style" "Style of text.") 
                         '("weight" "Weight of text.") 
                         '("underline" "Whether or not the text should be underlined."))]{
    Like @secref[(tag-name "image" "text")], constructs an image that draws the given
    string, but makes use of a complete font specification. This function
    corresponds to @racket[(text/font ...)] in HtDP.
  }
  @function[
    "name-to-color"
            #:contract (a-arrow (a-id "String" (xref "<global>" "String")))
            #:args (list '("name" ""))]{
  }
  @section{Polygons}
  @function[
    "triangle"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("side-length" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Constructs an image of an upward-pointing equilateral triangle. Each
    side will be of length @pyret["side-length"].
  }
  @function[
    "right-triangle"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("side-length1" "") 
                         '("side-length2" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Constructs an image of a triangle with a right angle at the bottom-left
    corner and where the two sides adjacent to the right angle have lengths
    @pyret["side-length1"] and @pyret["side-length2"].
  }
  @function[
    "isosceles-triangle"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("side-length" "") 
                         '("angle-c" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Constructs an image of a triangle with two equal-length sides, of
    length @pyret["side-length"] where the angle between those two sides is
    @pyret["angle-c"]. if the angle is less than @pyret["180"], then the triangle
    will point up; otherwise, the triangle will point down.
  }
  @function[
    "triangle-sss"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("side-a" "") 
                         '("side-b" "") 
                         '("side-c" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Constructs an image of a triangle using the three given sides. This function 
    corresponds to @racket[(triangle/sss ...)] in HtDP.
  }
  @function[
    "triangle-ass"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("angle-a" "") '("side-b" "") '("side-c" "") '("mode" "") '("color" ""))]{
    Constructs an image of a triangle using the given angle and two sides. This
    function corresponds to @racket[(triangle/ass ...)] in HtDP.
  }
  @function[
    "triangle-sas"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("side-a" "") 
                         '("angle-b" "") 
                         '("side-c" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Constructs an image of a triangle using the given angle and two sides. This
    function corresponds to @racket[(triangle/sas ...)] in HtDP.
  }
  @function[
    "triangle-ssa"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("side-a" "") 
                         '("side-b" "") 
                         '("angle-c" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Constructs an image of a triangle using the given angle and two sides. This
    function corresponds to @racket[(triangle/ssa ...)] in HtDP.
  }
  @function[
    "triangle-aas"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("angle-a" "") 
                         '("angle-b" "") 
                         '("side-c" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Constructs an image of a triangle using the two given angles and
    side. This function corresponds to @racket[(triangle/aas ...)] in HtDP.
  }
  @function[
    "triangle-asa"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("angle-a" "") 
                         '("side-b" "") 
                         '("angle-c" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Constructs an image of a triangle using the two given angles and
    side. This function corresponds to @racket[(triangle/asa ...)] in HtDP.
  }
  @function[
    "triangle-saa"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("side-a" "") 
                         '("angle-b" "") 
                         '("angle-c" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Constructs an image of a triangle using the two given angles and
    sides. This function corresponds to @racket[(triangle/saa ...)] in HtDP.
  }
  @function[
    "square"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("side-length" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Constructs an image of a square with the given side length, mode and color.
  }
  @function[
    "rectangle"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("width" "") 
                         '("height" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Constructs an image of a rectangle with the given side width, height,
    mode and color.
  }
  @function[
    "rhombus"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("side-length" "") 
                         '("angle" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Constructs a four-sided polygon whose sides are of length
    @pyret["side-length"] and thus has angles equal to their opposites. The
    top and bottom pair of angles is @pyret["angle"] and the left and right
    pair is @pyret["180 - angle"].
  }
  @function[
    "star"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("side-length" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Constructs a five-pointed star with sides of length @pyret["side-length"],
    and with the given mode and color.
  }
  @function[
    "radial-star"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("point-count" "") '("outer" "") '("inner" "") '("mode" "") '("color" ""))]{
    Constructs a star with @pyret["point-count"] points. The outer points will
    lie a distance of @pyret["outer"] from the center of the star, while the
    inner points will lie a distance of @pyret["inner"] from the center.
  }
  @function[
    "star-sized"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("point-count" "") 
                         '("outer" "") 
                         '("inner" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Same as @secref[(tag-name "image" "radial-star")].
  }
  @function[
    "star-polygon"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("side-length" "") 
                         '("point-count" "") 
                         '("step" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Constructs an image of an arbitrary regular star polygon. The polygon
    is enclosed by a regular polygon with @pyret["side-count"] sides each
    @pyret["side-length"] long. The polygon is actually constructed by going
    from vertex to vertex around the regular polygon, but connecting every
    @pyret["step-count"]-th vertex (i.e., skipping every 
    @pyret["step-count - 1"] vertices).
  }
  @function[
    "regular-polygon"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "String" (xref "<global>" "String"))
                                (a-id "Color"  (xref "image-structs" "Color"))
                                Image)
            #:args (list '("length" "") 
                         '("count" "") 
                         '("mode" "") 
                         '("color" ""))]{
    Constructs an image of a regular polygon with @pyret["side-count"] sides.
  }
  @section{Overlaying Images}
  @function[
    "overlay"
            #:contract (a-arrow Image
                                Image
                                Image)
            #:args (list '("img1" "") 
                         '("img2" ""))]{
    Constructs a new image where @pyret["img1"] overlays @pyret["img2"]. 
  }
  @function[
    "overlay-align"
            #:contract (a-arrow (a-id "is-x-place" (xref "image" "is-x-place"))
                                (a-id "is-x-place" (xref "image" "is-x-place"))
                                Image
                                Image
                                Image)
            #:args (list '("place-x" "") 
                         '("place-y" "") 
                         '("img1" "") 
                         '("img2" ""))]{
    Overlays @pyret["img1"] on @pyret["img2"] like 
    @secref[(tag-name "image" "overlay")], but uses @pyret["place-x"] and
    @pyret["place-y"] to determine where the images should line up.
  }
  @function[
    "overlay-xy"
            #:contract (a-arrow Image
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                Image
                                Image)
            #:args (list '("img1" "") 
                         '("dx" "") 
                         '("dy" "") 
                         '("img2" ""))]{
    Overlays @pyret["img1"] on @pyret["img2"] like 
    @secref[(tag-name "image" "overlay")], but initially lines up the two
    images upper-left corners and then shifts @pyret["img2"] to the right
    by @pyret["x"] pixels, and then down by @pyret["y"] pixels.
  }
  @function[
    "underlay"
            #:contract (a-arrow Image
                                Image
                                Image)
            #:args (list '("img1" "") 
                         '("img2" ""))]{
    Constructs a new image by placing @pyret["img1"] under @pyret["img2"].
  }
  @function[
    "underlay-align"
            #:contract (a-arrow (a-id "is-x-place" (xref "image" "is-x-place"))
                                (a-id "is-y-place" (xref "image" "is-y-place"))
                                Image
                                Image
                                Image)
            #:args (list '("place-x" "") 
                         '("place-y" "") 
                         '("img1" "") 
                         '("img2" ""))]{
    Underlays @pyret["img1"] beneath @pyret["img2"] like 
    @secref[(tag-name "image" "underlay")], but uses @pyret["place-x"] and
    @pyret["place-y"] to determine where the images should line up.
  }
  @function[
    "underlay-xy"
            #:contract (a-arrow Image
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                Image
                                Image)
            #:args (list '("img1" "") 
                         '("dx" "") 
                         '("dy" "") 
                         '("img2" ""))]{
    Underlays @pyret["img1"] beneath @pyret["img2"] like 
    @secref[(tag-name "image" "underlay")], but initially lines up the two
    images upper-left corners and then shifts @pyret["img2"] to the right
    by @pyret["x"] pixels, and then down by @pyret["y"] pixels.
  }
  @function[
    "beside"
            #:contract (a-arrow Image
                                Image
                                Image)
            #:args (list '("img1" "") 
                         '("img2" ""))]{
    Constructs an image by placing @pyret["img1"] to the left of
    @pyret["img2"].
  }
  @function[
    "beside-align"
            #:contract (a-arrow (a-id "is-y-place" (xref "image" "is-y-place"))
                                Image
                                Image
                                Image)
            #:args (list '("place-y" "") 
                         '("img1" "") 
                         '("img2" ""))]{
    Constructs an image by placing @pyret["img1"] to the left of
    @pyret["img2"], and aligning the two images as indicated by
    @pyret["place-y"].
  }
  @function[
    "above"
            #:contract (a-arrow Image
                                Image
                                Image)
            #:args (list '("img1" "") '("img2" ""))]{
    Constructs an image by placing @pyret["img1"] above @pyret["img2"].
  }
  @function[
    "above-align"
            #:contract (a-arrow (a-id "is-x-place" (xref "image" "is-x-place"))
                                Image
                                Image
                                Image)
            #:args (list '("place-x" "") 
                         '("img1" "") 
                         '("img2" ""))]{
    Constructs an image by placing @pyret["img1"] above @pyret["img2"],
    and aligning the two images as indicated by @pyret["place-x"].
  }
  @section{Placing Images & Scenes}
  @function[
    "empty-scene"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Scene" (xref "image" "Scene")))
            #:args (list '("width" "") 
                         '("height" ""))]{
    Construct an empty scene of given width and height.
  }
  @function[
    "put-image"
            #:contract (a-arrow Image
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Scene"  (xref "image" "Scene"))
                                (a-id "Scene"  (xref "image" "Scene")))
            #:args (list '("picture" "") 
                         '("x" "") 
                         '("y" "") 
                         '("background" ""))]{
    Places the image @pyret["img"] on the scene @pyret["background"] so that
    its center is located at the coordinates (x,y), cropping the resulting
    image as necessary to maintain the size of @pyret["background"]. The
    coordinates are relative to the bottom-left of @pyret["background"].
  }
  @function[
    "place-image"
            #:contract (a-arrow Image
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Scene"  (xref "image" "Scene"))
                                (a-id "Scene"  (xref "image" "Scene")))
            #:args (list '("img" "") 
                         '("x" "") 
                         '("y" "") 
                         '("background" ""))]{
    Places the image @pyret["img"] on the scene @pyret["background"] so that
    its center is located at the coordinates (x,y), cropping the resulting
    image as necessary to maintain the size of @pyret["background"]. The
    coordinates are relative to the top-left of @pyret["background"].
  }
  @function[
    "place-image-align"
            #:contract (a-arrow Image
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "is-x-place" (xref "image" "is-x-place"))
                                (a-id "is-y-place" (xref "image" "is-y-place"))
                                (a-id "Scene"  (xref "image" "Scene"))
                                (a-id "Scene"  (xref "image" "Scene")))
            #:args (list '("img" "") 
                         '("x" "") 
                         '("y" "") 
                         '("place-x" "") 
                         '("place-y" "") 
                         '("background" ""))]{
    Functions like @secref[(tag-name "image" "place-image")], but uses
    @pyret["place-x"] and @pyret["place-y"] to determine where to anchor
    @pyret["img"], instead of using the center. This function corresponds
    to @racket[(place-image/align ...)] in HtDP.
  }
  @function[
    "scene-line"
            #:contract (a-arrow (a-id "Scene"  (xref "image" "Scene"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Scene"  (xref "image" "Scene")))
            #:args (list '("img" "") 
                         '("x1" "") 
                         '("y1" "") 
                         '("x2" "") 
                         '("y2" "") 
                         '("background" ""))]{
    Draws a line from (x1,y1) to (x2,y2) on the scene
    @pyret["background"]. Unlike @secref[(tag-name "image" "add-line")],
    this function crops the resulting image to be the same size as
    @pyret["background"].
  }
  @section{Rotating, Scaling, Flipping, Cropping and Framing Images}
  @function[
    "rotate"
            #:contract (a-arrow (a-id "Number"  (xref "<global>" "Number"))
                                Image
                                Image)
            #:args (list '("angle" "") 
                         '("img" ""))]{
    Rotates @pyret["img"] counter-clockwise by @pyret["angle"] degrees.
  }
  @function[
    "scale"
            #:contract (a-arrow (a-id "Number"  (xref "<global>" "Number"))
                                Image
                                Image)
            #:args (list '("factor" "") 
                         '("img" ""))]{
    Scales @pyret["img"] by @pyret["factor"].
  }
  @function[
    "scale-xy"
            #:contract (a-arrow (a-id "Number"  (xref "<global>" "Number"))
                                (a-id "Number"  (xref "<global>" "Number"))
                                Image
                                Image)
            #:args (list '("x-factor" "") 
                         '("y-factor" "") 
                         '("img" ""))]{
    Scales by @pyret["x-factor"] horizontally and by @pyret["y-factor"]
    vertically.
  }
  @function[
    "flip-horizontal"
            #:contract (a-arrow Image
                                Image)
            #:args (list '("img" ""))]{
    Flips @pyret["img"] left to right.
  }
  @function[
    "flip-vertical"
            #:contract (a-arrow Image
                                Image)
            #:args (list '("img" ""))]{
    Flips @pyret["img"] top to bottom.
  }
  @function[
    "crop"
            #:contract (a-arrow (a-id "Number"  (xref "<global>" "Number"))
                                (a-id "Number"  (xref "<global>" "Number"))
                                (a-id "Number"  (xref "<global>" "Number"))
                                (a-id "Number"  (xref "<global>" "Number"))
                                Image
                                Image)
            #:args (list '("x" "") 
                         '("y" "") 
                         '("width" "") 
                         '("height" "") 
                         '("img" ""))]{
    Crops @pyret["img"] to the rectangle with the upper left at the point
    (x,y) and with width @pyret["width"] and height @pyret["height"].
  }
  @function[
    "frame"
            #:contract (a-arrow Image
                                Image)
            #:args (list '("img" ""))]{
    Construct an image similar to @pyret["img"], but with a black, single
    pixel frame draw around the bounding box of the image.
  }
  @section{Bitmaps}
  @function[
    "open-image-url"
            #:contract (a-arrow (a-id "String" (xref "<global>" "String"))
                                Image)
            #:args (list '("url" ""))]{
    Loads the image specified by @pyret["url"].
  }
  @function[
    "image-url"
            #:contract (a-arrow (a-id "String" (xref "<global>" "String"))
                                Image)
            #:args (list '("url" ""))]{
    Loads the image specified by @pyret["url"].
  }
  @function[
    "bitmap-url"
            #:contract (a-arrow (a-id "String" (xref "<global>" "String"))
                                Image)
            #:args (list '("url" ""))]{
    Loads the image specified by @pyret["url"].
  }
  @function[
    "image-to-color-list"
            #:contract (a-arrow Image
                                (a-app (a-id "List" (xref "lists" "List"))
                                       (a-id "Color" (xref "image-structs" "Color"))))
            #:args (list '("image" ""))]{
    Returns a list of colors that correspond to the colors in the image,
    reading from left to right, top to bottom. This function corresponds to
    @racket[(image->color-list ...)] in HtDP.
  }
  @function[
    "color-list-to-image"
            #:contract (a-arrow (a-app (a-id "List" (xref "lists" "List"))
                                       (a-id "Color" (xref "image-structs" "Color")))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                Image)
            #:args (list '("list" "") 
                         '("width" "") 
                         '("height" "") 
                         '("pinhole-x" "") 
                         '("pinhole-y" ""))]{
    Given a list of colors, creates an image with the given width
    @pyret["width"] and height @pyret["height"]. This function corresponds
    to @racket[(color-list->bitmap ...)] in HtDP.
  }
  @function[
    "color-list-to-bitmap"
            #:contract (a-arrow (a-app (a-id "List" (xref "lists" "List"))
                                       (a-id "Color" (xref "image-structs" "Color")))
                                (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Number" (xref "<global>" "Number"))
                                Image)
            #:args (list '("list" "") 
                         '("width" "") 
                         '("height" ""))]{
    Given a list of colors, creates an image with the given width
    @pyret["width"] and height @pyret["height"]. This function corresponds
    to @racket[(color-list->bitmap ...)] in HtDP.
  }
  @section{Image Properties}
  @function[
    "image-width"
            #:contract (a-arrow Image
                                (a-id "Number" (xref "<global>" "Number")))
            #:args (list '("img" ""))]{
    Returns the width of @pyret["img"].
  }
  @function[
    "image-height"
            #:contract (a-arrow Image
                                (a-id "Number" (xref "<global>" "Number")))
            #:args (list '("img" ""))]{
    Returns the height of @pyret["img"].
  }
  @function[
    "image-baseline"
            #:contract (a-arrow Image
                                (a-id "Number" (xref "<global>" "Number")))
            #:args (list '("img" ""))]{
    Returns the distance from the top of @pyret["img"] to its baseline. The
    baseline of an image is the place where the bottoms of letters line up,
    without counting the descender, such as the tails on "y", "g" or "j".
  }
  @section{Image Predicates}
  @function[
    "is-image"
            #:contract (a-arrow "Any"
                                (a-id "Boolean" (xref "<global>" "Boolean")))
            #:args (list '("maybe-image" ""))]{
    Checks if @pyret["maybe-image"] is an image.
  }
  @function[
    "is-mode"
            #:contract (a-arrow "Any"
                                (a-id "Boolean" (xref "<global>" "Boolean")))
            #:args (list '("maybe-mode" ""))]{
    Checks if @pyret["maybe-mode"] is a mode.
  }
  @function[
    "is-image-color"
            #:contract (a-arrow "Any"
                                (a-id "Boolean" (xref "<global>" "Boolean")))
            #:args (list '("maybe-color" ""))]{
    Checks if @pyret["maybe-color"] can be used as a color. Strings, if names of colors (e.g. "red" or "green") can also be used, if they exist in the color database.
  }
  @function[
    "is-y-place"
            #:contract (a-arrow "Any"
                                (a-id "Boolean" (xref "<global>" "Boolean")))
            #:args (list '("maybe-y-place" ""))]{
    Checks if @pyret["maybe-y-place"] can be used as y-place in appropriate
    functions. Valid strings are @pyret["top"], @pyret["bottom"],
    @pyret["middle"], @pyret["center"], @pyret["baseline"] and
    @pyret["pinhole"].
  }
  @function[
    "is-x-place"
            #:contract (a-arrow (a-id "String" (xref "<global>" "String"))
                                (a-id "Boolean" (xref "<global>" "Boolean")))
            #:args (list '("maybe-x-place" ""))]{
    Checks if @pyret["maybe-x-place"] can be used as x-place in appropriate
    functions. Valid strings are @pyret["left"], @pyret["right"],
    @pyret["middle"], @pyret["center"] and @pyret["pinhole"].
  }
  @function[
    "is-angle"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Boolean" (xref "<global>" "Boolean")))
            #:args (list '("maybe-angle" ""))]{
    Checks if @pyret["maybe-angle"] is an angle, namely a real number. All
    angles in the library are in degrees.
  }
  @function[
    "is-side-count"
            #:contract (a-arrow "Any"
                                (a-id "Boolean" (xref "<global>" "Boolean")))
            #:args (list '("side-count" ""))]{
    Checks if @pyret["maybe-side-count"] is an integer greater than or equal
    to 3.
  }
  @function[
    "is-step-count"
            #:contract (a-arrow (a-id "Number" (xref "<global>" "Number"))
                                (a-id "Boolean" (xref "<global>" "Boolean")))
            #:args (list '("step-count" ""))]{
    Checks if @pyret["maybe-step-count"] is an integer greater than or equal
    to 1.
  }
  @section{Image Equality}
  @function[
    "images-equal"
            #:contract (a-arrow Image
                                Image
                                (a-id "Boolean" (xref "<global>" "Boolean")))
            #:args (list '("image1" "") 
                         '("image2" ""))]{
    Compares two images for equality.
  }
}
