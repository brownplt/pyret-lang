provide *
provide-types *

import image as image
import image-structs as image-structs
import equality as E

data Color:
  | color(red :: Number, green :: Number, blue :: Number)
end

fun Image(img) -> Boolean:
  image.Image(img.img)
end

fun mk-image(img):
  {
    img : img,
    width : image.image-width(img),
    height : image.image-height(img),
    method _equals(self, other, rec-eq): rec-eq(self.img, other.img) end,
    method _torepr(self, rec-torepr):
      rec-torepr({
          img: "<use .img to see this at the repl>",
          width: self.width,
          height: self.height
        })
    end
  }
end

fun image-from-url(url :: String):
  img = image.bitmap-url(url)
  mk-image(img)
end

# TODO: make this not fail
#fun image-from-file(file :: String) -> Image:
#  img = image.bitmap-file(file)
#  {img : img, width : image.image-width(img), height : image.image-height(img)}
#end
  
TRAFALGAR-SQUARE = image-from-url("http://cs.brown.edu/courses/cs019/2012/images/seam_carving/trafalgar-square.jpg")
ULURU = image-from-url("http://cs.brown.edu/courses/cs019/2012/images/seam_carving/uluru.jpg")

# output: a list of rows from the image, with each row being a list of the
#         colors in that row
fun image-to-2d-color-list(img) -> List:
  width = image.image-width(img.img)
  
  fun to-color(c):
    # TA: this is converting image-structs.color to our manual color, which is just RGB. This should all be changed to use the image-structs color.
    color(c.red, c.green, c.blue)
  end
  
  fun convert(lst :: List) -> List:
    cases(List) lst:
      | empty => empty
      | link(_, _) => link(lst.take(width).map(to-color),
                           convert(lst.drop(width)))
    end
  end
  
  convert(image.image-to-color-list(img.img))
end

# input:  a matrix of colors in the format of image-to-2d-color-list
# output: an image
fun image-from-2d-color-list(img :: List):
  width = img.first.length()
  height = img.length()
  
  fun append-row(row, bottom) block:
    when row.length() <> width:
        raise("image is not a rectangle")
    end
    for map(pixel from row):
      image-structs.color(pixel.red, pixel.green, pixel.blue, 255)
    end.append(bottom)
  end
  mk-image(image.color-list-to-bitmap(img.foldr(append-row, empty), width, height))
end

# input:  two images
# output: true if they are equal, false otherwise
fun image-equals(a, b):
  a.img == b.img
where:
  image-equals(ULURU, ULURU) is true
  image-equals(ULURU, TRAFALGAR-SQUARE) is false
end

check:
  TRAFALGAR-SQUARE is%(image-equals) image-from-2d-color-list(image-to-2d-color-list(TRAFALGAR-SQUARE))
end

