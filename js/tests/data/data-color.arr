data Color:
 | red with: asRGB(_): { r: 256, g: 0, b: 0 } end
 | green with: asRGB(_): { r: 0, g: 256, b: 0 } end
 | blue with: asRGB(_): { r: 0, g: 0, b: 256 } end
 | rgb(r :: Number, g :: Number, b :: Number) with:
    asRGB(self): { r: self.r, g: self.g, b: self.b } end
end
fun asRGB(obj): obj.asRGB() end
fun getR(obj): obj.r end
[rgb(100,100,100), red, green].map(asRGB).map(getR)._equals([100, 256, 0])