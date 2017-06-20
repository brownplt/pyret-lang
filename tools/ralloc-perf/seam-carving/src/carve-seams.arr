import file("fluid-images-2014.arr") as F
import file("fluid-images-support-2014.arr") as FS
import image as I

taj = F.image-from-url("https://lh3.googleusercontent.com/proxy/469BTDimN_0co0qFKvHhdENq8TCwaC5UHvJ1yW2uFZFaQ2jOPBR1Prmxi5MMRi33l5PzGHH8YsIrxd6XKdm-PHWCKOsckUDFCfV8bP3Z7jl6h4DCccmvQdomBXrJzq_A46pV6NExNxkukj3umvAPwAdpWfns=w506-h284")

carved = F.seam-carve(taj, 1)