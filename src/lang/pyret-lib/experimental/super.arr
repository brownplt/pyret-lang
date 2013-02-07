fun super(obj):
  builtins.keys(obj).foldr(\name, obj-super: (
    # need to test for methods
    obj-super.{
      [name]: (
