define(["js/runtime-util", "js/type-util"], function(util, t) {
  return util.definePyretModule(
    "convert-primitive-types",
    [
      {
        "import-type": "legacy-path",
        args: ["compiler/type-structs.arr"]
      }
    ],
    {
      values:
        {
          "convert-type": t
    }

  )

});
