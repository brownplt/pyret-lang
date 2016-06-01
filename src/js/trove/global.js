({
  requires: [ ],
  provides: {
    values: {},
    aliases: {
      "Number": "tany",
      "Exactnum": "tany",
      "Roughnum": "tany",
      "NumInteger": "tany",
      "NumRational": "tany",
      "NumPositive": "tany",
      "NumNegative": "tany",
      "NumNonPositive": "tany",
      "NumNonNegative": "tany",
      "String": "tany",
      "Function": "tany",
      "Boolean": "tany",
      "Object": "tany",
      "Method": "tany",
      "Nothing": "tany",
      "RawArray": "tany"
    },
    datatypes: {}
  },
  nativeRequires: [ ],
  theModule: function(runtime, namespace, uri /* intentionally blank */) {
    return runtime.globalModuleObject;
  }
})
