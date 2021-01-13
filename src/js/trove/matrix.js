({
  requires:
    [
      { "import-type": "builtin", name: "valueskeleton" },
      {"import-type" : "builtin" , name: "lists"} ,
      {"import-type": "builtin", name:"arrays"}
    ],
  nativeRequires: [],
  provides: {
    shorthands: {
      //"matOfAny": ["tyapp", ["local", "Matrix"], []],
      "Equality": { tag: "name",
                    origin: { "import-type": "uri", uri: "builtin://equality" },
                    name: "EqualityResult" },
      "VS": { tag: "name",
                    origin: { "import-type": "uri", uri: "builtin://valueskeleton" },
                    name: "ValueSkeleton" },
      "List" : { tag: "name",
      origin: { "import-type": "uri", uri: "builtin://list" },
      name: "List" },
      "Array" : { tag: "name",
      origin: { "import-type": "uri", uri: "builtin://array" },
      name: "Array" },            
      "tva": ["tid", "a"],
      "tvb": ["tid", "b"]
    },
    values: {
      "mat" : ["arrow" , ["Number", "Number"] ,  ["Maker", "Any", ["local", "Matrix"]]]
      /*
      "row-map" : ["arrow", [["arrow" ["Vector"] , "Vector" ] , "Matrix"]  , "Matrix"  ],
      "col-map" : ["arrow" ,[["arrow" ["Vector"] , "Vector" ] , "Matrix"]  , "Matrix"  ],
      "map" : ["arrow" ,[["arrow" ["Number"] , "Number" ] , "Matrix"]  , "Matrix"  ],
      "transpose" : ["arrow", ["Matrix"] , "Matrix"] , 
      "sub-matrix" : ["arrow", ["Number" , "Number" , "Number","Number"] , "Matrix"] ,
      "get-row" : ["arrow" ,["Matrix"] , "Vector"] , 
      "get-col" : ["arrow" ,["Matrix"] , "Vector"] , 
      "dimensions" : ["arrow" ,["Matrix"] , ["List", "Number"] ] ,
      "add" : ["arrow", ["Matrix", "Matrix"] , "Matrix"] , 
      "subtract" : ["arrow", ["Matrix", "Matrix"] , "Matrix"] , 
      "multiply" : ["arrow" ,["Matrix", "Matrix"] , "Matrix"] , 
      "determinant" : ["arrow", ["Matrix"] , "Number"] , 
      "frobenius-norm" : ["arrow", ["Matrix"] , "Number"] , 
      "norm" : ["arrow", ["Matrix" , "Number"] , "Number" ]   ,
      "inverse" : ["arrow", ["Matrix"] , "Matrix"] , 
      "exponent": ["arrow" ,["Matrix" , "Number"] , "Number" ]  ,
      "dot-product" : ["arrow", ["Vector" , "Vector" ] , "Number" ] ,
      "scale" : ["arrow" ,["Matrix" , "Number"] , "Number" ]   ,
      "stack" : ["arrow" ,["Matrix", "Matrix"] , "Matrix"] , 
      "vector-to-list"  : ["arrow", ["Vector"] ,  "List"] ,
      "vector-to-array" : ["arrow", ["Vector"] , "Array"] , 
      "get" : ["arrow", ["Matrix" , "Number", "Number" ] , "Number"] ,*/
    },
    aliases: {
        "Matrix" : {
          tag: "name", 
          origin : {"import-type": "$ELF"} ,
          name : "Matrix" 
        },
        "Vector" : {
          tag: "name", 
          origin : {"import-type": "$ELF"} ,
          name : "Vector" 
        }
    },
    datatypes: {
      "Matrix": ["data", "Matrix", [], [], {
        /*
      "_equals": ["arrow", ["Matrix", ["arrow", ["Any", "Any"], "Equality"]], "Equality"],
      "_plus" : ["arrow" ,["Matrix"] , "Matrix"] ,
      "_minus" : ["arrow" , ["Matrix"] , "Matrix" ],
      "_times" : ["arrow" , ["Matrix"] , "Matrix" ]*/
           }],
      "Vector": ["data", "Vector", [], [], {
        /*
          "_equals": ["arrow", ["Vector", ["arrow", ["Any", "Any"], "Equality"]], "Equality"],
          "_plus" : ["arrow" ,["Vector"] , "Vector"] ,
      "_minus" : ["arrow" , ["Vector"] , "Vector" ],
      "_times" : ["arrow" , ["Vector"] , "Vector" ]*/
      }],
    }
  },
  theModule: function(runtime, namespace, uri, VSlib ,LSlib,ARRLib){
    var O = runtime.makeObject;
    var F = runtime.makeFunction;
    var arity = runtime.checkArity;
    var get = runtime.getField;

    var VS = get(VSlib, "values");
    var LS = get(LSlib,"values")  ;
    var ARR = get(ARRLib,"values")  ; 
   
    var brandMatrix = runtime.namedBrander("matrix",["matrix: matrix brander"]);
    var brandVector = runtime.namedBrander("vector",["vector: vector brander"])  ;

    var annMatrix = runtime.makeBranderAnn(brandMatrix,"Matrix") ; 
    var annVector = runtime.makeBranderAnn(brandVector,"Vector") ; 

    var checkMtrx = function(v) {runtime._checkAnn(['matrix'],annMatrix,v)} ; 
    var checkVec = function(v) { runtime._checkAnn(['vector'],annVector,v)} ; 
    function applyBrand(brand, val) {
      return get(brand, "brand").app(val);
    }
    function hasBrand(brand, val) {
      return get(brand, "test").app(val);
    }

    function internal_isMtrx(obj) {
        return hasBrand(brandMatrix,obj) ;
    }
    function internal_isVec(obj) { 
        return hasBrand(brandVector,obj) ; 
    }
  
    function makeMatrix(h, w, underlyingMat){
      var obj = O({});
      // Applying a brand creates a new object, so we need to add the reflective field afterward
      obj = applyBrand(brandMatrix, obj);
      obj.$underlyingMat = underlyingMat;
      obj.$h = h;
      obj.$w = w;

      return obj;
    }
    function createMatrixFromArray(h, w, array){
      arity(3, arguments, "matrix", false);
      runtime.checkArray(array);
      var matr = [];
      var len = array.length;
      if(h * w != len){
          runtime.ffi.throwMessageException("The number of provided elements does not match the given width and height.");
      }
      for(var i = 0; i < h; i += 1){
        var row = [];
        for (var j = 0; j < w; j += 1){
          console.log(array[(i * w) + j])
          runtime.checkNumber(array[(i * w) + j]);
          row.push(array[(i * w) + j]);
        }
        matr.push(row);
      }
      return makeMatrix(h, w, matr);
    }
    function matrixInit(h, w){
      if(!(Number.isInteger(h)) || !(Number.isInteger(w)) || h < 0 || w < 0){
          runtime.ffi.throwMessageException("The provided width or height is invalid.");
      }
      return O({
          make: F((arr)=>{return createMatrixFromArray(h, w, arr)}, "matrix:make"),
          make0: F(()=>{return createMatrixFromArray(h, w, runtime.makeArray([]))}, "matrix:make0"),
          make1: F((a)=>{return createMatrixFromArray(h, w, runtime.makeArray([a]))}, "matrix:make1"),
          make2: F((a, b)=>{return createMatrixFromArray(h, w, runtime.makeArray([a, b]))}, "matrix:make2"),
          make3: F((a, b, c)=>{return createMatrixFromArray(h, w, runtime.makeArray([a, b, c]))}, "matrix:make3"),
          make4: F((a ,b, c, d)=>{return createMatrixFromArray(h, w, runtime.makeArray([a, b, c, d]))}, "matrix:make4"),
          make5: F((a, b, c, d, e)=>{return createMatrixFromArray(h, w, runtime.makeArray([a, b, c, d, e]))}, "matrix:make5")
        });
    }

    var jsCheckMtrx = runtime.makeCheckType(internal_isMtrx,"Matrix")  ;
    var jsCheckVec = runtime.makeCheckType(internal_isVec,"Vector") ; 
    var vals = {
      "mat" : F(matrixInit, "mat")
      }
    var types = {
        Matrix : annMatrix,
        Vector : annVector 
    }
    var internal = {
        checkMtrx : jsCheckMtrx , 
        checkVec : jsCheckVec
    }
    
    return runtime.makeModuleReturn(vals,types,internal) ; 
  }
})