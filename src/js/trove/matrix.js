/*
TODO: 
Fix Equality function
Figure out mat-dim type issue
replace duplicate function,method for +,-,*
Fix Vector


*/
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
      "Matrix": ["tyapp", ["local", "Matrix"], []],
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
      "mat" : ["arrow" , ["Number", "Number"] ,  ["Maker", "Any", ["local", "Matrix"]]],
      "vector" : ["arrow",[], ["Maker", "Any", ["local", "Vector"]]],
      "add-mat" : ["arrow", ["Matrix", "Matrix"] , "Matrix"] , 
     "sub-mat" : ["arrow", ["Matrix", "Matrix"] , "Matrix"] , 
      "mult-mat" : ["arrow" ,["Matrix", "Matrix"] , "Matrix"] , 
     "get-elem" : ["arrow", ["Matrix" , "Number", "Number" ] , "Number"],
     "transpose" : ["arrow", ["Matrix"] , "Matrix"] , 
     "stack-mat" : ["arrow" ,["Matrix", "Matrix"] , "Matrix"] ,
     "scale" : ["arrow" ,["Matrix" , "Number"] , "Number" ]   ,
      "set-elem" : ["arrow",["Matrix","Number","Number","Number"],"Matrix"],
      "reshape" : ["arrow",["Matrix","Number","Number"],"Matrix"],
      "matrix-map" : ["arrow" ,["Matrix",["arrow" ,["Number","Number","Number"],"Number" ]]  , "Matrix"  ],
     // "row-map" : ["arrow", [["arrow" ["Vector"] , "Vector" ] , "Matrix"]  , "Matrix"  ],
      //"col-map" : ["arrow" ,[["arrow" ["Vector"] , "Vector" ] , "Matrix"]  , "Matrix"  ],
     /* "mat-dims" : ["arrow" ,[["Matrix"] , ["List", "Number"]], "tva"]   */

      /*


      
      "sub-matrix" : ["arrow", ["Number" , "Number" , "Number","Number"] , "Matrix"] ,
      "get-row" : ["arrow" ,["Matrix"] , "Vector"] , 
      "get-col" : ["arrow" ,["Matrix"] , "Vector"] , 
      "determinant" : ["arrow", ["Matrix"] , "Number"] , 
      "frobenius-norm" : ["arrow", ["Matrix"] , "Number"] , 
      "norm" : ["arrow", ["Matrix" , "Number"] , "Number" ]   ,
      "inverse" : ["arrow", ["Matrix"] , "Matrix"] , 
      "exponent": ["arrow" ,["Matrix" , "Number"] , "Number" ]  ,
      "dot-product" : ["arrow", ["Vector" , "Vector" ] , "Number" ] ,

      
      "vector-to-list"  : ["arrow", ["Vector"] ,  "List"] ,
      "vector-to-array" : ["arrow", ["Vector"] , "Array"] , 
      ,*/
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
        "_output":  ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
        
      "_equals": ["arrow", ["Matrix", ["arrow", ["Any", "Any"], "Equality"]], "Equality"],
      
      "_plus" : ["arrow" ,["Matrix"] , "Matrix"] ,
      "_minus" : ["arrow" , ["Matrix"] , "Matrix" ],
      "_times" : ["arrow" , ["Matrix"] , "Matrix" ]
           }],
      "Vector": ["data", "Vector", [], [], {
        "_output":  ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
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
    function sameDims(self,other){
      (checkMtrx(self) && checkMtrx(other)) ; 
      return (self.$h == other.$h) || (self.$w == other.$w)
     }
    function printDims(self) { 
      checkMtrx(self) ; 
      return "(" + self.$h + "," + self.$w + ")" ; 
     }

    function get1d(mtrx,h,w){
      checkMtrx(mtrx) ; 
     return mtrx.$underlyingMat[(h * mtrx.$w) + w ] ; 
    }
    function get1dpos(h,w,c) {
      return (h * c) + w ;
    }


    function posInteger(h,w) {
      if ((h > 0) && (w > 0) && Number.isInteger(h) && Number.isInteger(w)){
        return true
      } else{
        runtime.ffi.throwMessageException("Dimensions need to be positive integers") ;
      }
    }
    function checkRange(mtrx,h,w) { 
      if( !posInteger(h,w) || (h >= mtrx.$h) || (w >= mtrx.$w) ){
        runtime.ffi.throwMessageException("Given dimensions are not valid") ;
      }
      return true ; 
    }
    
    function duplicateArray(mtrx,start,end,arr,offset) { 
      len = end - start  ; 
      for (var i = 0 ; i < len ; i++) {
        arr[i + offset] = mtrx.$underlyingMat[start + i ] ; 
      }
      return arr; 
    }

    function duplicateMatrix(self){
      new_arr = new Array(self.$l) ;
      duplicateArray(self,0,self.$l,new_arr,0) ; 
      return createMatrixFromArray(self.$h,self.$w,new_arr) ;

    }
    var funcaddMatrix = function(self,other){
      runtime.ffi.checkArity(2,arguments,"add-mat",false) ; 
      runtime.checkArgsInternal2("matrix","add-mat",self,annMatrix,other,annMatrix) ; 
      if(!sameDims(self,other)){
        return runtime.ffi.throwMessageException("Matrices have dimensions " + printDims(self) + " and " + printDims(other) + " . They cannot be added" ) ; 
      } else{
        new_arr = new Array(self.$l) ; 
        for(var i  = 0 ; i < self.$l ; i++) {
          new_arr[i] = self.$underlyingMat[i] + other.$underlyingMat[i] ;
        }

        return createMatrixFromArray(self.$h,self.$w,new_arr) ; 
      }
    };

    var funcsubMatrix = runtime.makeFunction(function(self,other){
      runtime.ffi.checkArity(2,arguments,"sub-mat",false) ; 
      runtime.checkArgsInternal2("matrix","sub-mat",self,annMatrix,other,annMatrix) ; 
      if(!sameDims(self,other)){
        return runtime.ffi.throwMessageException("Matrices have dimensions " + printDims(self) + " and " + printDims(other) + " . They cannot be added" ) ; 
      } else{
        new_arr = new Array(self.$l) ; 
        for(var i  = 0 ; i < self.$l ; i++) {
          new_arr[i] = self.$underlyingMat[i] - other.$underlyingMat[i] ;
        }

        return createMatrixFromArray(self.$h,self.$w,new_arr) ; 
      }
    },"sub-mat") ; 

    var funcmultMatrix = runtime.makeFunction(function(self,other){
      runtime.ffi.checkArity(2,arguments,"mult-mat",false) ; 
      runtime.checkArgsInternal2("Matrix" ,"mult-mat",self,annMatrix,other,annMatrix) ; 
      if(self.$w != other.$h) { 
        return runtime.ffi.throwMessageException("The width of the first matrix and the height of the second matrix need to be equal") ; 

      } else{
        new_arr = new Array((self.$h * other.$w)) ;
        for (var i = 0 ; i < self.$h ; i++ ) { 
          for (var  j = 0 ; j < other.$w ; j++) {
            var elm  = 0 ; 
            for (var k = 0 ; k < self.$w ; k++){
              elm+=(get1d(self,i,k)*get1d(other,k,j)) ; 
            } 
            new_arr[get1dpos(i,j,other.$w)]  =  elm ; 
          }
        }
        return createMatrixFromArray(self.$h,other.$w,new_arr) ; 
      }
    },"mult-mat") ; 

    var outputMatrix = runtime.makeMethod0(function(self) {
      //if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['_output'], 1, $a, true); }
      var rows = [];
      var matr = self.$underlyingMat;
      var vsValue = get(VS, "vs-value");
      for (var i = 0; i < matr.length; i++){
        rows.push(vsValue.app(matr[i]));
      }
      return get(VS, "vs-collection").app(
        runtime.makeString("mat" + printDims(self)),
        runtime.ffi.makeList(rows))
    });

    
     var getMatrixDims = runtime.makeFunction(function(self) { 
      runtime.ffi.checkArity(1,arguments,"mat-dims",false) ; 
      runtime.checkArgsInternal1("Matrix","mat-dims",self,annMatrix) ;
      return runtime.makeTuple([self.$h,self.$w]) ; 
     } , "mat-dims") ; 

    var getMatrixElms = runtime.makeFunction(function(self,h,c) { 
      runtime.ffi.checkArity(3,arguments,"get-elem",false) ; 
      runtime.checkArgsInternal3("Matrix","get-elem",self,annMatrix,h,runtime.Number,c,runtime.Number ) ;
      if (checkRange(self,h,c)) {
        return runtime.makeNumber(get1d(self,h,c)) ; 
      }

    },"get-elem") ;

    var setMatrixElms = function(self,h,w,num){
      arity(4,arguments,"set-elem",false) ;
      runtime.checkArgsInternalInline("Matrix","set-elem",self,annMatrix,h,runtime.Number,w,runtime.Number,num,runtime.Number) ;
      if(checkRange(self,h,w)){
        new_mtrx = duplicateMatrix(self);
        new_mtrx.$underlyingMat[get1dpos(h,w,self.$w)]  = num ;
        return new_mtrx ;
      }

    }

    var reshapeMatrix = function(self,h,w) {
      arity(3,arguments,"reshape",false) ;
      runtime.checkArgsInternalInline("Matrix","reshape",self,annMatrix,h,runtime.Number,w,runtime.Number) ;
      posInteger(h,w) ;
      if((h * w) != self.$l) {
        runtime.ffi.throwMessageException("Given dimensions do not match the matrix") ;
      } else {
        mtrx = duplicateMatrix(self) ;
        mtrx.$h = h ;
        mtrx.$w = w ;
        return mtrx ;
      }

    }
    var transposeMatrix = runtime.makeFunction(function(self){
      runtime.ffi.checkArity(1,arguments,"transpose",false) ; 
      runtime.checkArgsInternal1("Matrix","transpose",self,annMatrix);
      new_arr = new Array(self.$l) ; 
      for(var i = 0 ; i < self.$h ; i++){
        for(var j = 0 ; j < self.$w ; j++) {
          new_arr[get1dpos(j,i,self.$h)] = get1d(self,i,j) ; 
        }
      }
      return createMatrixFromArray(self.$w,self.$h,new_arr) ;

    },"transpose") ;

    var stackMatrix = runtime.makeFunction(function(self,other){
      runtime.ffi.checkArity(2,arguments,"stack-mat",false) ; 
      runtime.checkArgsInternal2("Matrix","stack-mat",self,annMatrix,other,annMatrix) ; 
      if(self.$w != other.$w) { 
        runtime.ffi.throwMessageException("Matrices need to have same width to be stacked " )
      } else{
        new_arr = new Array(self.$l + other.$l ) ; 
        duplicateArray(self,0,self.$l,new_arr,0) ; 
        duplicateArray(other,0,other.$l,new_arr,self.$l) ; 
        return createMatrixFromArray(self.$h + other.$h,self.$w,new_arr) ; 
      }
    },"stack-mat") ; 

    var scaleMatrix = runtime.makeFunction(function(self,num){
      runtime.ffi.checkArity(2,arguments,"scale",false) ; 
      runtime.checkArgsInternal2("Matrix","scale",self,annMatrix,num,runtime.Number) ; 
      new_mtrx = duplicateMatrix(self) ; 
      for (var i = 0 ; i < new_mtrx.$l ; i++) {
        new_mtrx.$underlyingMat[i] = new_mtrx.$underlyingMat[i] * num ; 
      }
      return new_mtrx ; 
    },"scale") ;

    var mapMatrix = function(self,f) {
      arity(2,arguments,"matrix-map",false) ;
      runtime.checkArgsInternalInline("Matrix","matrix-map",self,annMatrix,f,runtime.Function) ;
      new_mtrx = duplicateMatrix(self) ;
      for(var i = 0 ; i < new_mtrx.$l ; i++) {
          var h = Math.floor(i/new_mtrx.$h) ;
          var w = i%new_mtrx.$w ;
           new_mtrx.$underlyingMat[i] = f.app(h,w,new_mtrx.$underlyingMat[i] );

      }
      return new_mtrx ;
    }

    function makeMatrix(h, w, underlyingMat){
      var equalMatrix =  runtime.makeMethod2(function(self,other,Eq){
         runtime.ffi.checkArity(3, arguments, "_equals", true);
        runtime.checkArgsInternal3("matrix", "_equals",self, annMatrix, other, annMatrix, Eq, runtime.Function);    
       
        if(!hasBrand(brandMatrix,other)){
          return runtime.ffi.notEqual.app('',self,other) ;
        } else if (!sameDims(self,other)) { 
          return runtime.ffi.notEqual.app('',self,other) ;
        } else { 
          for( var i = 0 ; i < self.$l ; i++) { 
              if (self.$underlyingMat[i] != other.$underlyingMat[i]){
                return runtime.ffi.notEqual.app('',self,other) ;
              }
            } 
            return runtime.ff.equal ; 
        
      }},"equals") ;  
      var addMatrix = runtime.makeMethod1(function(self,other){
        runtime.ffi.checkArity(2,arguments,"_plus",true) ; 
        runtime.checkArgsInternal2("matrix","_plus",self,annMatrix,other,annMatrix) ; 
        if(!sameDims(self,other)){
          return runtime.ffi.throwMessageException("Matrices have dimensions " + printDims(self) + " and " + printDims(other) + " . They cannot be added" ) ; 
        } else{
          new_arr = new Array(self.$l) ; 
          for(var i  = 0 ; i < self.$l ; i++) {
            new_arr[i] = self.$underlyingMat[i] + other.$underlyingMat[i] ;
          }
  
          return createMatrixFromArray(self.$h,self.$w,new_arr) ; 
        }
      },"plus") ; 
      var minusMatrix = runtime.makeMethod1(function(self,other){
        runtime.ffi.checkArity(2,arguments,"_minus",true) ; 
        runtime.checkArgsInternal2("matrix","_minus",self,annMatrix,other,annMatrix) ; 
        if(!sameDims(self,other)){
          return runtime.ffi.throwMessageException("Matrices have dimensions " + printDims(self) + " and " + printDims(other) + " . They cannot be added" ) ; 
        } else{
          new_arr = new Array(self.$l) ; 
          for(var i  = 0 ; i < self.$l ; i++) {
            new_arr[i] = self.$underlyingMat[i] - other.$underlyingMat[i] ;
          }
  
          return createMatrixFromArray(self.$h,self.$w,new_arr) ; 
        } 
      },"minus") ; 
      var timesMatrix = runtime.makeMethod1(function(self,other) {
        runtime.ffi.checkArity(2,arguments,"_times",true) ; 
        runtime.checkArgsInternal2("Matrix" ,"_times",self,annMatrix,other,annMatrix) ; 
        if(self.$w != other.$h) { 
          return runtime.ffi.throwMessageException("The width of the first matrix and the height of the second matrix need to be equal") ; 
  
        } else{
          new_arr = new Array((self.$h * other.$w)) ;
          for (var i = 0 ; i < self.$h ; i++ ) { 
            for (var  j = 0 ; j < other.$w ; j++) {
              var elm  = 0 ; 
              for (var k = 0 ; k < self.$w ; k++){
                elm+=(get1d(self,i,k)*get1d(other,k,j)) ; 
              } 
              new_arr[get1dpos(i,j,other.$w)]  =  elm ; 
            }
          }
          return createMatrixFromArray(self.$h,other.$w,new_arr) ; 
        }
      },"times") ; 
      var get_height = runtime.makeMethod0(function(self){return self.$h},"get-height");
      var get_width = runtime.makeMethod0(function(self){return self.$w},"get-width") ;
      var get_shape = runtime.makeMethod0(function(self){return runtime.makeTuple([self.$h,self.$w])},"get-shape") ; 
      var obj = O({
        _output: outputMatrix,
        _plus : addMatrix,
        _minus : minusMatrix ,
        _equals : equalMatrix,
        _times : timesMatrix ,
        "get-height": get_height,
        "get-width" : get_width ,
        "get-shape" : get_shape 
       });
      // Applying a brand creates a new object, so we need to add the reflective field afterward
      obj = applyBrand(brandMatrix, obj);
      obj.$underlyingMat = underlyingMat;
      obj.$h = h;
      obj.$w = w;
      obj.$l = h * w ; 

      return obj;
    }

    var outputVector = runtime.makeMethod0(function(self) {
      arity(1,arguments,"_output",false) ;
      var rows = [];
      var matr = self.$underlyingMat;
      var vsValue = get(VS, "vs-value");
      for (var i = 0; i < matr.length; i++){
        rows.push(vsValue.app(matr[i]));
      }
      return get(VS, "vs-collection").app(
          runtime.makeString("vector(" + self.$l + "):"),
          runtime.ffi.makeList(rows))
    });
    function makeVector(underlyingArr){
      var obj = O({
        _output : outputVector
      }) ;
      obj = applyBrand(brandVector,obj) ;
      obj.$underlyingMat = underlyingArr ;
      obj.$l = underlyingArr.length ;
      return obj ;
    }
    function createMatrixFromArray(h, w, array){
      arity(3, arguments, "matrix", false);
      runtime.checkArray(array);
      var matr = [];
      var len = array.length;
      if(h * w != len){
          runtime.ffi.throwMessageException("The number of provided elements does not match the given width and height.");
      }
      matr = [...array] ; 
      return makeMatrix(h, w, matr);
    }

    function createVectorFromArray(arr){
      arity(1,arguments,"vector",false);
      runtime.checkArray(arr);
      makeVector([...arr]) ;

    }
    function matrixInit(h, w){
      if(!(Number.isInteger(h)) || !(Number.isInteger(w)) || h < 0 || w < 0){
          runtime.ffi.throwMessageException("The provided width or height is invalid. Matrix dimensions need to be a positive non zero integer");
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
    function vectorInit() {
      return O({
        make: F((arr)=>{return createVectorFromArray(arr)},"vector:make"),
        make0: F(()=>{return createVectorFromArray( runtime.makeArray([]) )},"vector:make0"),
        make1: F((a)=>{return createVectorFromArray(runtime.makeArray([a]))},"vector:make1"),
        make2: F((a, b)=>{return createVectorFromArray(runtime.makeArray([a],b))},"vector:make2"),
        make3: F((a, b, c)=>{return createVectorFromArray(runtime.makeArray([a,b,c]))},"vector:make3"),
        make4: F((a ,b, c, d)=>{return createVectorFromArray(runtime.makeArray([a,b,c,d]))},"vector:make4"),
        make5: F((a, b, c, d, e)=>{return createVectorFromArray(runtime.makeArray([a,b,c,d,e]))},"vector:make5"),
      })
    }

    var jsCheckMtrx = runtime.makeCheckType(internal_isMtrx,"Matrix")  ;
    var jsCheckVec = runtime.makeCheckType(internal_isVec,"Vector") ; 
    var vals = {
      "mat" : F(matrixInit, "mat"),
      "vector": F(vectorInit,"vector") ,
      "add-mat" : F(funcaddMatrix ,"add-mat"),
     "sub-mat": funcsubMatrix , 
      "mult-mat" : funcmultMatrix ,
      "get-elem" : getMatrixElms,
      "transpose" : transposeMatrix,
      "stack-mat" : stackMatrix ,
      "scale" : scaleMatrix ,
      "set-elem" : F(setMatrixElms,"set-elem") ,
      "reshape" : F(reshapeMatrix,"reshape"),
      "matrix-map" : F(mapMatrix,"matrix-map")
    
   //  "mat-dims" : getMatrixDims 
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