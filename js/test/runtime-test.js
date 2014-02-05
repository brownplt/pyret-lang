var r = require("requirejs")

define(["./matchers"], function (matchers) {

    _ = require('jasmine-node');
    var path = require('path');
    var addPyretMatchers = matchers.addPyretMatchers;
    function simulateBrand(obj, name) {
      obj.brands = Object.create(obj.brands);
      obj.brands["$brand" + name] = true;
      obj.brands.brandCount++;
    }
    function simulateHasBrand(obj, name) {
      return obj.brands["$brand" + name] === true;
    }

    function performTest(useCompiled) {

      if(useCompiled) {
        var R = r('../anf-comp');
      }
      else {
        var R = r('../runtime-anf');
      }

      var output;
      var rt;

      /**@ param {string} str, output*/
      function stdout(str) {
          output += str;
      }

      //Test functions for creating functions/method
      function x() {return makeNumber(3);}
      function y(a) {return makeNumber(3);}
      function z(a,b) {return makeNumber(3);}
      function w(a,b,c) {return makeNumber(3);}

      function y_curry(a) {return function() {makeNumber(3);}}
      function z_curry(a) {return function(b) {makeNumber(3);}}
      function w_curry(a) {return function(b, c) {makeNumber(3);}}

      //Test varaibles of each type
      var aNum;
      var aBool;
      var aStr;
      var aFun;
      var aMeth;
      var aNoth;
      var anObj;

      beforeEach(function(){

          output = "";
          rt = R.makeRuntime({'stdout' : stdout});
          addPyretMatchers(this, rt);

          //Make Examples for testing
          aNum = rt.makeNumber(42);
          aBool = rt.makeBoolean(true);
          aStr = rt.makeString("pyret");
          aFun = rt.makeFunction(x);
          aMeth = rt.makeMethod(y_curry, y);
          aNoth = rt.makeNothing();
          anObj = rt.makeObject({});
      });


      describe("Basic values", function() {
          it("should have an n field on numbers", function() {
            expect(rt.makeNumber(5).n).toEqual(5);
          });

          it("should have an b field on booleans", function() {
            expect(rt.makeBoolean(true).b).toEqual(true);
            expect(rt.makeBoolean(false).b).toEqual(false);
          });

          it("should have an s field on strings", function() {
            expect(rt.makeString("hello world").s).toEqual("hello world");
            expect(rt.makeString("\n").s).toEqual("\n");
          });

          it("should have an app field on functions", function() {
            function x() {return makeNumber(3);}
            expect(rt.makeFunction(x).app).toEqual(x);

            function y(a,b,c) {return makeNumber(3);}
            expect(rt.makeFunction(y).app).toEqual(y);
          });

          it("should have a meth and full_meth field on methods", function() {
            expect(rt.makeMethod(y_curry, y).meth).toEqual(y_curry);
            expect(rt.makeMethod(y_curry, y).full_meth).toEqual(y);

            expect(rt.makeMethod(z_curry, z).meth).toEqual(z_curry);
            expect(rt.makeMethod(z_curry, z).full_meth).toEqual(z);
          });

          it("should have no brands", function() {
            expect(rt.makeNumber(1)).toHaveNoBrands();
            expect(rt.makeString("hello")).toHaveNoBrands();
            expect(rt.makeBoolean(true)).toHaveNoBrands();
            expect(rt.makeFunction(y)).toHaveNoBrands();
            expect(rt.makeMethod(y_curry, y)).toHaveNoBrands();
            expect(rt.makeNothing()).toHaveNoBrands();
            expect(rt.makeObject({})).toHaveNoBrands();
          });
      });
      
      describe("Booleans", function() {
          it("should be singletons", function() {
        expect(rt.makeBoolean(true)).toBeIdentical(rt.makeBoolean(true));    
        expect(rt.makeBoolean(false)).toBeIdentical(rt.makeBoolean(false));    
          });
      });

      describe("Functions and Methods", function() {
          it("should have correct arity fields", function() {
        expect(rt.makeFunction(x).arity).toEqual(0);
        expect(rt.makeFunction(y).arity).toEqual(1);
        expect(rt.makeFunction(z).arity).toEqual(2);
        expect(rt.makeFunction(w).arity).toEqual(3);

        expect(rt.makeMethod(y_curry, y).arity).toEqual(1);
        expect(rt.makeMethod(z_curry, z).arity).toEqual(2);
        expect(rt.makeMethod(w_curry, w).arity).toEqual(3);
          });
      });

      describe("is* tests", function(){
          it("isBase works", function(){
        expect(rt.isBase(aNum)).toEqual(    true) 
        expect(rt.isBase(aBool)).toEqual(   true) 
        expect(rt.isBase(aStr)).toEqual(    true) 
        expect(rt.isBase(aFun)).toEqual(    true) 
        expect(rt.isBase(aMeth)).toEqual(   true) 
        expect(rt.isBase(aNoth)).toEqual(   true) 
        expect(rt.isBase(anObj)).toEqual(   true) 
          });

          it("isNumber works", function(){
        expect(rt.isNumber(aNum)).toEqual(    true) 
        expect(rt.isNumber(aBool)).toEqual(   false) 
        expect(rt.isNumber(aStr)).toEqual(    false) 
        expect(rt.isNumber(aFun)).toEqual(    false) 
        expect(rt.isNumber(aMeth)).toEqual(   false) 
        expect(rt.isNumber(aNoth)).toEqual(   false) 
        expect(rt.isNumber(anObj)).toEqual(   false) 
          });

          it("isBoolean works", function(){
        expect(rt.isBoolean(aNum)).toEqual(    false) 
        expect(rt.isBoolean(aBool)).toEqual(   true) 
        expect(rt.isBoolean(aStr)).toEqual(    false) 
        expect(rt.isBoolean(aFun)).toEqual(    false) 
        expect(rt.isBoolean(aMeth)).toEqual(   false) 
        expect(rt.isBoolean(aNoth)).toEqual(   false) 
        expect(rt.isBoolean(anObj)).toEqual(   false) 
          });

          it("isString works", function(){
        expect(rt.isString(aNum)).toEqual(    false) 
        expect(rt.isString(aBool)).toEqual(   false) 
        expect(rt.isString(aStr)).toEqual(    true) 
        expect(rt.isString(aFun)).toEqual(    false) 
        expect(rt.isString(aMeth)).toEqual(   false) 
        expect(rt.isString(aNoth)).toEqual(   false) 
        expect(rt.isString(anObj)).toEqual(   false) 
          });

          it("isFunction works", function(){
        expect(rt.isFunction(aNum)).toEqual(    false) 
        expect(rt.isFunction(aBool)).toEqual(   false) 
        expect(rt.isFunction(aStr)).toEqual(    false) 
        expect(rt.isFunction(aFun)).toEqual(    true) 
        expect(rt.isFunction(aMeth)).toEqual(   false) 
        expect(rt.isFunction(aNoth)).toEqual(   false) 
        expect(rt.isFunction(anObj)).toEqual(   false) 
          });

          it("isMethod works", function(){
        expect(rt.isMethod(aNum)).toEqual(    false) 
        expect(rt.isMethod(aBool)).toEqual(   false) 
        expect(rt.isMethod(aStr)).toEqual(    false) 
        expect(rt.isMethod(aFun)).toEqual(    false) 
        expect(rt.isMethod(aMeth)).toEqual(   true) 
        expect(rt.isMethod(aNoth)).toEqual(   false) 
        expect(rt.isMethod(anObj)).toEqual(   false) 
          });

          it("isNothing works", function(){
        expect(rt.isNothing(aNum)).toEqual(    false) 
        expect(rt.isNothing(aBool)).toEqual(   false) 
        expect(rt.isNothing(aStr)).toEqual(    false) 
        expect(rt.isNothing(aFun)).toEqual(    false) 
        expect(rt.isNothing(aMeth)).toEqual(   false) 
        expect(rt.isNothing(aNoth)).toEqual(   true) 
        expect(rt.isNothing(anObj)).toEqual(   false) 
          });

          it("isObject works", function(){
        expect(rt.isObject(aNum)).toEqual(    false) 
        expect(rt.isObject(aBool)).toEqual(   false) 
        expect(rt.isObject(aStr)).toEqual(    false) 
        expect(rt.isObject(aFun)).toEqual(    false) 
        expect(rt.isObject(aMeth)).toEqual(   false) 
        expect(rt.isObject(aNoth)).toEqual(   false) 
        expect(rt.isObject(anObj)).toEqual(   true) 
          });


          it("should work on instances created by makeRuntime", function(){
        //Inheritance depends on code order, ensure that the methods inside number etc are correct class
        var plus = aNum.dict['_plus'];
        expect(plus).not.toBeUndefined();
        expect(rt.isMethod(plus)).toEqual(true);
          });
      });

      describe("Cloning", function() {
          xit("should work for numbers", function() {
        var orig = rt.makeNumber(42);
        var clone = orig.clone();

        expect(orig.n).toEqual(clone.n);
        expect(orig.dict).not.toBeIdentical(clone.dict); 
        expect(orig.brands).not.toBeIdentical(clone.brands); 
          });

          xit("should work for booleans", function() {
        var orig = rt.makeBoolean(true);
        var clone = orig.clone();

        expect(orig.b).toEqual(clone.b);
        expect(orig.dict).not.toBeIdentical(clone.dict); 
        expect(orig.brands).not.toBeIdentical(clone.brands); 
          });

          xit("should work for strings", function() {
        var orig = rt.makeString("pyret");
        var clone = orig.clone();

        expect(orig.s).toEqual(clone.s);
        expect(orig.dict).not.toBeIdentical(clone.dict); 
        expect(orig.brands).not.toBeIdentical(clone.brands); 
          });

          xit("should work for functions", function() {
        var orig = rt.makeFunction(x);
        var clone = orig.clone();

        expect(orig.app).toEqual(clone.app);
        expect(orig.arity).toEqual(clone.arity);
        expect(orig.dict).not.toBeIdentical(clone.dict); 
        expect(orig.brands).not.toBeIdentical(clone.brands); 
          });

          xit("should work for methods", function() {
        var orig = rt.makeMethod(y, y_curry);
        var clone = orig.clone();

        expect(orig.meth).toEqual(clone.meth);
        expect(orig.full_meth).toEqual(clone.full_meth);
        expect(orig.arity).toEqual(clone.arity);
        expect(orig.dict).not.toBeIdentical(clone.dict); 
        expect(orig.brands).not.toBeIdentical(clone.brands); 
          });
      });


      describe("Extending Objects", function() {
          it( "should have same fields and brands when no fields added", function() {
        var x = aNum.extendWith({});
        expect(aNum.n).toEqual(x.n);
        expect(aNum.dict).toEqual(x.dict); 
        expect(aNum.brands).toEqual(x.brands); 
          });

          it( "should add a new field", function() {
        simulateBrand(aNum, "1");
        var x = aNum.extendWith({s : aStr});

        expect(aNum.n).toEqual(x.n);
        expect(x.dict.s).toBeIdentical(aStr);


        expect(aNum.dict).not.toEqual(x.dict); 
        expect(simulateHasBrand(aNum, "1"));
        expect(simulateHasBrand(x, "1"));
          });

          it( "should add multiple new fields", function() {
        simulateBrand(aNum, "1");
        var x = aNum.extendWith({s : aStr, b : aBool, f : aFun});

        expect(aNum.n).toEqual(x.n);
        expect(x.dict.s).toBeIdentical(aStr);
        expect(x.dict.b).toBeIdentical(aBool);
        expect(x.dict.f).toBeIdentical(aFun);


        expect(aNum.dict).not.toEqual(x.dict); 
        expect(simulateHasBrand(aNum, "1"));
        expect(simulateHasBrand(x, "1"));
          });

          it( "should overwrite an existing field", function() {
        simulateBrand(aNum, "1");
        var x = aNum.extendWith({s : aStr});
        console.log("about to extend");
        var y = x.extendWith({s : rt.makeString("not-equal")});

        expect(y.n).toEqual(x.n);
        expect(x.dict.s).toBeIdentical(aStr);
        expect(y.dict.s.s).toEqual("not-equal");


        expect(aNum.dict).not.toEqual(y.dict); 
        expect(simulateHasBrand(aNum, "1")).toBe(true);
        expect(simulateHasBrand(y, "1")).toBe(false);
          });

          it( "should overwrite an existing field and add new field", function() {
        simulateBrand(aNum, "1");
        var x = aNum.extendWith({s : aStr});
        var y = x.extendWith({s : rt.makeString("not-equal"), b : aBool});

        expect(y.n).toEqual(x.n);
        expect(x.dict.s).toBeIdentical(aStr);
        expect(y.dict.s).not.toBeIdentical(aStr);
        expect(y.dict.s.s).toEqual("not-equal");
        expect(y.dict.b).toEqual(aBool);

        expect(aNum.dict).not.toEqual(y.dict); 
        expect(simulateHasBrand(aNum, "1"));
        expect(simulateHasBrand(y, "1"));
          });
      });

      describe("Sameness testing", function() {
        it("should work for simple values", function() {
          expect(rt.makeNumber(42)).toBeSameAs(rt, rt.makeNumber(42));

          expect(rt.makeNumber(2)).not.toBeSameAs(rt, rt.makeNumber(42));
          expect(rt.makeString("asdf")).toBeSameAs(rt, rt.makeString("asdf"))
          expect(rt.makeString("as")).not.toBeSameAs(rt, rt.makeString("asdf"))

          expect(rt.makeBoolean(true)).toBeSameAs(rt, rt.makeBoolean(true))
          expect(rt.makeBoolean(true)).not.toBeSameAs(rt, rt.makeBoolean(false))
          expect(rt.makeBoolean(false)).not.toBeSameAs(rt, rt.makeBoolean(true))
          expect(rt.makeBoolean(false)).toBeSameAs(rt, rt.makeBoolean(false))

          expect(rt.makeFunction(function() { })).not.toBeSameAs(rt, rt.makeFunction(function() { }));
          var f = rt.makeFunction(function() { });
          var g = rt.makeFunction(function() { });
          expect(f).toBeSameAs(rt, f);
          expect(f).not.toBeSameAs(rt, g);
        });

        it("should work for objects", function() {
          var o = rt.makeObject;
          expect(o({x: rt.makeNumber(5)})).toBeSameAs(rt, o({x: rt.makeNumber(5)}));

          var five = rt.makeNumber(5);
          expect(o({x: five})).toBeSameAs(rt, o({x: five}));

          expect(o({})).not.toBeSameAs(rt, o({x: five}));

          expect(o({})).toBeSameAs(rt, o({}));

          function mkobj() { return o({x:five}); }
          var obj = mkobj();
          expect(o({obj: obj})).toBeSameAs(rt, o({obj:obj}));
          expect(o({obj: mkobj()})).toBeSameAs(rt, o({obj: mkobj()}));

          var f = rt.makeFunction(function() { });
          expect(o({obj: f})).toBeSameAs(rt, o({obj: f}));
          expect(o({obj: f})).not.toBeSameAs(rt, o({obj: rt.makeFunction(function() { })}));
        });

        it("should work for branded objects", function() {
          var b1 = rt.namespace.get("brander").app();
          var b2 = rt.namespace.get("brander").app();
          var o1 = rt.makeObject({ x: rt.makeNumber(5) });
          var o2 = rt.getField(b1, "brand").app(o1);
          expect(o1).not.toBeSameAs(rt, o2);
          var o3 = rt.getField(b1, "brand").app(o1);
          expect(o2).not.toBeIdentical(o3);
          expect(o2).toBeSameAs(rt, o3);
          var o4 = rt.getField(b2, "brand").app(o1);
          expect(o4).not.toBeSameAs(rt, o1);
          expect(o4).not.toBeSameAs(rt, o2);
          expect(o4).not.toBeSameAs(rt, o3);

          var o5 = rt.getField(b1, "brand").app(o4);
          var o6 = rt.getField(b2, "brand").app(o2);
          expect(o5).not.toBeIdentical(o6);
          expect(o5).toBeSameAs(rt, o6);
          expect(o5).not.toBeSameAs(rt, o1);
          expect(o5).not.toBeSameAs(rt, o3);

        });
      });
    }

    return { performTest: performTest };

});
