var r = require("requirejs")

define(["./matchers", "js/js-numbers"], function (matchers, jsnums) {

  _ = require('jasmine-node');
  var path = require('path');
  var addPyretMatchers = matchers.addPyretMatchers;

  function performTest(useCompiled){

    var R = require("js/runtime-anf");

    var output;
    var rt;

    /**@ param {string} str, output*/
    function stdout(str) {
        output += str;
    }

    //Numbers for testing
    var five;
    var nfive;
    var zero;
    var one;
    var none;
    var two;
    var four;
    var twty;
    var seven;
    var six;
    var twtyseven;
    var half;
    var big;

    beforeEach(function(){
        output = "";
        rt = R.makeRuntime({'stdout' : stdout});
        addPyretMatchers(this, rt);

        //Make Some numbers
        five = rt.makeNumber(5);
        nfive = rt.makeNumber(-5);
        zero = rt.makeNumber(0);
        one = rt.makeNumber(1);
        none = rt.makeNumber(-1);
        two = rt.makeNumber(2);
        four = rt.makeNumber(4);
        six = rt.makeNumber(6);
        twty = rt.makeNumber(20);
        twtyseven = rt.makeNumber(27);
        seven = rt.makeNumber(7);
        half = rt.makeNumber(0.5);
        big = rt.makeNumber(9000000000000000);
    });

    describe("Number Dictionary", function() {
      it("should have correct _plus", function() {
          expect(five.dict).toBeUndefined();

          var plus = rt.plus;

          //Basic
          expect(plus(zero, zero)).toBigEqual(0);
          expect(plus(one, zero)).toBigEqual(1);
          expect(plus(two, two)).toBigEqual(4);
          expect(plus(two, four)).toBigEqual(6);
          expect(plus(four, two)).toBigEqual(6);

          //Negative Numbers
          expect(plus(nfive, two)).toBigEqual(-3);
          expect(plus(two, nfive)).toBigEqual(-3);

          //Fractions
          expect(plus(two, half)).toBigEqual(2.5);

          //Chaining
          expect(plus(plus(two, half), plus(two, half))).toBigEqual(5);
          expect(plus(plus(two, one), plus(twty, seven))).toBigEqual(30);

          //BIG
          expect(plus(big, big)).toBeBig(jsnums.fromString("18000000000000000"));
      });

      it("should have correct _minus", function() {
          var minus = rt.minus;

          //Basic
          expect(minus(zero, zero)).toBigEqual(0);
          expect(minus(one, zero)).toBigEqual(1);
          expect(minus(two, two)).toBigEqual(0);
          expect(minus(two, four)).toBigEqual(-2);
          expect(minus(four, two)).toBigEqual(2);

          //Negative Numbers
          expect(minus(nfive, two)).toBigEqual(-7);
          expect(minus(two, nfive)).toBigEqual(7);

          //Fractions
          expect(minus(two, half)).toBigEqual(1.5);

          //Chaining
          expect(minus(minus(two, half), minus(two, half))).toBigEqual(0);
          expect(minus(minus(two, one), minus(twty, seven))).toBigEqual(-12);
          expect(minus(minus(zero, big), big)).toBeBig(jsnums.fromString("-18000000000000000"));
      });

      it("should have correct _times", function() {
          var times = rt.times;

          //Basic
          expect(times(zero, zero)).toBigEqual(0);
          expect(times(one, zero)).toBigEqual(0);
          expect(times(two, two)).toBigEqual(4);
          expect(times(two, four)).toBigEqual(8);
          expect(times(four, two)).toBigEqual(8);

          //Negative Numbers
          expect(times(nfive, two)).toBigEqual(-10);
          expect(times(two, nfive)).toBigEqual(-10);

          //Fractions
          expect(times(two, half)).toBigEqual(1);

          //Chaining
          expect(times(times(two, half), times(two, half))).toBigEqual(1);
          expect(times(times(two, one), times(twty, seven))).toBigEqual(280);

          //BIG Numbers
          expect(times(big, big)).toBeBig(jsnums.fromString("81000000000000000000000000000000"));
          expect(times(none, times(big, big))).toBeBig(jsnums.fromString("-81000000000000000000000000000000"));
      });

      it("should have correct _divide", function() {
          var divide = rt.divide;

          //Basic
          try{
              divide(one, zero);
              throw new Error("Did not error on divide by zero");
          }
          catch(e) {
             if(!rt.isPyretException(e)) {
              throw new Error("Divide by zero did not result in pyret error, but threw other error: " + e.message)
             }
          }

          try{
              divide(zero, zero);
              throw new Error("Did not error on divide by zero");
          }
          catch(e) {
             if(!rt.isPyretException(e)) {
              throw new Error("Divide by zero did not result in pyret error, but threw other error" + e.message)
             }
          }

          expect(divide(two, two)).toBigEqual(1);
          expect(divide(two, four)).toBigEqual(.5);
          expect(divide(four, two)).toBigEqual(2);

          //Negative Numbers
          expect(divide(nfive, two)).toBigEqual(-2.5);
          expect(divide(two, nfive)).toBigEqual(-2/5);

          //Fractions
          expect(divide(two, half)).toBigEqual(4);

          //Chaining
          expect(divide(divide(two, half), divide(two, half))).toBigEqual(1);
          expect(divide(divide(two, one), divide(twty, seven))).toBigEqual(2 / (20/7));
      });

      //Comparison functions
      it("should have correct _lessthan", function() {
          var lessthan = rt.lessthan;

          //Equal
          expect(lessthan(zero, zero)).toEqual(  false);
          expect(lessthan(one , one)).toEqual(   false);
          expect(lessthan(four, four)).toEqual(  false);
          expect(lessthan(half, half)).toEqual(  false);
          expect(lessthan(nfive, nfive)).toEqual(false);

          //Less than
          expect(lessthan(zero, one)).toEqual(  true);
          expect(lessthan(one , two)).toEqual(  true);
          expect(lessthan(four, seven)).toEqual(true);
          expect(lessthan(half, seven)).toEqual(true);
          expect(lessthan(zero, half)).toEqual( true);
          expect(lessthan(nfive, zero)).toEqual(true);

          //Greater than
          expect(lessthan(one, zero)).toEqual(   false);
          expect(lessthan(one, half)).toEqual(   false);
          expect(lessthan(seven, four)).toEqual( false);
          expect(lessthan(twty, four)).toEqual(  false);
          expect(lessthan(twty, seven)).toEqual( false);
          expect(lessthan(half, zero)).toEqual(  false);
          expect(lessthan(half, nfive)).toEqual( false);
          expect(lessthan(six, nfive)).toEqual(  false);
      });

      it("should have correct _lessequal", function() {
          var lessequal = rt.lessequal;

          //Equal
          expect(lessequal(zero, zero)).toEqual(  true);
          expect(lessequal(one , one)).toEqual(   true);
          expect(lessequal(four, four)).toEqual(  true);
          expect(lessequal(half, half)).toEqual(  true);
          expect(lessequal(nfive, nfive)).toEqual(true);

          //Less than
          expect(lessequal(zero, one)).toEqual(  true);
          expect(lessequal(one , two)).toEqual(  true);
          expect(lessequal(four, seven)).toEqual(true);
          expect(lessequal(half, seven)).toEqual(true);
          expect(lessequal(zero, half)).toEqual( true);
          expect(lessequal(nfive, zero)).toEqual(true);

          //Greater than
          expect(lessequal(one, zero)).toEqual(   false);
          expect(lessequal(one, half)).toEqual(   false);
          expect(lessequal(seven, four)).toEqual( false);
          expect(lessequal(twty, four)).toEqual(  false);
          expect(lessequal(twty, seven)).toEqual( false);
          expect(lessequal(half, zero)).toEqual(  false);
          expect(lessequal(half, nfive)).toEqual( false);
          expect(lessequal(six, nfive)).toEqual(  false);
      });

      it("should have correct _greaterthan", function() {
          var greaterthan = rt.greaterthan;

          //Equal
          expect(greaterthan(zero, zero)).toEqual(  false);
          expect(greaterthan(one , one)).toEqual(   false);
          expect(greaterthan(four, four)).toEqual(  false);
          expect(greaterthan(half, half)).toEqual(  false);
          expect(greaterthan(nfive, nfive)).toEqual(false);

          //Less than
          expect(greaterthan(zero, one)).toEqual(   false);
          expect(greaterthan(one , two)).toEqual(   false);
          expect(greaterthan(four, seven)).toEqual( false);
          expect(greaterthan(half, seven)).toEqual( false);
          expect(greaterthan(zero, half)).toEqual(  false);
          expect(greaterthan(nfive, zero)).toEqual( false);

          //Greater than
          expect(greaterthan(one, zero)).toEqual(   true);
          expect(greaterthan(one, half)).toEqual(   true);
          expect(greaterthan(seven, four)).toEqual( true);
          expect(greaterthan(twty, four)).toEqual(  true);
          expect(greaterthan(twty, seven)).toEqual( true);
          expect(greaterthan(half, zero)).toEqual(  true);
          expect(greaterthan(half, nfive)).toEqual( true);
          expect(greaterthan(six, nfive)).toEqual(  true);
      });

      it("should have correct _greaterequal", function() {
          var greaterequal = rt.greaterequal;

          //Equal
          expect(greaterequal(zero, zero)).toEqual(  true);
          expect(greaterequal(one , one)).toEqual(   true);
          expect(greaterequal(four, four)).toEqual(  true);
          expect(greaterequal(half, half)).toEqual(  true);
          expect(greaterequal(nfive, nfive)).toEqual(true);

          //Less than
          expect(greaterequal(zero, one)).toEqual(   false);
          expect(greaterequal(one , two)).toEqual(   false);
          expect(greaterequal(four, seven)).toEqual( false);
          expect(greaterequal(half, seven)).toEqual( false);
          expect(greaterequal(zero, half)).toEqual(  false);
          expect(greaterequal(nfive, zero)).toEqual( false);

          //Greater than
          expect(greaterequal(one, zero)).toEqual(   true);
          expect(greaterequal(one, half)).toEqual(   true);
          expect(greaterequal(seven, four)).toEqual( true);
          expect(greaterequal(twty, four)).toEqual(  true);
          expect(greaterequal(twty, seven)).toEqual( true);
          expect(greaterequal(half, zero)).toEqual(  true);
          expect(greaterequal(half, nfive)).toEqual( true);
          expect(greaterequal(six, nfive)).toEqual(  true);
      });

      it("should have correct _equals", function() {

          var equals = rt.equal_always;

          //Equal
          expect(equals(zero, zero)).toEqual(  true);
          expect(equals(one , one)).toEqual(   true);
          expect(equals(four, four)).toEqual(  true);
          expect(equals(half, half)).toEqual(  true);
          expect(equals(nfive, nfive)).toEqual(true);

          //Less than
          expect(equals(zero, one)).toEqual(   false);
          expect(equals(one , two)).toEqual(   false);
          expect(equals(four, seven)).toEqual( false);
          expect(equals(half, seven)).toEqual( false);
          expect(equals(zero, half)).toEqual(  false);
          expect(equals(nfive, zero)).toEqual( false);

          //Greater than
          expect(equals(one, zero)).toEqual(   false);
          expect(equals(one, half)).toEqual(   false);
          expect(equals(seven, four)).toEqual( false);
          expect(equals(twty, four)).toEqual(  false);
          expect(equals(twty, seven)).toEqual( false);
          expect(equals(half, zero)).toEqual(  false);
          expect(equals(half, nfive)).toEqual( false);
          expect(equals(six, nfive)).toEqual(  false);
      });

      it("should have correct tostring", function() {
          var tostring = rt.toReprJS;

          expect(tostring(zero)).toEqual("0");
          expect(tostring(one)).toEqual("1");
          expect(tostring(two)).toEqual("2");
          expect(tostring(seven)).toEqual("7");
          expect(tostring(twty)).toEqual("20");
          expect(tostring(half)).toEqual("1/2");
      });

      });

  }

  return { performTest: performTest };
});
