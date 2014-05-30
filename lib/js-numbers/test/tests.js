
// Let's open up plt.lib.Numbers to make it easy to test.
var N = jsnums;
for (val in N) {
    if (N.hasOwnProperty(val)) {
	this[val] = N[val];
    }
}


var diffPercent = function(x, y) {
    if (typeof(x) === 'number') {
	x = fromFixnum(x);
    }
    if (typeof(y) === 'number') {
	y = fromFixnum(y);
    }
    return Math.abs(toFixnum(divide(subtract(x, y), y)));
};



var assertEqv = function(x, y) {
    value_of(eqv(x, y)).should_be_true();
};

var assertTrue = function(aVal) {
    value_of(aVal).should_be_true();
};

var assertFalse = function(aVal) {
    value_of(aVal === false).should_be_true();
};

var assertEquals = function(expected, aVal) {
    value_of(aVal).should_be(expected);
};

var assertFails = function(thunk) {
    var isFailed = false;
    try {
	thunk();
    } catch (e) {
	isFailed = true;
    }
    value_of(isFailed).should_be_true();
};


describe('rational constructions', {
    'constructions' : function() {
	value_of(isSchemeNumber(makeRational(42)))
	    .should_be_true();

	value_of(isSchemeNumber(makeRational(21, 2)))
	    .should_be_true();

	value_of(isSchemeNumber(makeRational(2, 1)))
	    .should_be_true();


	value_of(isSchemeNumber(makeRational(-17, -171)))
	    .should_be_true();

	value_of(isSchemeNumber(makeRational(17, -171)))
	    .should_be_true();
	value_of(toFixnum(makeRational(1, 5)) === 0.2)
	    .should_be_true();
    },


    'reductions' : function() {
	value_of(equals(makeRational(1, 2),

			  makeRational(5, 10)))
	    .should_be_true();
	value_of(equals(makeRational(1, 2),

			makeRational(6, 10)));

	value_of(equals(makeRational(1, 2),

			  makeRational(-1, -2)))
	    .should_be_true();
    }

});


describe('complex construction', {
    'polar' : function() {
	assertTrue(eqv(makeComplexPolar(1, 2),
		       makeComplex(makeFloat(-0.4161468365471424),
				   makeFloat(0.9092974268256817))));
    },
    'non-real inputs should raise errors' : function() { 
	// FIXME: add tests for polar construction
    }});



describe('built-in constants', {
    'pi': function() {
 	value_of(isSchemeNumber(pi)).should_be_true(); },
    'e': function() {
	value_of(isSchemeNumber(e)).should_be_true(); },
    'nan' : function() {
	value_of(isSchemeNumber(nan)).should_be_true(); },
    'negative_inf' : function() {
	value_of(isSchemeNumber(negative_inf)).should_be_true(); },
    'inf' : function() {
	value_of(isSchemeNumber(inf)).should_be_true(); },
    'negative_one' : function() {
	value_of(isSchemeNumber(negative_one)).should_be_true(); },
    'zero' : function() {
	value_of(isSchemeNumber(zero)).should_be_true(); },
    'one' : function() {
	value_of(isSchemeNumber(one)).should_be_true(); },
    'i' : function() {
	value_of(isSchemeNumber(i)).should_be_true(); },
    'negative_i' : function() {
	value_of(isSchemeNumber(negative_i)).should_be_true(); }
});


describe('fromString', {
    'fixnums': function() {
	assertEquals(43, fromString("43"));
	assertEquals(43, fromString("+43"));
	assertEquals(-43, fromString("-43"));
    },

    'bignums': function() {
	assertEquals(makeBignum("123456789012345678901234567890"),
		     fromString("123456789012345678901234567890"));
	assertEquals(makeBignum("123456789012345678901234567890"),
		     fromString("+123456789012345678901234567890"));
	assertEquals(makeBignum("-123456789012345678901234567890"),
		     fromString("-123456789012345678901234567890"));
    },

    'rationals': function() {
	assertEquals(makeRational(1, 2),
		     fromString("1/2"));
	assertEquals(makeRational(23,34),
		     fromString("+23/34"));
	assertEquals(makeRational(-1, 2),
		     fromString("-1/2"));
	assertEquals(makeRational(1234, 5678910),
		     fromString("1234/5678910"));
	assertEquals(makeRational(makeBignum("99999999999999999999"),
				  5678910),
		     fromString("99999999999999999999/5678910"));
	assertEquals(makeRational(makeBignum("-13284973298"),
				  makeBignum("239875239")),
		     fromString("-13284973298/239875239"));
    },

    'floats': function() {
	assertEquals(makeFloat(42.1), fromString("42.1"));
 	assertEquals(makeFloat(0.1), fromString(".1"));
 	assertEquals(makeFloat(0.23), fromString("0.23"));
 	assertEquals(makeFloat(0.1), fromString("+.1"));
 	assertEquals(makeFloat(-0.1), fromString("-.1"));
 	assertEquals(makeFloat(-0.123423), fromString("-.123423"));
 	assertEquals(makeFloat(123.45), fromString("123.45"));
 	assertEquals(makeFloat(4123.423), fromString("4.123423e3"));
	assertEquals(makeFloat(1000000000000000.2),
		     fromString("1000000000000000.2"));
 	assertEquals(makeFloat(10000000000000000.2),
 		     fromString("10000000000000000.2"));
    },

    'complex': function() {
	assertEquals(makeComplex(0, 1), fromString("0+i"));
	assertEquals(makeComplex(0, 1), fromString("0+1i"));
	assertEquals(makeComplex(0, makeRational(23, 45)), fromString("0+23/45i"));
	assertEquals(makeComplex(0, makeFloat(2.5)), fromString("0+2.5i"));
	assertEquals(makeComplex(0, -1), fromString("0-i"));
	assertEquals(makeComplex(0, -1), fromString("0-1i"));
	assertEquals(makeComplex(0, makeRational(-29, 42)), fromString("0-29/42i"));
	assertEquals(makeComplex(0, makeFloat(-3.7)), fromString("0-3.7i"));

	assertEquals(makeComplex(1, makeRational(-29, 42)), fromString("1-29/42i"));
	assertEquals(makeComplex(makeFloat(100.5),
				 makeRational(-29, 42)), fromString("100.5-29/42i"));
	assertEquals(makeComplex(0, makeFloat(-3.7)), fromString("0-3.7i"));
	assertEquals(makeComplex(inf, makeFloat(-3.7)), fromString("+inf.0-3.7i"));
	assertEquals(makeComplex(nan, makeFloat(-3.7)), fromString("+nan.0-3.7i"));
	assertEquals(makeComplex(negative_inf, makeFloat(-3.7)),
		     fromString("-inf.0-3.7i"));
	assertEquals(makeComplex(negative_inf, negative_inf),
		     fromString("-inf.0-inf.0i"));
	assertEquals(makeComplex(nan, nan),
		     fromString("+nan.0+nan.0i"));
	assertEquals(makeComplex(-42, -43),
		     fromString("-42-43i"));
    },

    'special constants' : function() {
	assertEquals(nan, fromString("+nan.0"));
	assertEquals(nan, fromString("-nan.0"));
	assertEquals(inf, fromString("+inf.0"));
	assertEquals(negative_inf, fromString("-inf.0"));
	assertTrue(fromString("-0.0") === negative_zero);
    },

    'malformed': function() {
	assertFalse(fromString(""));
	assertFalse(fromString("   "));
	assertFalse(fromString("sdkfjls"));
	assertFalse(fromString("1+1k"));
	assertFalse(fromString("1.2.3"));
	assertFalse(fromString("1.2/3"));
	assertFalse(fromString("--1/3"));
	assertFalse(fromString("-1/-3"));
    }});


describe('fromFixnum', {
    'fixnums': function() {
	value_of(equals(fromFixnum(42),
			makeRational(42))).should_be_true();
	value_of(equals(fromFixnum(43),
			makeRational(43))).should_be_true();
	value_of(equals(fromFixnum(42),
			makeRational(43))).should_be_false();
    },

    'bignums': function() {
	assertTrue(equals(fromString("100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
			  fromFixnum(10e100)));

	assertTrue(equals(fromString("1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
			  fromFixnum(10e200)));
    },

    'floats': function() {
	value_of(equals(fromFixnum(42.1),
			makeFloat(42.1))).should_be_true();
    }
});



describe('equals', {
    'nan': function() {
	value_of(equals(nan, nan)).should_be_false();
	value_of(equals(nan, 3)).should_be_false();
	value_of(equals(nan, makeFloat(239843))).should_be_false();
	value_of(equals(nan, makeComplex(239843, 2))).should_be_false();
    },

    '-0.0': function() {
	assertTrue(equals(negative_zero, makeFloat(0)));
	assertTrue(equals(negative_zero, 0));
	assertTrue(equals(negative_zero, negative_zero));
	assertTrue(equals(negative_zero, makeRational(0)));
	assertTrue(equals(negative_zero, makeRational(makeBignum("0"))));
	assertFalse(equals(negative_zero, 1));
    },

    'fixnum / fixnum': function() {
	value_of(equals(42, 42)).should_be_true();
	value_of(equals(42, 43)).should_be_false();
    },

    'fixnum / bignum': function() {
	assertTrue(equals(42, makeBignum("42")));
	assertTrue(equals(-42, makeBignum("-42")));
	assertTrue(equals(0, makeBignum("0")));
	assertFalse(equals(1, makeBignum("0")));
    },

    'bignum / bignum' : function() {
	assertTrue(equals(makeBignum("31415926"),
			  makeBignum("31415926")));
	assertFalse(equals(makeBignum("31415926"),
			   makeBignum("271818296")));
    },

    'bignum / rational': function() {
	assertTrue(equals(makeBignum("0"),
			  makeRational(0)));
	assertTrue(equals(makeBignum("12345"),
			  makeRational(12345)));
	assertTrue(equals(makeBignum("-12345"),
			  makeRational(-12345)));
	assertFalse(equals(makeBignum("1"),
			  makeRational(0)));
    },

    'bignum / float' : function() {
	assertTrue(equals(makeBignum("12345"),
			  makeFloat(12345.0)));
	assertTrue(equals(makeBignum("-12345"),
			  makeFloat(-12345.0)));
	assertTrue(equals(fromString("7e40"),
			  makeFloat(7e40)));
	assertTrue(equals(fromString("-7e40"),
			  makeFloat(-7e40)));
    },

    'bignum / complex' : function() {
	assertTrue(equals(makeBignum("1e2000"),
			  makeComplex(makeBignum("1e2000"),
				      0)));
	assertFalse(equals(makeBignum("1.1e2000"),
			  makeComplex(makeBignum("1e2000"),
				      0)));
 	assertTrue(equals(makeBignum("0"),
 			  makeComplex(0, 0)));
 	assertTrue(equals(makeBignum("91326"),
 			  makeComplex(makeBignum("91326"))));
 	assertFalse(equals(makeBignum("00000"),
 			   makeComplex(makeBignum("91326"))));
 	assertTrue(equals(makeBignum("90210"),
 			  makeComplex(makeFloat(90210))));
 	assertTrue(equals(makeBignum("90210"),
 			  makeComplex(makeFloat(90210), makeFloat(0))));
 	assertFalse(equals(makeBignum("90210"),
 			   makeComplex(makeFloat(90210), makeFloat(0.1))));
    },

    'fixnum / rational': function() {
	value_of(equals(0, zero)).should_be_true();
	value_of(equals(42, makeRational(84, 2))).should_be_true();
	value_of(equals(42, makeRational(84, 3))).should_be_false();
	value_of(equals(28, makeRational(84, 3))).should_be_true();
    },

    'fixnum / float ' : function() {
	value_of(equals(1024, makeFloat(1024))).should_be_true();
	value_of(equals(1024, makeFloat(1024.0001))).should_be_false();
    },

    'fixnum / complex ': function() {
	value_of(equals(31337, makeComplex(31337))).should_be_true();
	value_of(equals(31337, makeComplex(31337, 1))).should_be_false();
    },

    'rational / rational ' : function() {
	value_of(equals(makeRational(23849),
			  makeRational(23849))).should_be_true();
	value_of(equals(makeRational(23849),
			  makeRational(23489))).should_be_false();
    },

    'rational / float': function() {
	value_of(equals(makeRational(2),
			  makeFloat(2))).should_be_true();
	value_of(equals(makeRational(2),
			  makeFloat(2.1))).should_be_false();
    },

    'rational / complex': function() {
	value_of(equals(makeRational(2),
			  makeComplex(2, 0))).should_be_true();
	value_of(equals(makeRational(2),
			  makeComplex(2, 1))).should_be_false();
	value_of(equals(makeRational(2),
			  makeComplex(0, 0))).should_be_false();
    },

    'float / float': function() {
	value_of(equals(pi, pi)).should_be_true();
	value_of(equals(pi, e)).should_be_false();
    },

    'float / complex': function() {
	value_of(equals(pi, makeComplex(pi, 0))).should_be_true();
	value_of(equals(pi, makeComplex(e, 0))).should_be_false();
    },

    'complex / complex': function() {
	value_of(equals(makeComplex(17, 2),
			  makeComplex(17, 2))).should_be_true();
	value_of(equals(makeComplex(17, 2),
			  makeComplex(2, 17))).should_be_false();
	value_of(equals(makeComplex(17, 2),
			  makeComplex(17, 17))).should_be_false();
	value_of(equals(makeComplex(2, 17),
			  makeComplex(17, 17))).should_be_false();

	value_of(equals(makeComplex(makeFloat(100), 0),
			  makeComplex(makeFloat(100), 0))).should_be_true();
	value_of(equals(makeComplex(makeFloat(100), 0),
			  makeComplex(makeRational(100), 0))).should_be_true();
	value_of(equals(makeComplex(makeFloat(100.1), 0),
			  makeComplex(makeRational(100), 0))).should_be_false();
	value_of(equals(makeComplex(makeFloat(100), 0),
			  makeComplex(makeRational(100), 1))).should_be_false();
    }
});


describe('eqv', {
    'nan' : function() {
	value_of(eqv(nan,
		       makeFloat(Number.NaN))).should_be_true();
    },

    '-0.0' : function() {
	assertFalse(eqv(negative_zero, makeFloat(0)));
	assertFalse(eqv(negative_zero, 0));
	assertTrue(eqv(negative_zero, negative_zero));
	assertFalse(eqv(negative_zero, makeRational(0)));
	assertFalse(eqv(negative_zero, makeRational(makeBignum("0"))));
	assertFalse(eqv(negative_zero, 1));
    },

    'inf' : function() {
	value_of(eqv(inf, inf)).should_be_true();
	value_of(eqv(negative_inf, negative_inf)).should_be_true();

	assertFalse(eqv(inf, negative_inf));
	assertFalse(eqv(inf,
			makeBignum("1e3000")));
	assertFalse(eqv(negative_inf,
			makeBignum("-1e3000")));
    },


    'fixnum / fixnum': function() {
	value_of(eqv(42, 42)).should_be_true();
	value_of(eqv(42, 43)).should_be_false();
    },


    'fixnum / bignum': function() {
	assertTrue(eqv(0, makeBignum("0")));
	assertTrue(eqv(-1, makeBignum("-1")));
	assertTrue(eqv(42, makeBignum("42")));
	assertTrue(eqv(123456789, makeBignum("123456789")));
	assertTrue(eqv(-123456789, makeBignum("-123456789")));
	assertFalse(eqv(-123456788, makeBignum("-123456789")));
    },

    'bignum / bignum' : function() {


	assertTrue(eqv(makeBignum("0"),
		       makeBignum("0")));
	assertTrue(eqv(makeBignum("-1"),
		       makeBignum("-1")));
	assertTrue(eqv(makeBignum("7.3241e324"),
		       makeBignum("7.3241e324")));
	assertFalse(eqv(makeBignum("7.3240e324"),
		       makeBignum("7.3241e324")));
	assertTrue(eqv(makeBignum("-13241e324"),
		       makeBignum("-13241e324")));
	assertTrue(eqv(makeBignum("3"),
		       makeBignum("3")));
	assertFalse(eqv(makeBignum("12345"),
			makeBignum("12344")));
    },


    'bignum / rational': function() {
	assertTrue(eqv(makeBignum("0"),
			makeRational(0, 1)));
	assertTrue(eqv(makeBignum("0"),
			makeRational(makeBignum("0"), 1)));
	assertTrue(eqv(makeBignum("24"),
			makeRational(makeBignum("24"), 1)));
	assertTrue(eqv(makeBignum("-1"),
			makeRational(-1, 1)));
	assertFalse(eqv(makeBignum("0"),
			makeRational(-1, 1)));
	assertTrue(eqv(makeBignum("3"),
		       makeRational(makeBignum("3"))));
	assertTrue(eqv(makeBignum("27"),
		       makeRational(27)));
    },

    'bignum / float' : function() {
	assertFalse(eqv(makeBignum("27"),
			makeFloat(27.0)));
	assertFalse(eqv(makeBignum("0"),
			makeFloat(0)));
	assertFalse(eqv(makeBignum("1e100"),
			makeFloat(1e100)));
	assertFalse(eqv(makeBignum("-1e100"),
			makeFloat(-1e100)));
    },

    'bignum / complex' : function() {
	assertTrue(eqv(makeBignum("71"),
		       makeComplex(71)));
	assertTrue(eqv(makeBignum("71"),
		       makeComplex(makeBignum("71"))));
	assertTrue(eqv(makeBignum("71"),
		       makeComplex(makeRational(71))));
	assertFalse(eqv(makeBignum("71"),
			makeComplex(makeFloat(71))));

	assertFalse(eqv(makeBignum("71"),
			makeComplex(70)));
	assertFalse(eqv(makeBignum("66"),
		       makeComplex(makeBignum("42"))));
	assertFalse(eqv(makeBignum("71"),
		       makeComplex(makeRational(71, 17))));
	assertFalse(eqv(makeBignum("71"),
			makeComplex(makeFloat(71), 2)));
	assertTrue(eqv(makeBignum("5e23"),
		       makeComplex(makeBignum("5e23"),
				   0)));
	assertTrue(eqv(makeBignum("5e23"),
		       makeComplex(makeRational(makeBignum("5e23")),
				   0)));
	assertFalse(eqv(makeBignum("5e23"),
			makeComplex(makeFloat(5e23), 0)));
	assertFalse(eqv(makeBignum("5e23"),
		       makeComplex(makeBignum("5e23"),
				   1)));
    },

    'fixnum / rational': function() {
	value_of(eqv(42, makeRational(84, 2))).should_be_true();
	value_of(eqv(42, makeRational(84, 3))).should_be_false();
	value_of(eqv(28, makeRational(84, 3))).should_be_true();
    },

    'fixnum / float ' : function() {
	value_of(eqv(makeRational(1024), makeFloat(1024))).should_be_false();
	value_of(eqv(makeRational(1024), makeFloat(1024.0001))).should_be_false();
    },

    'fixnum / complex' : function() {
	value_of(eqv(10, makeComplex(10))).should_be_true();
	value_of(eqv(10, makeComplex(0))).should_be_false();
    },

    'rational / rational': function() {
	value_of(eqv(makeRational(2, 3),
		       makeRational(2, 3))).should_be_true();
	value_of(eqv(makeRational(3, 2),
		       makeRational(2, 3))).should_be_false();
    },

    'rational / float': function() {
	value_of(eqv(makeRational(2),
		       makeFloat(2))).should_be_false();
	value_of(eqv(makeRational(2),
		       makeFloat(2.1))).should_be_false();
    },

    'rational / complex': function() {
	value_of(eqv(makeRational(2),
		       makeComplex(2, 0))).should_be_true();
	value_of(eqv(makeRational(2),
		       makeComplex(2, 1))).should_be_false();
	value_of(eqv(makeRational(2),
		       makeComplex(0, 0))).should_be_false();
    },

    'float / float': function() {
	value_of(eqv(pi, pi)).should_be_true();
	value_of(eqv(e, e)).should_be_true();
	value_of(eqv(pi, e)).should_be_false();
    },

    'float / complex': function() {
	value_of(eqv(pi, makeComplex(pi))).should_be_true();
	value_of(eqv(3, makeComplex(makeFloat(3)))).should_be_false();
	value_of(eqv(pi, makeComplex(pi, 1))).should_be_false();
    },

    'complex / complex': function() {
	value_of(eqv(makeComplex(17, 2),
		       makeComplex(17, 2))).should_be_true();
	value_of(eqv(makeComplex(17, 2),
		       makeComplex(2, 17))).should_be_false();
	value_of(eqv(makeComplex(17, 2),
		       makeComplex(17, 17))).should_be_false();
	value_of(eqv(makeComplex(2, 17),
		       makeComplex(17, 17))).should_be_false();

 	value_of(eqv(makeComplex(makeFloat(100), 0),
 		       makeComplex(makeFloat(100), 0))).should_be_true();
 	value_of(eqv(makeComplex(makeFloat(100), 0),
 		       makeComplex(makeRational(100), 0))).should_be_false();
 	value_of(eqv(makeComplex(makeFloat(100.1), 0),
 		       makeComplex(makeRational(100), 0))).should_be_false();
 	value_of(eqv(makeComplex(makeFloat(100), 0),
 		       makeComplex(makeRational(100), 1))).should_be_false();
    },

    'tricky case with complex': function() {
	// If any component of a complex is inexact, both
	// the real and imaginary parts get turned into
	// inexact quantities.
	value_of(eqv(makeComplex(0, 
				 makeFloat(1.1)),
		     makeComplex(makeFloat(0.0),
				 makeFloat(1.1)))).should_be_true();
    }
});


describe('isSchemeNumber', {
    'strings': function() {
	value_of(isSchemeNumber("42")).should_be_false();
	value_of(isSchemeNumber(42)).should_be_true();
	assertTrue(isSchemeNumber(makeBignum("298747328418794387941798324789421978")));
	value_of(isSchemeNumber(makeRational(42, 42))).should_be_true();
	value_of(isSchemeNumber(makeFloat(42.2))).should_be_true();
	value_of(isSchemeNumber(makeComplex(17))).should_be_true();
	value_of(isSchemeNumber(makeComplex(17, 1))).should_be_true();
	value_of(isSchemeNumber(makeComplex(makeFloat(17), 1))).should_be_true();
	value_of(isSchemeNumber(undefined)).should_be_false();
	value_of(isSchemeNumber(null)).should_be_false();
	value_of(isSchemeNumber(false)).should_be_false();
    }
});



describe('isRational', {
    'fixnums': function() {
	assertTrue(isRational(0));
	assertTrue(isRational(1));
	assertTrue(isRational(238977428));
	assertTrue(isRational(-2371));
    },

    'bignums': function() {
	assertTrue(isRational(makeBignum("324987329848724791")));
	assertTrue(isRational(makeBignum("0")));
	assertTrue(isRational(makeBignum("-1239847210")));
    },

    'rationals': function() {
	assertTrue(isRational(makeRational(0, 1)));
	assertTrue(isRational(makeRational(1, 100)));
	assertTrue(isRational(makeRational(9999, 10000)));
	assertTrue(isRational(makeRational(1, 4232)));
    },

    'floats': function() {
 	assertTrue(isRational(makeFloat(1.0)));
 	assertTrue(isRational(makeFloat(25.0)));
 	assertTrue(isRational(e));
	assertTrue(isRational(pi));
	assertFalse(isRational(inf));
	assertFalse(isRational(negative_inf));
	assertFalse(isRational(nan));
    },

    'complex': function() {
	assertTrue(isRational(makeComplex(0, 0)));
	assertTrue(isRational(makeComplex(e, 0)));
	assertTrue(isRational(makeComplex(pi, 0)));
	assertFalse(isRational(makeComplex(nan, 0)));
	assertFalse(isRational(makeComplex(0, 1)));
	assertFalse(isRational(makeComplex(0, negative_inf)));
	assertFalse(isRational(makeComplex(makeFloat(0), makeFloat(0))));
    },

    'others': function() {
	assertFalse(isRational("0"));
	assertFalse(isRational("hello"));
	assertFalse(isRational({}));
	assertFalse(isRational([]));
	assertFalse(isRational(false));
    },
});


describe('isReal', {
    'fixnums': function() {
	assertTrue(isReal(237489));
	assertTrue(isReal(0));
	assertTrue(isReal(-12345));
    },

    'bignums': function() {
	assertTrue(isReal(makeBignum("0")));
	assertTrue(isReal(makeBignum("1")));
	assertTrue(isReal(makeBignum("-1")));
	assertTrue(isReal(makeBignum("23497842398287924789232439723")));
	assertTrue(isReal(makeBignum("1e1000")));
	assertTrue(isReal(makeBignum("-1e1000")));
	assertTrue(isReal(makeBignum("1e23784")));
	assertTrue(isReal(makeBignum("-7.241e23784")));
    },

    'rationals': function() {
	assertTrue(isReal(makeRational(0, 1)));
	assertTrue(isReal(makeRational(0, 12342)));
	assertTrue(isReal(makeRational(-2324, 12342)));
	assertTrue(isReal(makeRational(1, 2)));
    },

    'floats': function() {
 	assertTrue(isReal(makeFloat(1.0)));
 	assertTrue(isReal(makeFloat(25.0)));
 	assertTrue(isReal(e));
	assertTrue(isReal(pi));
	assertTrue(isReal(inf));
	assertTrue(isReal(negative_inf));
	assertTrue(isReal(nan));
    },

    'complex': function() {
	assertTrue(isReal(makeComplex(0, 0)));
	assertTrue(isReal(makeComplex(e, 0)));
	assertTrue(isReal(makeComplex(pi, 0)));
	assertTrue(isReal(makeComplex(nan, 0)));
	assertTrue(isReal(makeComplex(inf, 0)));
	assertTrue(isReal(makeComplex(negative_inf, 0)));
	assertFalse(isReal(makeComplex(0, 1)));
	assertFalse(isReal(makeComplex(0, negative_inf)));
	assertFalse(isReal(makeComplex(pi, inf)));
	assertFalse(isReal(makeComplex(234, nan)));
	assertFalse(isReal(makeComplex(makeFloat(3),
				       makeFloat(0))));
    },

    'others': function() {
	assertFalse(isReal("0"));
	assertFalse(isReal("hello"));
	assertFalse(isReal([]));
	assertFalse(isReal({}));
	assertFalse(isReal(false));
    }
});


describe('isExact', {
    'fixnums': function() {
	assertTrue(isExact(19));
	assertTrue(isExact(0));
	assertTrue(isExact(-1));
	assertTrue(isExact(1));
    },

    'bignums': function() {
	assertTrue(isExact(makeBignum("0")));
	assertTrue(isExact(makeBignum("1")));
	assertTrue(isExact(makeBignum("-1")));
	assertTrue(isExact(makeBignum("23497842398287924789232439723")));
	assertTrue(isExact(makeBignum("1e1000")));
	assertTrue(isExact(makeBignum("-1e1000")));
	assertTrue(isExact(makeBignum("12342357892297851728921374891327893")));
	assertTrue(isExact(makeBignum("4.1321e200")));
	assertTrue(isExact(makeBignum("-4.1321e200")));
    },

    'rationals': function() {
	assertTrue(isExact(makeRational(19)));
	assertTrue(isExact(makeRational(0)));
	assertTrue(isExact(makeRational(-1)));
	assertTrue(isExact(makeRational(1)));
	assertTrue(isExact(makeRational(1, 2)));
	assertTrue(isExact(makeRational(1, 29291)));
    },

    'floats': function() {
	assertFalse(isExact(e));
	assertFalse(isExact(pi));
	assertFalse(isExact(inf));
	assertFalse(isExact(negative_inf));
	assertFalse(isExact(nan));
	assertFalse(isExact(makeFloat(0)));
	assertFalse(isExact(makeFloat(1111.1)));
    },

    'complex': function() {
	assertTrue(isExact(makeComplex(0, 0)));
	assertTrue(isExact(makeComplex(makeRational(1,2),
					   makeRational(1, 17))));
	assertFalse(isExact(makeComplex(e,
					    makeRational(1, 17))));
	assertFalse(isExact(makeComplex(makeRational(1,2),
					    pi)));
	assertFalse(isExact(makeComplex(makeRational(1,2),
					    nan)));

	assertFalse(isExact(makeComplex(negative_inf,
					    nan)));
    }
});






describe('isInexact', {
    'fixnums': function() {
	assertFalse(isInexact(19));
	assertFalse(isInexact(0));
	assertFalse(isInexact(-1));
	assertFalse(isInexact(1));
    },

    'bignums': function() {
	assertFalse(isInexact(makeBignum("0")));
	assertFalse(isInexact(makeBignum("1")));
	assertFalse(isInexact(makeBignum("-1")));
	assertFalse(isInexact(makeBignum("23497842398287924789232439723")));
	assertFalse(isInexact(makeBignum("1e1000")));
	assertFalse(isInexact(makeBignum("-1e1000")));
	assertFalse(isInexact(makeBignum("12342357892297851728921374891327893")));
	assertFalse(isInexact(makeBignum("4.1321e200")));
	assertFalse(isInexact(makeBignum("-4.1321e200")));
    },

    'rationals': function() {
	assertFalse(isInexact(makeRational(19)));
	assertFalse(isInexact(makeRational(0)));
	assertFalse(isInexact(makeRational(-1)));
	assertFalse(isInexact(makeRational(1)));
	assertFalse(isInexact(makeRational(1, 2)));
	assertFalse(isInexact(makeRational(1, 29291)));
    },

    'floats': function() {
	assertTrue(isInexact(e));
	assertTrue(isInexact(pi));
	assertTrue(isInexact(inf));
	assertTrue(isInexact(negative_inf));
	assertTrue(isInexact(nan));
	assertTrue(isInexact(makeFloat(0)));
	assertTrue(isInexact(makeFloat(1111.1)));
    },

    'complex': function() {
	assertFalse(isInexact(makeComplex(0, 0)));
	assertFalse(isInexact(makeComplex(makeRational(1,2),
					  makeRational(1, 17))));
	assertTrue(isInexact(makeComplex(e,
					 makeRational(1, 17))));
	assertTrue(isInexact(makeComplex(makeRational(1,2),
					 pi)));
	assertTrue(isInexact(makeComplex(makeRational(1,2),
					 nan)));
	
	assertTrue(isInexact(makeComplex(negative_inf,
					 nan)));
    }
});








describe('isInteger', {
    'fixnums': function() {
	assertTrue(isInteger(1));
	assertTrue(isInteger(-1));
    },

    'bignums': function() {
	assertTrue(isInteger(makeBignum("2983473189472187414789132743928148151617364")));
	assertTrue(isInteger(makeBignum("-99999999999999999999999999999999999999")));
    },

    'rationals': function() {
	assertTrue(isInteger(makeRational(1, 1)));
	assertFalse(isInteger(makeRational(1, 2)));
	assertFalse(isInteger(makeRational(9999, 10000)));
	assertFalse(isInteger(makeRational(9999, 1000)));
    },

    'floats': function() {
	assertFalse(isInteger(makeFloat(2.3)));
	assertTrue(isInteger(makeFloat(4.0)));
	assertFalse(isInteger(inf));
	assertFalse(isInteger(negative_inf));
	assertFalse(isInteger(nan));
    },

    'complex': function() {
	assertTrue(isInteger(makeComplex(42, 0)));
	assertFalse(isInteger(makeComplex(makeFloat(42), makeFloat(0))));
	assertFalse(isInteger(makeComplex(42, 42)));
	assertFalse(isInteger(i));
	assertFalse(isInteger(negative_i));
    },

    'others': function() {
	assertFalse(isInteger("hello"));
	assertFalse(isInteger("0"));
    }
});


describe('toFixnum', {
    'fixnums': function() {
	assertEquals(42, toFixnum(42));
	assertEquals(-20, toFixnum(-20));
	assertEquals(0, toFixnum(0));
    },

    'bignums': function() {
	assertEquals(123456789, toFixnum(makeBignum("123456789")));
	assertEquals(0, toFixnum(makeBignum("0")));
	assertEquals(-123, toFixnum(makeBignum("-123")));
	assertEquals(123456, toFixnum(makeBignum("123456")));
	// We're dealing with big numbers, where the numerical error
	// makes it difficult to compare for equality.  We just go for
	// percentage and see that it is ok.
	assertTrue(diffPercent(1e200, toFixnum(makeBignum("1e200")))
		   < 1e-10);
	assertTrue(diffPercent(-1e200, toFixnum(makeBignum("-1e200")))
		   < 1e-10);
    },

    'rationals': function() {
	assertEquals(0, toFixnum(zero));
	assertEquals(17/2, toFixnum(makeRational(17, 2)));
	assertEquals(1926/3, toFixnum(makeRational(1926, 3)));
	assertEquals(-11150/17, toFixnum(makeRational(-11150, 17)));
    },

    'floats': function() {
	assertEquals(12345.6789, toFixnum(makeFloat(12345.6789)));
	assertEquals(Math.PI, toFixnum(pi));
	assertEquals(Math.E, toFixnum(e));
	assertEquals(Number.POSITIVE_INFINITY, toFixnum(inf));
	assertEquals(Number.NEGATIVE_INFINITY, toFixnum(negative_inf));
	assertTrue(isNaN(toFixnum(nan)));
    },

    'complex': function() {
	assertFails(function() { toFixnum(makeComplex(2, 1)); });
	assertFails(function() { toFixnum(i); });
	assertFails(function() { toFixnum(negative_i); });
	assertEquals(2, toFixnum(makeComplex(2, 0)));
	assertEquals(1/2, toFixnum(makeComplex(makeRational(1, 2),
						   0)));
	assertEquals(Number.POSITIVE_INFINITY,
		     toFixnum(makeComplex(inf, 0)));

	assertEquals(Number.NEGATIVE_INFINITY,
		     toFixnum(makeComplex(negative_inf, 0)));
	assertTrue(isNaN(toFixnum(makeComplex(nan, 0))));

    }
});



describe('toExact', {
    'fixnums': function() {
	assertEquals(1792, toExact(1792));
	assertEquals(0, toExact(0));
	assertEquals(-1, toExact(-1));
    },

    'bignums': function() {
	assertEquals(makeBignum("4.2e100"),
		     toExact(makeBignum("4.2e100")));
	assertEquals(makeBignum("0"),
		     toExact(makeBignum("0")));
	assertEquals(makeBignum("1"),
		     toExact(makeBignum("1")));
	assertEquals(makeBignum("-1"),
		     toExact(makeBignum("-1")));
	assertEquals(makeBignum("-12345"),
		     toExact(makeBignum("-12345")));
	assertEquals(makeBignum("-1.723e500"),
		     toExact(makeBignum("-1.723e500")));
    },

    'rationals': function() {
	assertEquals(makeRational(1, 2), toExact(makeRational(1, 2)));
	assertEquals(makeRational(1, 9999), toExact(makeRational(1, 9999)));
	assertEquals(makeRational(0, 1), toExact(makeRational(0, 9999)));
	assertEquals(makeRational(-290, 1), toExact(makeRational(-290, 1)));
    },

    'floats': function() {
	assertEquals(makeRational(1, 2), toExact(makeFloat(0.5)));
	assertEquals(makeRational(1, 10), toExact(makeFloat(0.1)));
	assertEquals(makeRational(9, 10), toExact(makeFloat(0.9)));
	assertTrue(isExact(toExact(makeFloat(10234.7))));
	assertTrue(diffPercent(makeRational(102347, 10),
			       toExact(makeFloat(10234.7))) < 1);
	assertEquals(-1, toExact(makeFloat(-1)));
	assertEquals(0, toExact(makeFloat(0)));
	assertEquals(1024, toExact(makeFloat(1024)));
	assertFails(function() { toExact(nan); });
	assertFails(function() { toExact(inf); });
	assertFails(function() { toExact(negative_inf); });
    },

    'complex': function() {
	assertEquals(0, toExact(makeComplex(0, 0)));
	assertEquals(99, toExact(makeComplex(99, 0)));
	assertEquals(makeRational(-1, 2),
		     toExact(makeComplex(makeRational(-1, 2), 0)));
 	assertEquals(makeRational(1, 4),
 		     toExact(makeComplex(makeFloat(.25), 0)));
 	assertFails(function() { toExact(makeComplex(nan, 0)); });
 	assertFails(function() { toExact(makeComplex(inf, 0)); });
 	assertFails(function() { toExact(makeComplex(negative_inf, 0)); });
 	assertTrue(eqv(toExact(makeComplex(0, 1)), makeComplex(0, 1)));
 	assertFails(function() { toExact(makeComplex(0, nan)); });
    }
});



describe('toInexact', {
    'fixnum' : function() {
	assertTrue(eqv(toInexact(5), makeFloat(5)));
	assertTrue(eqv(toInexact(0), makeFloat(0)));
	assertTrue(eqv(toInexact(-167), makeFloat(-167)));
    },

    'bignum': function() {
	assertTrue(eqv(toInexact(makeBignum('5')), makeFloat(5)));
	assertTrue(eqv(toInexact(makeBignum('0')), makeFloat(0)));
	assertTrue(eqv(toInexact(makeBignum('-167')), makeFloat(-167)));
	assertTrue(eqv(toInexact(expt(2, 10000)),
		       inf));
	assertTrue(eqv(toInexact(subtract(0, expt(2, 10000))),
		       negative_inf));
    },

    'rational': function() {
	assertTrue(eqv(toInexact(makeRational(1, 2)),
		       makeFloat(0.5)));
	assertTrue(eqv(toInexact(makeRational(12362534, 237)),
		       makeFloat(52162.59071729958)));
    },

    'float': function() {
	assertTrue(eqv(toInexact(makeFloat(0)),
		       toInexact(makeFloat(0))));

	assertTrue(eqv(toInexact(makeFloat(123.4)),
		       toInexact(makeFloat(123.4))));
	assertTrue(eqv(toInexact(makeFloat(-42)),
		       toInexact(makeFloat(-42))));
	assertTrue(eqv(toInexact(inf),
		       inf));
	assertTrue(eqv(toInexact(negative_inf),
		       negative_inf));
	assertTrue(eqv(toInexact(negative_zero),
		       negative_zero));
    },


    'complex': function() {
	assertTrue(eqv(toInexact(makeComplex(1, 2)),
		       makeComplex(makeFloat(1), makeFloat(2))));

	assertTrue(eqv(toInexact(makeComplex(makeRational(1, 2), 
					     2)),
		       makeComplex(makeFloat(0.5), makeFloat(2))));
    }});





describe('add', {
    'fixnum / fixnum' : function() {
	assertEquals(0, add(0, 0));
	assertEquals(1025, add(1024, 1));
	assertEquals(-84, add(-42, -42));
	assertEquals(982, add(1024, -42));
    },

    'finum overflows to bignum' : function() {
	var aNumber = 0;
	for (var i = 0 ; i < 1000; i++) {
	    aNumber = add(aNumber, fromFixnum(1e20));
	}
	assertTrue(eqv(makeBignum("1e23"), aNumber));
    },

    'fixnum / bignum': function() {
	assertTrue(eqv(2, add(1, makeBignum("1"))));
	assertTrue(eqv(makeBignum("1234298352389543294732947983"),
		       add(1, makeBignum("1234298352389543294732947982"))));
	assertFalse(eqv(makeBignum("1234298352389543294732947982"),
			add(1, makeBignum("1234298352389543294732947982"))));
	assertTrue(eqv(makeBignum("1234298352389543294732947982"),
		       add(0, makeBignum("1234298352389543294732947982"))));
	assertTrue(eqv(makeBignum("1234298352389543294732947981"),
		       add(-1, makeBignum("1234298352389543294732947982"))));
	assertTrue(eqv(makeBignum("999999999999999999999999999999"),
		       add(-1, makeBignum("1000000000000000000000000000000"))));
    },

    'bignum / bignum' : function() {
	assertTrue(eqv(makeBignum("1999999999999999999999999999999"),
		       add(makeBignum("999999999999999999999999999999"),
			   makeBignum("1000000000000000000000000000000"))));
	assertTrue(eqv(1,
		       add(makeBignum("-999999999999999999999999999999"),
			   makeBignum("1000000000000000000000000000000"))));
	assertTrue(eqv(makeBignum("-1999999999999999999999999999999"),
		       add(makeBignum("-999999999999999999999999999999"),
			   makeBignum("-1000000000000000000000000000000"))));
	assertTrue(eqv(makeBignum("-1"),
		       add(makeBignum("999999999999999999999999999999"),
			   makeBignum("-1000000000000000000000000000000"))));
	assertFalse(eqv(makeBignum("-20000000000000000000000000000"),
		       add(makeBignum("-999999999999999999999999999999"),
			   makeBignum("-1000000000000000000000000000000"))));
    },

    'bignum / rational': function() {
 	assertFalse(eqv(makeBignum("1234"),
 		       add(makeBignum("1234"),
 			   makeRational(2))));
 	assertTrue(eqv(makeBignum("1236"),
 		       add(makeBignum("1234"),
 			   makeRational(2))));
 	assertTrue(eqv(makeBignum("1e500"),
 		       add(makeBignum("1e500"),
 			   makeRational(0))));
 	assertTrue(eqv(makeRational(add(makeBignum("1e500"), 1),
 				    makeBignum("1e500")),
 		       add(1, makeRational(1, makeBignum("1e500")))));

	assertTrue(eqv(fromString("461489806479620935470974478730/23987523567"),
		       add(makeBignum("19238743223768948327"),
			   fromString("1732914256756321/23987523567"))));

	assertTrue(eqv(fromString("-77100525133482588244247/239875239"),
		       add(fromString("-321419273847891"),
			   fromString("-13284973298/239875239"))));
    },


    'bignum / float' : function() {
	assertTrue(diffPercent(add(makeBignum("42"),
				   makeFloat(17.5)),
			       makeFloat(59.5))
		   < 2e-10);
	assertTrue(diffPercent(add(makeBignum("-42"),
				   makeFloat(17.5)),
			       makeFloat(-24.5))
		   < 2e-10);

	assertTrue(eqv(nan,
		       add(makeBignum("0"), nan)));
	assertTrue(eqv(nan,
		       add(makeBignum("10e500"), nan)));
	assertTrue(eqv(nan,
		       add(makeBignum("-10e500"), nan)));

	assertTrue(eqv(inf,
		       add(makeBignum("0"), inf)));
	assertTrue(eqv(negative_inf,
		       add(makeBignum("0"), negative_inf)));
    },


    'huge bignum and infinity': function() {
	// NOTE: this case is tricky, because 1e1000 will be naively coersed
	// to inf by toFixnum.  We need to somehow distinguished coersed
	// values that are too large to represent with fixnums, but are yet
	// finite, so that addition with infinite quantities does the right
	// thing, at least with respect to adding bignums to inexact floats.
	assertTrue(eqv(negative_inf,
		       add(makeBignum("1e1000"), negative_inf)));
	assertTrue(eqv(inf,
		       add(makeBignum("-1e1000"), inf)));

	assertTrue(eqv(inf, add(makeBignum("2e1000"), inf)));
	assertTrue(eqv(inf, add(makeBignum("-2e1000"), inf)));
	assertTrue(eqv(negative_inf, add(makeBignum("2e1000"), negative_inf)));
	assertTrue(eqv(negative_inf, add(makeBignum("-2e1000"), negative_inf)));
    },

    'bignum / complex' : function() {
	assertTrue(eqv(add(makeBignum("12345"),
			   makeComplex(1, 1)),
		       makeComplex(makeBignum("12346"),
				   1)));

	assertTrue(eqv(add(makeBignum("10e500"),
			   makeComplex(makeBignum("10e500"),
				       makeBignum("124529478"))),
		       makeComplex(makeBignum("20e500"),
				   makeBignum("124529478"))));
    },


    'fixnum / rational' : function() {
	assertEquals(0, add(0, makeRational(0)));
	assertEquals(12347, add(12345, makeRational(2)));
	assertEquals(makeRational(33, 2), add(16, makeRational(1, 2)));
	assertEquals(makeRational(-1, 2), add(0, makeRational(-1, 2)));
	assertEquals(makeRational(-1, 7), add(0, makeRational(-1, 7)));
	assertEquals(makeRational(6, 7), add(1, makeRational(-1, 7)));
    },

    'fixnum / floating' : function() {
	assertTrue(equals(0, add(0, makeFloat(0))));
	assertEquals(makeFloat(1.5), add(1, makeFloat(.5)));
	assertEquals(makeFloat(1233.5), add(1234, makeFloat(-.5)));
	assertEquals(makeFloat(-1233.5), add(-1234, makeFloat(.5)));
	assertEquals(inf, add(1234, inf));
	assertEquals(negative_inf, add(1234, negative_inf));
	assertEquals(nan, add(1234, nan));
    },
    'fixnum / complex' : function() {
	assertTrue(equals(0, add(0, makeComplex(0, 0))));
	assertTrue(equals(1040, add(16, makeComplex(1024, 0))));
	assertEquals(makeComplex(1040, 17), add(16, makeComplex(1024, 17)));
	assertEquals(makeComplex(1040, -17), add(16, makeComplex(1024, -17)));
	assertEquals(makeComplex(1040, pi), add(16, makeComplex(1024, pi)));
    },

    'rational / rational' : function() {
	assertEquals(1, add(makeRational(1, 2),
			    makeRational(1, 2)));
	assertEquals(0, add(makeRational(1, 2),
			    makeRational(-1, 2)));
	assertEquals(makeRational(155, 21),
		     add(makeRational(17, 3), makeRational(12, 7)));
	assertEquals(makeRational(-1199068363, 9758),
		     add(makeRational(-29384289, 238), makeRational(23897, 41)));
	assertEquals(makeRational(-1, 99990000),
		     add(makeRational(1, 10000), makeRational(-1, 9999)));
    },

    'rational / floating' : function() {
	assertEquals(makeFloat(0.2), add(makeRational(0), makeFloat(0.2)));
	assertEquals(makeFloat(0.8), add(makeRational(1), makeFloat(-0.2)));
	assertEquals(makeFloat(0.8), add(makeRational(1), makeFloat(-0.2)));
	assertEquals(makeFloat(1.1), add(makeRational(1, 2), makeFloat(0.6)));
	assertEquals(inf, add(makeRational(1, 2), inf));
	assertEquals(negative_inf, add(makeRational(1, 2), negative_inf));
	assertEquals(nan, add(makeRational(1, 2), nan));
    },

    'rational / complex' : function() {
	assertEquals(makeComplex(makeRational(-324, 23), 1),
		     add(makeRational(-324, 23), makeComplex(0, 1)));
	assertEquals(makeComplex(0, -234),
		     add(makeRational(-324, 23),
			 makeComplex(makeRational(324, 23),
				     -234)));
    },

    'floating / floating' : function() {
	assertEquals(makeFloat(12345.678),
		     add(makeFloat(12345), makeFloat(.678)));
	assertEquals(makeFloat(-12344.322),
		     add(makeFloat(-12345), makeFloat(.678)));
	assertEquals(makeFloat(Math.PI + Math.E),
		     add(pi, e));
	assertEquals(inf, add(inf, inf));
	assertEquals(nan, add(inf, negative_inf));
	assertEquals(nan, add(inf, nan));
	assertEquals(nan, add(negative_inf, inf));
	assertEquals(negative_inf, add(negative_inf, negative_inf));
	assertEquals(nan, add(negative_inf, nan));
	assertEquals(nan, add(nan, inf));
	assertEquals(nan, add(nan, negative_inf));
	assertEquals(nan, add(nan, nan));
    },

    'floating / complex' : function() {
	assertEquals(makeComplex(inf, 1), add(inf, makeComplex(inf, 1)));
	assertEquals(makeComplex(nan, 2), add(inf, makeComplex(negative_inf, 2)));
	assertEquals(makeComplex(nan, 3), add(inf, makeComplex(nan, 3)));
	assertEquals(makeComplex(nan, 4), add(negative_inf, makeComplex(inf, 4)));
	assertEquals(makeComplex(negative_inf, 5),
		     add(negative_inf, makeComplex(negative_inf, 5)));
	assertEquals(makeComplex(nan, 6), add(negative_inf, makeComplex(nan, 6)));
	assertEquals(makeComplex(nan, 7), add(nan, makeComplex(inf, 7)));
	assertEquals(makeComplex(nan, 8), add(nan, makeComplex(negative_inf, 8)));
	assertEquals(makeComplex(nan, 9), add(nan, makeComplex(nan, 9)));
    },

    'complex / complex' : function() {
	assertEquals(makeComplex(4, 6), add(makeComplex(1, 2),
					    makeComplex(3, 4)));
	assertEquals(makeComplex(2, 6), add(makeComplex(-1, 2),
					    makeComplex(3, 4)));
	assertEquals(makeComplex(4, -2), add(makeComplex(1, 2),
					     makeComplex(3, -4)));
	assertEquals(makeComplex(pi, e), add(makeComplex(pi, 0),
					     makeComplex(0, e)));
	assertEquals(makeComplex(add(pi, makeRational(-1,4)),
				 add(makeRational(1, 2), e)),
		     add(makeComplex(pi, makeRational(1, 2)),
			 makeComplex(makeRational(-1, 4), e)));
    },


    'negative zero': function() {
	assertTrue(eqv(negative_zero,
		       add(negative_zero, negative_zero)));
	assertFalse(eqv(negative_zero,
		       add(makeFloat(0), negative_zero)));
	assertTrue(eqv(makeFloat(0),
		       add(makeFloat(0), negative_zero)));
	assertFalse(eqv(negative_zero,
		       add(negative_zero, makeFloat(0))));
	assertTrue(eqv(makeFloat(0),
		       add(negative_zero, makeFloat(0))));
	assertTrue(eqv(makeComplex(negative_zero, makeFloat(0)),
		       add(0, makeComplex(negative_zero, makeFloat(0)))));
	assertTrue(eqv(makeComplex(makeFloat(0), makeFloat(0)),
		       add(makeFloat(0), makeComplex(negative_zero, makeFloat(0)))));
	assertTrue(eqv(makeComplex(negative_zero, makeFloat(0)),
		       add(negative_zero, makeComplex(negative_zero, makeFloat(0)))));
    },


    '0 acts as the identity': function() {
	assertTrue(eqv(add(0, 42),
		       42));
	assertTrue(eqv(add(0, -42),
		       -42));
	assertTrue(eqv(add(0, makeBignum("129834789213412345678910")),
		       makeBignum("129834789213412345678910")));
 	assertTrue(eqv(add(0, makeFloat(0.12345)),
 		       makeFloat(0.12345)));
 	assertTrue(eqv(add(0, nan),
 		       nan));
 	assertTrue(eqv(add(0, inf),
 		       inf));
 	assertTrue(eqv(add(0, negative_inf),
 		       negative_inf));


	assertTrue(eqv(add(42, 0),
		       42));
	assertTrue(eqv(add(-42, 0),
		       -42));
	assertTrue(eqv(add(makeBignum("129834789213412345678910"), 0),
		       makeBignum("129834789213412345678910")));
 	assertTrue(eqv(add(makeFloat(0.12345), 0),
 		       makeFloat(0.12345)));
 	assertTrue(eqv(add(nan, 0),
 		       nan));
 	assertTrue(eqv(add(inf, 0),
 		       inf));
 	assertTrue(eqv(add(negative_inf, 0),
 		       negative_inf));
    }
});


describe('subtract', {
    'fixnum / fixnum' : function() {
	assertTrue(eqv(subtract(fromFixnum(32768),
				fromFixnum(32768)),
		       fromFixnum(0)));
	assertTrue(eqv(subtract(10, 1), 9));
	assertTrue(eqv(subtract(1, 10), -9));

	assertTrue(eqv(subtract(13274, 1659),
		       11615));
	assertTrue(eqv(subtract(-13274, 1659),
		       -14933));
    },

    'finum overflows to bignum' : function() {
	var aNumber = 0;
	for (var i = 0 ; i < 1000; i++) {
	    aNumber = subtract(aNumber, fromFixnum(1e20));
	}
	assertTrue(eqv(makeBignum("-1e23"), aNumber));
    },


    'fixnum / bignum': function() {
	assertTrue(eqv(subtract(1, makeBignum("3219789841236123678239865237892936825367953267523689352968869532869")),
		       makeBignum("-3219789841236123678239865237892936825367953267523689352968869532868")));

	assertTrue(eqv(subtract(-1,
				makeBignum("3219789841236123678239865237892936825367953267523689352968869532869")),
		       makeBignum("-3219789841236123678239865237892936825367953267523689352968869532870")));
    },

    'bignum / bignum' : function() {
	assertTrue(eqv(subtract(makeBignum("10e500"),
				makeBignum("10e500")),
		       0));
	assertFalse(eqv(subtract(makeBignum("0"),
				 makeBignum("0")),
			1));
	assertTrue(eqv(subtract(makeBignum("0"),
				 makeBignum("-1")),
			1));
	assertTrue(eqv(subtract(makeBignum("235869532689532689523689532689"),
				makeBignum("532679532692536916785915689")),
		       makeBignum("235336853156840152606903617000")));
	assertTrue(eqv(subtract(makeBignum("235869532689532689523689532689"),
				makeBignum("-532679532692536916785915689")),
		       makeBignum("236402212222225226440475448378")));

	assertTrue(eqv(subtract(makeBignum("1e500"),
				makeBignum("1e400")),
		       makeBignum("99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999990000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")));
    },

    'bignum / rational': function() {
	assertTrue(eqv(subtract(makeBignum("12342539328423789239827369"),
				makeRational(1, 2)),
		       makeRational(makeBignum("24685078656847578479654737"),
				    2)));


	assertTrue(eqv(subtract(makeBignum("12342539328423789239827369"),
				makeRational(makeBignum("32658963528962385326953269"),
					     makeBignum("653289953253269"))),
		       makeRational(makeBignum("8063256940892578770775763336720167965992"),
				    makeBignum("653289953253269"))));
    },

    'bignum / float' : function() {
	assertTrue(eqv(subtract(makeBignum("1e50"),
				makeFloat(1)),
		       makeFloat(1e50-1)));
	assertFalse(eqv(subtract(makeBignum("1e50"),
				 makeFloat(1e-50)),
			makeBignum("1e50")));
	assertTrue(equals(subtract(makeBignum("1e50"),
				   makeFloat(1e-50)),
			  makeBignum("1e50")));


	assertTrue(eqv(subtract(makeBignum("0"),
				inf),
		       negative_inf));
	assertTrue(eqv(subtract(inf,
				makeBignum("0")),
		       inf));
	assertTrue(eqv(subtract(makeBignum("1e500"),
				inf),
		       negative_inf));
	assertTrue(eqv(subtract(inf,
				makeBignum("1e500")),
		       inf));
	assertTrue(eqv(subtract(makeBignum("-1e500"),
				inf),
		       negative_inf));
	assertTrue(eqv(subtract(inf,
				makeBignum("-1e500")),
		       inf));
    },

    'bignum / complex' : function() {
	assertTrue(eqv(0, subtract(makeBignum("42"),
				     makeComplex(42, 0))));
	assertTrue(eqv(makeComplex(-1, -4567),
		       subtract(makeBignum("1233"),
				makeComplex(1234, 4567))));
	assertTrue(eqv(makeComplex(-1, 4567),
		       subtract(makeBignum("1233"),
				makeComplex(1234, -4567))));
    },

    'fixnum / rational' : function() {
	assertTrue(eqv(makeRational(1, 2),
	               subtract(1, makeRational(1, 2))));
	assertTrue(eqv(makeRational(-1, 2),
	               subtract(1, makeRational(3, 2))));
	assertTrue(eqv(makeRational(341, 20),
	               subtract(17, makeRational(-1, 20))));
    },

    'fixnum / floating' : function() {
	assertTrue(eqv(makeFloat(0),
		       subtract(1, makeFloat(1.0))));

	assertTrue(eqv(makeFloat(-22998.1),
		       subtract(2398, makeFloat(25396.1))));
    },

    'fixnum / complex' : function() {
	assertTrue(eqv(makeComplex(0, -1),
		       subtract(0, makeComplex(0, 1))));
 	assertTrue(eqv(makeComplex(negative_zero, makeFloat(-1.1234)),
 		       subtract(0, makeComplex(makeFloat(0), makeFloat(1.1234)))));
 	assertTrue(eqv(makeComplex(negative_zero, makeFloat(1.1234)),
 		       subtract(0, makeComplex(makeFloat(0), makeFloat(-1.1234)))));
 	assertTrue(eqv(makeComplex(makeFloat(234), makeFloat(1.1234)),
 		       subtract(234, makeComplex(makeFloat(0), makeFloat(-1.1234)))));
 	assertTrue(eqv(makeComplex(makeFloat(200), makeFloat(1.1234)),
 		       subtract(234, makeComplex(makeFloat(34), makeFloat(-1.1234)))));
 	assertTrue(eqv(makeComplex(makeFloat(-24), makeFloat(1.1234)),
 		       subtract(0, makeComplex(makeFloat(24), makeFloat(-1.1234)))));
 	assertTrue(eqv(makeComplex(makeFloat(toFixnum(makeRational(16, 17))), 
 				   makeFloat(1.1234)),
 		       subtract(1, makeComplex(makeFloat(toFixnum(makeRational(1, 17))), 
 					       makeFloat(-1.1234)))));
    },

    'rational / rational' : function() {
	value_of(subtract(makeRational(1, 3), makeRational(1, 2))
		).should_be(makeRational(-1, 6));
	value_of(subtract(makeRational(6, 3), makeRational(0, 1))
		).should_be(makeRational(2, 1));
	value_of(subtract(makeRational(6, -3), makeRational(0, 1))
		).should_be(makeRational(-2, 1));
	value_of(subtract(makeRational(-1, 3), makeRational(1, -2))
		).should_be(makeRational(1, 6));
    },
    'rational / floating' : function() {
	// FIXME: we're missing this
    },
    'rational / complex' : function() {
	// FIXME: we're missing this
    },

    'floating / floating' : function() {
	assertTrue(eqv(subtract(makeFloat(1.2), makeFloat(0.2)),
		       makeFloat(1.2-0.2)));

	assertTrue(eqv(subtract(nan, nan),
		       nan));
	assertTrue(eqv(subtract(nan, makeFloat(3.0)), 
		       nan));
	assertTrue(eqv(subtract(nan, makeFloat(-3.0)), 
		       nan));
	// FIXME: we're missing this
    },

    'negative zero': function() {
	assertTrue(eqv(makeFloat(0.0),
		       subtract(negative_zero, negative_zero)));
 	assertTrue(eqv(makeFloat(0.0),
 		       subtract(makeFloat(0), negative_zero)));
 	assertTrue(eqv(negative_zero,
 		       subtract(negative_zero, 0)));
 	assertTrue(eqv(negative_zero,
 		       subtract(negative_zero, makeFloat(0))));
 	assertTrue(eqv(makeComplex(negative_zero, negative_zero),
 		       subtract(0, makeComplex(makeFloat(0), makeFloat(0)))));
    },

    'floating / complex' : function() {
	// FIXME: we're missing this
    },
    'complex / complex' : function() {
	// FIXME: we're missing this
    },


    'negation': function() {
	assertTrue(eqv(subtract(0, 42),
		       -42));
	assertTrue(eqv(subtract(0, -42),
		       42));
	assertTrue(eqv(subtract(0, makeBignum("129834789213412345678910")),
		       makeBignum("-129834789213412345678910")));
  	assertTrue(eqv(subtract(0, makeFloat(0.12345)),
  		       makeFloat(-0.12345)));
  	assertTrue(eqv(subtract(0, makeFloat(0)),
  		       negative_zero));
  	assertTrue(eqv(subtract(0, negative_zero),
  		       makeFloat(0)));
  	assertTrue(eqv(subtract(0, nan),
  		       nan));
  	assertTrue(eqv(subtract(0, inf),
  		       negative_inf));
  	assertTrue(eqv(subtract(0, negative_inf),
  		       inf));
    }
});


describe('multiply', {
    'fixnum / fixnum' : function() {
	// FIXME: add test case where value needs to become a bignum.
    },

    'fixnum overflows to bignum': function() {
	// FIXME
    },

    'fixnum / bignum': function() {
	// FIXME: we're missing this
    },

    'bignum / bignum' : function() {
	// FIXME: we're missing this
    },

    'bignum / rational': function() {
	// FIXME: we're missing this
    },

    'bignum / float' : function() {
	// FIXME: we're missing this
    },

    'bignum / complex' : function() {
	// FIXME: we're missing this
    },

    'fixnum / rational' : function() {
	// FIXME: we're missing this
    },
    'fixnum / floating' : function() {
	// FIXME: we're missing this
    },
    'fixnum / complex' : function() {
	// FIXME: we're missing this
    },
    'rational / rational' : function() {
	// FIXME: we're missing this
    },
    'rational / floating' : function() {
	// FIXME: we're missing this
    },
    'rational / complex' : function() {
	// FIXME: we're missing this
    },
    'floating / floating' : function() {
	// FIXME: we're missing this
    },
    'floating / complex' : function() {
	// FIXME: we're missing this
    },
    'complex / complex' : function() {
	// FIXME: we're missing this
    },

    'negative zero': function() {
	assertTrue(eqv(makeFloat(0),
		       multiply(negative_zero, negative_zero)));
  	assertTrue(eqv(negative_zero,
  		       multiply(makeFloat(0), negative_zero)));
  	assertTrue(eqv(negative_zero,
  		       multiply(negative_zero, makeFloat(0))));
 	assertTrue(eqv(makeComplex(0, 0),
 		       multiply(0, makeComplex(makeFloat(0), makeFloat(0)))));
 	assertTrue(eqv(makeComplex(negative_zero, negative_zero),
 		       multiply(-1, makeComplex(makeFloat(0), makeFloat(0)))));
 	assertTrue(eqv(makeComplex(negative_zero, negative_zero),
 		       multiply(makeComplex(makeFloat(0), makeFloat(0)), -1)));
    },

    '1 acts as the identity': function() {
	assertTrue(eqv(0,
		       multiply(1, 0)));
	assertTrue(eqv(1,
		       multiply(1, 1)));
	assertTrue(eqv(-1,
		       multiply(1, -1)));
	assertTrue(eqv(1234,
		       multiply(1, 1234)));
	assertTrue(eqv(makeBignum("1929365432165895132685321689"),
		       multiply(1, makeBignum("1929365432165895132685321689"))));
	assertTrue(eqv(makeBignum("-1929365432165895132685321689"),
		       multiply(1, makeBignum("-1929365432165895132685321689"))));
	assertTrue(eqv(makeRational(1, 24),
		       multiply(1, makeRational(1, 24))));
	assertTrue(eqv(makeRational(-21, 24),
		       multiply(1, makeRational(-21, 24))));
	assertTrue(eqv(makeFloat(123.45),
		       multiply(1, makeFloat(123.45))));	
	assertTrue(eqv(makeFloat(0),
		       multiply(1, makeFloat(0))));
	assertTrue(eqv(nan,
		       multiply(1, nan)));
	assertTrue(eqv(inf,
		       multiply(1, inf)));
	assertTrue(eqv(negative_inf,
		       multiply(1, negative_inf)));
	assertTrue(eqv(negative_zero,
		       multiply(1, negative_zero)));
    }
});


describe('divide', {
    'fixnum / fixnum' : function() {
	// FIXME: add test case where value needs to become a bignum.
    },

    'fixnum overflows to bignum': function() {
	// FIXME
    },

    'fixnum / bignum': function() {
	// FIXME: we're missing this
    },

    'bignum / bignum' : function() {
	// FIXME: we're missing this
    },

    'bignum / rational': function() {
	// FIXME: we're missing this
    },

    'bignum / float' : function() {
	// FIXME: we're missing this
    },

    'bignum / complex' : function() {
	// FIXME: we're missing this
    },

    'fixnum / rational' : function() {
	// FIXME: we're missing this
    },
    'fixnum / floating' : function() {
	// FIXME: we're missing this
    },
    'fixnum / complex' : function() {
	// FIXME: we're missing this
    },
    'rational / rational' : function() {
	// FIXME: we're missing this
    },
    'rational / floating' : function() {
	// FIXME: we're missing this
    },
    'rational / complex' : function() {
	// FIXME: we're missing this
    },
    'floating / floating' : function() {
	// FIXME: we're missing this
    },
    'floating / complex' : function() {
	// FIXME: we're missing this
    },
    'complex / complex' : function() {
	assertTrue(eqv(divide(makeComplex(makeFloat(1e300),
					  makeFloat(1e300)),
			      makeComplex(makeFloat(4e300), 
					  makeFloat(4e300))),
		       makeComplex(makeFloat(.25), makeFloat(0.0))));
	assertTrue(eqv(divide(makeComplex(2, 6),
			      makeComplex(4, 1)),
		       makeComplex(makeRational(14, 17),
				   makeRational(22, 17))));
		       
    },

    'division by zeros' : function() {
	assertFails(function() {divide(1, 0);});
	assertFails(function() {divide(makeRational(1, 2), 0);});
	assertFails(function() {divide(makeComplex(1, 2), 0);});

 	assertTrue(eqv(inf, divide(1, makeFloat(0.0))));
 	assertTrue(eqv(negative_inf, divide(1, negative_zero)));

 	assertTrue(eqv(inf, divide(makeFloat(42), makeFloat(0.0))));
 	assertTrue(eqv(negative_inf, divide(makeFloat(-42), makeFloat(0.0))));
 	assertTrue(eqv(negative_inf, divide(makeFloat(42), negative_zero)));
 	assertTrue(eqv(inf, divide(makeFloat(-42), negative_zero)));
 	assertTrue(eqv(inf, divide(makeRational(1, 2), makeFloat(0.0))));
 	assertTrue(eqv(makeComplex(inf, inf), divide(makeComplex(1, 2), makeFloat(0.0))));
    },

    'division 0 by 0': function() {
	assertFails(function() { divide(0, 0); });
	assertFails(function() { divide(makeFloat(0), 0); });
	assertTrue(eqv(nan,
		       divide(makeFloat(0), makeFloat(0))));
	assertTrue(eqv(0,
		       divide(0, makeFloat(0))));
    }
});


describe('greaterThanOrEqual', {
    'fixnum / fixnum' : function() {
	// FIXME: we're missing this
    },
    'fixnum / bignum': function() {
	// FIXME: we're missing this
    },

    'bignum / bignum' : function() {
	// FIXME: we're missing this
    },

    'bignum / rational': function() {
	// FIXME: we're missing this
    },

    'bignum / float' : function() {
	// FIXME: we're missing this
    },

    'bignum / complex' : function() {
	// FIXME: we're missing this
    },

    'fixnum / rational' : function() {
	// FIXME: we're missing this
    },
    'fixnum / floating' : function() {
	// FIXME: we're missing this
    },
    'fixnum / complex' : function() {
	// FIXME: we're missing this
    },
    'rational / rational' : function() {
	// FIXME: we're missing this
    },
    'rational / floating' : function() {
	// FIXME: we're missing this
    },
    'rational / complex' : function() {
	// FIXME: we're missing this
    },
    'floating / floating' : function() {
	// FIXME: we're missing this
    },
    'floating / complex' : function() {
	// FIXME: we're missing this
    },
    'complex / complex' : function() {
	// FIXME: we're missing this
    }
});


describe('lessThanOrEqual', {
    'fixnum / fixnum' : function() {
	// FIXME: we're missing this
    },

    'fixnum / bignum': function() {
	// FIXME: we're missing this
    },

    'bignum / bignum' : function() {
	// FIXME: we're missing this
    },

    'bignum / rational': function() {
	// FIXME: we're missing this
    },

    'bignum / float' : function() {
	// FIXME: we're missing this
    },

    'bignum / complex' : function() {
	// FIXME: we're missing this
    },

    'fixnum / rational' : function() {
	// FIXME: we're missing this
    },
    'fixnum / floating' : function() {
	// FIXME: we're missing this
    },
    'fixnum / complex' : function() {
	// FIXME: we're missing this
    },
    'rational / rational' : function() {
	// FIXME: we're missing this
    },
    'rational / floating' : function() {
	// FIXME: we're missing this
    },
    'rational / complex' : function() {
	// FIXME: we're missing this
    },
    'floating / floating' : function() {
	// FIXME: we're missing this
    },
    'floating / complex' : function() {
	// FIXME: we're missing this
    },
    'complex / complex' : function() {
	// FIXME: we're missing this
    }
});


describe('greaterThan', {
    'fixnum / fixnum' : function() {
	// FIXME: we're missing this
    },
    'fixnum / bignum': function() {
	// FIXME: we're missing this
    },

    'bignum / bignum' : function() {
	// FIXME: we're missing this
    },

    'bignum / rational': function() {
	// FIXME: we're missing this
    },

    'bignum / float' : function() {
	// FIXME: we're missing this
    },

    'bignum / complex' : function() {
	// FIXME: we're missing this
    },
    'fixnum / rational' : function() {
	// FIXME: we're missing this
    },
    'fixnum / floating' : function() {
	// FIXME: we're missing this
    },
    'fixnum / complex' : function() {
	// FIXME: we're missing this
    },
    'rational / rational' : function() {
	// FIXME: we're missing this
    },
    'rational / floating' : function() {
	// FIXME: we're missing this
    },
    'rational / complex' : function() {
	// FIXME: we're missing this
    },
    'floating / floating' : function() {
	// FIXME: we're missing this
    },
    'floating / complex' : function() {
	// FIXME: we're missing this
    },
    'complex / complex' : function() {
	// FIXME: we're missing this
    }
});


describe('lessThan', {
    'fixnum / fixnum' : function() {
	// FIXME: we're missing this
    },
    'fixnum / bignum': function() {
	// FIXME: we're missing this
    },

    'bignum / bignum' : function() {
	// FIXME: we're missing this
    },

    'bignum / rational': function() {
	// FIXME: we're missing this
    },

    'bignum / float' : function() {
	// FIXME: we're missing this
    },

    'bignum / complex' : function() {
	// FIXME: we're missing this
    },
    'fixnum / rational' : function() {
	// FIXME: we're missing this
    },
    'fixnum / floating' : function() {
	// FIXME: we're missing this
    },
    'fixnum / complex' : function() {
	// FIXME: we're missing this
    },
    'rational / rational' : function() {
	// FIXME: we're missing this
    },
    'rational / floating' : function() {
	// FIXME: we're missing this
    },
    'rational / complex' : function() {
	// FIXME: we're missing this
    },
    'floating / floating' : function() {
	// FIXME: we're missing this
    },
    'floating / complex' : function() {
	// FIXME: we're missing this
    },
    'complex / complex' : function() {
	// FIXME: we're missing this
    }
});


describe('expt', {
    'fixnum / fixnum' : function() {
	assertTrue(eqv(expt(2, 0), 1));
 	assertTrue(eqv(expt(2, 1), 2));
 	assertTrue(eqv(expt(2, 100), makeBignum("1267650600228229401496703205376")));
 	assertTrue(eqv(expt(2, -1),
 		       makeRational(1,2)));
 	assertTrue(eqv(expt(2, -2),
 		       makeRational(1,4)));
 	assertTrue(eqv(expt(2, -100),
 		       makeRational(1, makeBignum("1267650600228229401496703205376"))));
    },

    'fixnum overflows to bignum': function() {
	assertTrue(eqv(expt(2, 1000),
		       makeBignum("10715086071862673209484250490600018105614048117055336074437503883703510511249361224931983788156958581275946729175531468251871452856923140435984577574698574803934567774824230985421074605062371141877954182153046474983581941267398767559165543946077062914571196477686542167660429831652624386837205668069376")));
    },

    'fixnum / bignum': function() {
	assertTrue(eqv(expt(1, makeBignum("123689321689125689")),
		       1));

	assertTrue(eqv(expt(1, makeBignum("-123689321689125689")),
		       1));
    },

    'bignum / bignum' : function() {
	assertTrue(eqv(expt(makeBignum("0"),
			    makeBignum("0")),
		       1));
	assertTrue(eqv(expt(makeBignum("2"),
			    makeBignum("999")),
		       makeBignum("5357543035931336604742125245300009052807024058527668037218751941851755255624680612465991894078479290637973364587765734125935726428461570217992288787349287401967283887412115492710537302531185570938977091076523237491790970633699383779582771973038531457285598238843271083830214915826312193418602834034688")));

	
	assertTrue(eqv(expt(makeBignum("-2"),
			    makeBignum("1")),
		       -2));

	assertTrue(eqv(expt(makeBignum("-2"),
			    makeBignum("2")),
		       4));

	assertTrue(eqv(expt(makeBignum("2"),
			    makeBignum("-1")),
		       makeRational(1, 2)));


	assertTrue(eqv(expt(makeBignum("2"),
			    makeBignum("-999")),
		       makeRational(1, 
				    makeBignum("5357543035931336604742125245300009052807024058527668037218751941851755255624680612465991894078479290637973364587765734125935726428461570217992288787349287401967283887412115492710537302531185570938977091076523237491790970633699383779582771973038531457285598238843271083830214915826312193418602834034688"))));
    },
	
    'bignum / rational': function() {
	// FIXME: we're missing this
    },

    'bignum / float' : function() {
	// FIXME: we're missing this
    },

    'bignum / complex' : function() {
	// FIXME: we're missing this
    },
    'fixnum / rational' : function() {
	// FIXME: we're missing this
    },
    'fixnum / floating' : function() {
	// FIXME: we're missing this
    },
    'fixnum / complex' : function() {
	// FIXME: we're missing this
    },
    'rational / rational' : function() {
	// FIXME: we're missing this
    },
    'rational / floating' : function() {
	// FIXME: we're missing this
    },
    'rational / complex' : function() {
	// FIXME: we're missing this
    },
    'floating / floating' : function() {
	// FIXME: we're missing this
    },
    'floating / complex' : function() {
	// FIXME: we're missing this
    },
    'complex / complex' : function() {
	// FIXME: we're missing this
    },
    'expt of anything to zero is 1' : function() {
	assertTrue(eqv(1, expt(nan, 0)));
	assertTrue(eqv(1, expt(inf, 0)));
	assertTrue(eqv(1, expt(negative_inf, 0)));
	assertTrue(eqv(1, expt(negative_zero, 0)));
	assertTrue(eqv(1, expt(0, 0)));
    }
});


describe('modulo', {
    'fixnum / fixnum' : function() {
	// FIXME: we're missing this
    },
    'fixnum / bignum': function() {
	// FIXME: we're missing this
    },

    'bignum / bignum' : function() {
	// FIXME: we're missing this
    },

    'bignum / rational': function() {
	// FIXME: we're missing this
    },

    'bignum / float' : function() {
	// FIXME: we're missing this
    },

    'bignum / complex' : function() {
	// FIXME: we're missing this
    },
    'fixnum / rational' : function() {
	// FIXME: we're missing this
    },
    'fixnum / floating' : function() {
	// FIXME: we're missing this
    },
    'fixnum / complex' : function() {
	// FIXME: we're missing this
    },
    'rational / rational' : function() {
	// FIXME: we're missing this
    },
    'rational / floating' : function() {
	// FIXME: we're missing this
    },
    'rational / complex' : function() {
	// FIXME: we're missing this
    },
    'floating / floating' : function() {
	// FIXME: we're missing this
    },
    'floating / complex' : function() {
	// FIXME: we're missing this
    },
    'complex / complex' : function() {
	// FIXME: we're missing this
    }
});


describe('numerator', {
    'fixnums': function() {
	// FIXME: we're missing this
    },

    'bignums': function() {
	// FIXME: we're missing this
    },

    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});


describe('denominator', {
    'fixnums': function() {
 	// FIXME: we're missing this
    },
    'bignums': function() {
	// FIXME: we're missing this
    },
    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});


describe('sqrt', {
    'fixnums': function() {
	assertTrue(eqv(sqrt(4), 2));
	assertTrue(eqv(sqrt(-4), makeComplex(0, 2)));
	assertTrue(eqv(sqrt(-1), makeComplex(0, 1)));
	assertTrue(eqv(sqrt(297354289), makeFloat(17243.963842458033)));
	assertTrue(eqv(sqrt(-297354289), 
		       makeComplex(0, 
				   makeFloat(17243.963842458033))));
    },

    'bignums': function() {
	assertTrue(eqv(sqrt(makeBignum("4")),
		       2))

	assertTrue(eqv(sqrt(makeBignum("-4")),
		       makeComplex(0, 2)))

	assertTrue(diffPercent(makeFloat(4893703081.846022), 
			       sqrt(makeBignum("23948329853269253680")))
		   < 1e-2)
	assertTrue(diffPercent(sqrt(expt(15, 21)).toFixnum(),
			       2233357359474.6265)
		   < 2e-10);
	assertTrue(eqv(sqrt(expt(15, 22)),
		       expt(15, 11)));
	assertTrue(eqv(sqrt(expt(15, 22)),
		       expt(15, 11)));
    },

    'rationals': function() {
	assertTrue(eqv(sqrt(makeRational(1, 4)),
		       makeRational(1, 2)));
	
	assertTrue(eqv(sqrt(makeRational(-1, 4)),
		       makeComplex(0, makeRational(1, 2))));
    },

    'floats': function() {
	assertTrue(eqv(sqrt(makeFloat(-4)), makeComplex(0, makeFloat(2))));
	assertTrue(eqv(sqrt(makeFloat(4)), makeFloat(2)));
    },

    'complex': function() {
	// FIXME: we're missing this
    }
});


describe('abs', {
    'fixnums': function() {
	assertEquals(0, abs(0));
	assertEquals(42, abs(42));
	assertEquals(42, abs(-42));
	assertEquals(1, abs(-1));
    },
    'bignums': function() {
	assertTrue(eqv(makeBignum("23569236859962835638935268952936825689536829253968"),
		       abs(makeBignum("23569236859962835638935268952936825689536829253968"))));
	assertTrue(eqv(makeBignum("23569236859962835638935268952936825689536829253968"),
		       abs(makeBignum("-23569236859962835638935268952936825689536829253968"))));
	assertEquals(makeBignum("1"),
		     abs(makeBignum("-1")));
	assertEquals(makeBignum("1"),
		     abs(makeBignum("1")));
	assertEquals(makeBignum("0"),
		     abs(makeBignum("0")));

    },
    'rationals': function() {
	assertEquals(makeRational(2, 1),
	             abs(makeRational(-2, 1)));
	assertEquals(makeRational(2, 1),
	             abs(makeRational(2, 1)));
	assertEquals(makeRational(0, 1),
	             abs(makeRational(0, 1)));
	assertEquals(makeRational(3298, 28),
	             abs(makeRational(3298, 28)));
	assertEquals(makeRational(3298, 28),
	             abs(makeRational(-3298, 28)));
    },

    'floats': function() {
	assertEquals(makeFloat(0.0),
		     abs(makeFloat(0.0)));
	assertTrue(eqv(makeFloat(0.0),
		       abs(negative_zero)));
	assertTrue(eqv(makeFloat(1342.7),
		       abs(makeFloat(1342.7))));
	assertTrue(eqv(makeFloat(1342.7),
		       abs(makeFloat(-1342.7))));
    },

    'complex': function() {
	assertTrue(eqv(makeComplex(2, 0),
		       abs(makeComplex(-2, 0))));
	assertTrue(eqv(makeComplex(2, 0),
	           abs(makeComplex(2, 0))));
    }
});


describe('floor', {
    'fixnums': function() {
        assertEquals(0, floor(0));
        assertEquals(1, floor(1));
        assertEquals(-1, floor(-1));
        assertEquals(1, floor(makeFloat(1.2)));
        assertEquals(-2, floor(makeFloat(-1.2)));
        assertEquals(123123123, floor(makeFloat(123123123.9)));
        assertEquals(-1000000000, floor(makeFloat(-999999999.9)));
    },
    'bignums': function() {
        assertTrue(eqv(makeBignum("0"), floor(makeBignum("0"))));
        assertTrue(eqv(makeBignum("1"), floor(makeBignum("1"))));
        assertTrue(eqv(makeBignum("-1"), floor(makeBignum("-1"))));
        assertTrue(eqv(makeBignum("100"), floor(makeBignum("100"))));
        assertTrue(eqv(makeBignum("-100"), floor(makeBignum("-100"))));
        assertTrue(eqv(makeBignum("10000000000"), floor(makeBignum("10000000000"))));
        assertTrue(eqv(makeBignum("-10000000000"), floor(makeBignum("-10000000000"))));
        assertTrue(eqv(makeBignum("100000000000000000000"), floor(makeBignum("100000000000000000000"))));
        assertTrue(eqv(makeBignum("-100000009000000800000"), floor(makeBignum("-100000009000000800000"))));
    },
    'rationals': function() {
        assertTrue(eqv(makeBignum("0"), floor(makeRational(makeBignum("0"),
							   makeBignum("1")))));
        assertTrue(eqv(makeBignum("0"), floor(makeRational(makeBignum("0"),
							   makeBignum("-1")))));
        assertTrue(eqv(makeBignum("0"), floor(makeRational(makeBignum("1"),
							   makeBignum("2")))));
        assertTrue(eqv(makeBignum("1"), floor(makeRational(makeBignum("1"),
							   makeBignum("1")))));
        assertTrue(eqv(makeBignum("1"), floor(makeRational(makeBignum("3"),
							   makeBignum("2")))));
        assertTrue(eqv(makeBignum("-2"),
		       floor(makeRational(makeBignum("-3"),
					  makeBignum("2")))));
        assertTrue(eqv(makeBignum("-2"), 
		       floor(makeRational(makeBignum("3"),
					  makeBignum("-2")))));

        assertTrue(eqv(makeBignum("1"),
		       floor(makeRational(makeBignum("-3"),
					  makeBignum("-2")))));
        assertTrue(eqv(makeBignum("0"), 
		       floor(makeRational(makeBignum("100000000000000000000"),
					  makeBignum("200000000000000000000")))));
        assertTrue(eqv(makeBignum("33333333"), 
		       floor(makeRational(makeBignum("100000001"),
					  makeBignum("3")))));
        assertTrue(eqv(makeBignum("99481699535420"),
		       floor(makeRational(makeBignum("123456789123456789"),
					  makeBignum("1241")))));
        assertTrue(eqv(makeBignum("88104168679280445959689"),
		       floor(makeRational(makeBignum("8236487236482736823687236826582365827652875"),
					  makeBignum("93485783475983475983")))));


        assertTrue(eqv(makeBignum("0"), floor(makeRational(0, 1))));
        assertTrue(eqv(makeBignum("0"), floor(makeRational(0, -1))));
        assertTrue(eqv(makeBignum("0"), floor(makeRational(1, 2))));
        assertTrue(eqv(makeBignum("1"), floor(makeRational(1, 1))));
        assertTrue(eqv(makeBignum("1"), floor(makeRational(3, 2))));
        assertTrue(eqv(makeBignum("-2"), floor(makeRational(-3, 2))));
        assertTrue(eqv(makeBignum("-2"), floor(makeRational(3, -2))));
        assertTrue(eqv(makeBignum("1"), floor(makeRational(-3, -2))));
        assertTrue(eqv(makeBignum("33333333"), 
		       floor(makeRational(makeBignum("100000001"),
					  3))));
        assertTrue(eqv(makeBignum("99481699535420"),
		       floor(makeRational(makeBignum("123456789123456789"),
					  1241))));

	assertTrue(eqv(1,
		       floor(makeRational(7, 5))));
    },


    'floats': function() {
        assertEqv(makeFloat(0.0), floor(makeFloat(0.0)));
        assertEqv(makeFloat(1), floor(makeFloat(1.0)));
        assertEqv(makeFloat(-1), floor(makeFloat(-1.0)));
        assertEqv(makeFloat(1), floor(makeFloat(1.1)));
        assertEqv(makeFloat(1), floor(makeFloat(1.999)));
        assertEqv(makeFloat(-2), floor(makeFloat(-1.999)));
        assertEqv(makeFloat(123456), floor(makeFloat(123456.789)));
        assertEqv(makeFloat(1234567891234567),
		  floor(makeFloat(1234567891234567.8)));
        assertEqv(makeFloat(-1234567891234568),
		     floor(makeFloat(-1234567891234567.8)));
 	assertEqv(nan, floor(nan));
 	assertEqv(inf, floor(inf));
 	assertEqv(negative_inf, floor(negative_inf));
 	assertEqv(negative_zero, floor(negative_zero));
    },

    'complex': function() {
        assertTrue(eqv(nan, floor(makeComplex(nan, 0))));
	assertFails(function() { floor(makeComplex(0, nan))});
	assertFails(function() { floor(makeComplex(nan, 1))});
	assertFails(function() { floor(makeComplex(1, nan))});
	assertFails(function() { floor(makeComplex(nan, inf))});
	assertFails(function() { floor(makeComplex(inf, nan))});
     	assertFails(function() { floor(makeComplex(negative_zero, nan))});
     	assertFails(function() { floor(makeComplex(nan, nan))});
        assertFails(function() { floor(makeComplex(nan, negative_zero))} );
      	assertFails(function() { floor(makeComplex(inf,inf))});
      	assertFails(function() { floor(makeComplex(0,inf))});
      	assertTrue(eqv(inf, floor(makeComplex(inf,0))));
        assertFails(function() { floor(makeComplex(negative_inf,negative_inf))});
        assertFails(function() { floor(makeComplex(makeBignum("0"),makeBignum("2")))});
        assertFails(function() { floor(makeComplex(makeBignum("1"),makeBignum("2")))});

        assertTrue(eqv(1102,
 		       floor(makeComplex(makeRational(makeBignum("9919"),
 						      makeBignum("9")),
 					 0))));

        assertFails(function() { floor(makeComplex(makeRational(makeBignum("9919"),makeBignum("9")),makeBignum("200")))});
        assertFails(function() { floor(makeComplex(makeFloat(4.25), makeFloat(1.5)))});

        assertTrue(eqv(makeBignum("0"), floor(makeComplex(makeBignum("0"),
							  makeBignum("0")))));
        assertTrue(eqv(makeBignum("-1"), floor(makeComplex(makeBignum("-1"),
							   makeBignum("0")))));
        assertTrue(eqv(makeBignum("1"), floor(makeComplex(makeBignum("1"),
							  makeBignum("0")))));
        assertTrue(eqv(makeBignum("1"), floor(makeComplex(makeRational(makeBignum("1")),
							  makeBignum("0")))));
        assertTrue(eqv(makeBignum("1"), floor(makeComplex(makeRational(makeBignum("3"),
								       makeBignum("2")),
							  makeBignum("0")))));
        assertTrue(eqv(makeBignum("0"),
 		       floor(makeComplex(
 			   makeRational(makeBignum("100000000000000000000"),
 					makeBignum("200000000000000000000")),
 			   makeBignum("0")))));
        assertFails(function() { floor(makeComplex(makeFloat(-1.999),
 						   makeFloat(0.0)))});
	assertFails(function() { floor(makeComplex(makeFloat(1234567891234567.8),
						   makeFloat(0))) });
    }
});



describe('ceiling', {
    'fixnums': function() {
        assertEquals(0, ceiling(0));
        assertEquals(1, ceiling(1));
        assertEquals(-1, ceiling(-1));
        assertEquals(2, ceiling(makeFloat(1.2)));
        assertEquals(-1, ceiling(makeFloat(-1.2)));
        assertEquals(123123124, ceiling(makeFloat(123123123.9)));
        assertEquals(-999999999, ceiling(makeFloat(-999999999.9)));
    },
    'bignums': function() {
        assertTrue(eqv(makeBignum("0"), ceiling(makeBignum("0"))));
        assertTrue(eqv(makeBignum("1"), ceiling(makeBignum("1"))));
        assertTrue(eqv(makeBignum("-1"), ceiling(makeBignum("-1"))));
        assertTrue(eqv(makeBignum("100"), ceiling(makeBignum("100"))));
        assertTrue(eqv(makeBignum("-100"), ceiling(makeBignum("-100"))));
        assertTrue(eqv(makeBignum("10000000000"), ceiling(makeBignum("10000000000"))));
        assertTrue(eqv(makeBignum("-10000000000"), ceiling(makeBignum("-10000000000"))));
        assertTrue(eqv(makeBignum("100000000000000000000"), ceiling(makeBignum("100000000000000000000"))));
        assertTrue(eqv(makeBignum("-100000009000000800000"), ceiling(makeBignum("-100000009000000800000"))));
    },

    'rationals': function() {
        assertTrue(eqv(makeBignum("0"),
		       ceiling(makeRational(makeBignum("0"),makeBignum("1")))));
        assertTrue(eqv(makeBignum("0"),
		       ceiling(makeRational(makeBignum("0"),makeBignum("-1")))));
        assertTrue(eqv(makeBignum("1"),
		       ceiling(makeRational(makeBignum("1"),makeBignum("2")))));
        assertTrue(eqv(makeBignum("1"),
		       ceiling(makeRational(makeBignum("1"),makeBignum("1")))));
        assertTrue(eqv(makeBignum("2"), 
		       ceiling(makeRational(makeBignum("3"),makeBignum("2")))));
        assertTrue(eqv(makeBignum("-1"), 
		       ceiling(makeRational(makeBignum("-3"),makeBignum("2")))));
        assertTrue(eqv(makeBignum("2"),
		       ceiling(makeRational(makeBignum("-3"),makeBignum("-2")))));
        assertTrue(eqv(makeBignum("1"),
		       ceiling(makeRational(makeBignum("100000000000000000000"),makeBignum("200000000000000000000")))));
        assertTrue(eqv(makeBignum("33333334"), 
		       ceiling(makeRational(makeBignum("100000001"),makeBignum("3")))));
        assertTrue(eqv(makeBignum("99481699535421"),
		       ceiling(makeRational(makeBignum("123456789123456789"),makeBignum("1241")))));
        assertTrue(eqv(makeBignum("88104168679280445959690"),
		       ceiling(makeRational(makeBignum("8236487236482736823687236826582365827652875"),
					    makeBignum("93485783475983475983")))));


        assertTrue(eqv(makeBignum("0"),
		       ceiling(makeRational(0, 1))));
        assertTrue(eqv(makeBignum("0"),
		       ceiling(makeRational(0, -1))));
        assertTrue(eqv(makeBignum("1"),
		       ceiling(makeRational(1, 2))));
        assertTrue(eqv(makeBignum("1"),
		       ceiling(makeRational(1, 1))));
        assertTrue(eqv(makeBignum("2"), 
		       ceiling(makeRational(3, 2))));
        assertTrue(eqv(makeBignum("-1"), 
		       ceiling(makeRational(-3, 2))));
        assertTrue(eqv(makeBignum("2"),
		       ceiling(makeRational(-3, -2))));
        assertTrue(eqv(makeBignum("33333334"), 
		       ceiling(makeRational(100000001, 3))));
        assertTrue(eqv(makeBignum("99481699535421"),
		       ceiling(makeRational(makeBignum("123456789123456789"),
					    1241))));
    },


    'floats': function() {
        assertEqv(makeFloat(0), ceiling(makeFloat(0.0)));
        assertEqv(makeFloat(1), ceiling(makeFloat(1.0)));
        assertEqv(makeFloat(-1), ceiling(makeFloat(-1.0)));
        assertEqv(makeFloat(2), ceiling(makeFloat(1.1)));
        assertEqv(makeFloat(2), ceiling(makeFloat(1.999)));
        assertEqv(makeFloat(-1), ceiling(makeFloat(-1.999)));
        assertEqv(makeFloat(123457), ceiling(makeFloat(123456.789)));
        assertEqv(makeFloat(1234567891234568),
		  ceiling(makeFloat(1234567891234567.8)));
        assertEqv(makeFloat(-1234567891234567),
		  ceiling(makeFloat(-1234567891234567.8)));
	assertEqv(nan, ceiling(nan));
	assertEqv(inf, ceiling(inf));
	assertEqv(negative_inf, ceiling(negative_inf));
	assertEqv(negative_zero, ceiling(negative_zero));
    },
    'complex': function() {
        assertTrue(eqv(nan, ceiling(nan)));
	assertFails(function() { ceiling(makeComplex(makeFloat(0), nan))});
	assertFails(function() { ceiling(makeComplex(nan, makeFloat(1)))});
	assertFails(function() { ceiling(makeComplex(makeFloat(1), nan))});
	assertFails(function() { ceiling(makeComplex(nan, inf))});
	assertFails(function() { ceiling(makeComplex(inf, nan))});
        assertFails(function() { ceiling(makeComplex(nan, negative_zero))} );
     	assertFails(function() { ceiling(makeComplex(negative_zero, nan))});
     	assertFails(function() { ceiling(makeComplex(nan, nan))});
     	assertFails(function() { ceiling(makeComplex(inf,inf))});
     	assertFails(function() { ceiling(makeComplex(makeFloat(0),inf))});
     	assertFails(function() { ceiling(makeComplex(inf,makeFloat(0)))});
        assertFails(function() { ceiling(makeComplex(negative_inf,negative_inf))});
        assertFails(function() { ceiling(makeComplex(makeBignum("0"),makeBignum("2")))});
        assertFails(function() { ceiling(makeComplex(makeBignum("1"),makeBignum("2")))});

        assertTrue(eqv(1103,
 		       ceiling(makeComplex(makeRational(makeBignum("9919"),
 							makeBignum("9")),
 					   0))));

        assertFails(function() { ceiling(makeComplex(makeRational(makeBignum("9919"),makeBignum("9")),makeBignum("200")))});
        assertFails(function() { ceiling(makeComplex(makeFloat(4.25), makeFloat(1.5)))});

        assertTrue(eqv(makeBignum("0"), ceiling(makeComplex(makeBignum("0"),
							    makeBignum("0")))));
        assertTrue(eqv(makeBignum("-1"), ceiling(makeComplex(makeBignum("-1"),
							     makeBignum("0")))));
        assertTrue(eqv(makeBignum("1"), ceiling(makeComplex(makeBignum("1"),
							    makeBignum("0")))));
        assertTrue(eqv(makeBignum("1"), ceiling(makeComplex(makeRational(makeBignum("1")),
							    makeBignum("0")))));
        assertTrue(eqv(makeBignum("2"), ceiling(makeComplex(makeRational(makeBignum("3"),
									 makeBignum("2")),
							    makeBignum("0")))));
        assertTrue(eqv(makeBignum("1"),
		       ceiling(makeComplex(
			   makeRational(makeBignum("100000000000000000000"),
					makeBignum("200000000000000000000")),
			   makeBignum("0")))));
        assertFails(function() { ceiling(makeComplex(makeFloat(-1.999),
 						     makeFloat(0)))});
        assertFails(function() { ceiling(makeComplex(makeFloat(1234567891234567.8),
 						     makeFloat(0)))});
    }
});


describe('conjugate', {
    'fixnums': function() {
	// FIXME: we're missing this
    },
    'bignums': function() {
	// FIXME: we're missing this
    },
    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});


describe('magnitude', {
    'fixnums': function() {
	// FIXME: we're missing this
    },
    'bignums': function() {
	// FIXME: we're missing this
    },
    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});


describe('log', {
    'fixnums': function() {
	// FIXME: we're missing this
    },
    'bignums': function() {
	// FIXME: we're missing this
    },
    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});


describe('angle', {
    'fixnums': function() {
	// FIXME: we're missing this
    },
    'bignums': function() {
	// FIXME: we're missing this
    },
    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});


describe('atan', {
    'fixnums': function() {
	// FIXME: we're missing this
    },
    'bignums': function() {
	// FIXME: we're missing this
    },
    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});


describe('cos', {
    'fixnums': function() {
	// FIXME: we're missing this
    },
    'bignums': function() {
	// FIXME: we're missing this
    },
    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});


describe('sin', {
    'fixnums': function() {
	// FIXME: we're missing this
    },
    'bignums': function() {
	// FIXME: we're missing this
    },
    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});

describe('tan', {
    'fixnums': function() {
	// FIXME: we're missing this
    },
    'bignums': function() {
	// FIXME: we're missing this
    },
    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});


describe('acos', {
    'fixnums': function() {
	// FIXME: we're missing this
    },
    'bignums': function() {
	// FIXME: we're missing this
    },
    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});


describe('asin', {
    'fixnums': function() {
	// FIXME: we're missing this
    },
    'bignums': function() {
	// FIXME: we're missing this
    },
    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});


describe('cosh', {
    'fixnums': function() {
	// FIXME: we're missing this
    },
    'bignums': function() {
	// FIXME: we're missing this
    },
    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});


describe('sinh', {
    'fixnums': function() {
	// FIXME: we're missing this
    },
    'bignums': function() {
	// FIXME: we're missing this
    },
    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});




describe('imaginaryPart', {
    'fixnums': function() {
	// FIXME: we're missing this
    },
    'bignums': function() {
	// FIXME: we're missing this
    },
    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});


describe('realPart', {
    'fixnums': function() {
	// FIXME: we're missing this
    },
    'bignums': function() {
	// FIXME: we're missing this
    },
    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});


describe('round', {
    'fixnums': function() {
	// FIXME: we're missing this
    },
    'bignums': function() {
	// FIXME: we're missing this
    },
    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});


describe('exp', {
    'fixnums': function() {
	// FIXME: add test case where value needs to become a bignum.
    },

    'bignums': function() {
	// FIXME: we're missing this
    },
    'rationals': function() {
	// FIXME: we're missing this
    },
    'floats': function() {
	// FIXME: we're missing this
    },
    'complex': function() {
	// FIXME: we're missing this
    }
});


describe('sqr', {
    'fixnums': function() {
	// FIXME: add test case where value needs to become a bignum.
	assertTrue(eqv(sqr(42), 1764));
	assertTrue(eqv(sqr(0), 0));
	assertTrue(eqv(sqr(1), 1));
	assertTrue(eqv(sqr(2), 4));
	assertTrue(eqv(sqr(-1), 1));
	assertTrue(eqv(sqr(-2), 4));
	assertTrue(eqv(sqr(-12349), 152497801));
    },

    'fixnum overflows to bignum': function() {
	var x = fromFixnum(2);
	for(var i = 0; i < 10; i++) {
	    x = sqr(x);
	}
	// Basically, computing (expt 2 (expt 2 10))
	assertTrue(eqv(x, makeBignum("179769313486231590772930519078902473361797697894230657273430081157732675805500963132708477322407536021120113879871393357658789768814416622492847430639474124377767893424865485276302219601246094119453082952085005768838150682342462881473913110540827237163350510684586298239947245938479716304835356329624224137216")));
    },

    'bignums': function() {
	assertTrue(eqv(sqr(makeBignum("1297684398542133568912839")),
		       makeBignum("1683984798219658952314406790914015952992379039921")));

	assertTrue(eqv(sqr(makeBignum("-1297684398542133568912839")),
		       makeBignum("1683984798219658952314406790914015952992379039921")));
    },

    'rationals': function() {
	assertTrue(eqv(sqr(makeRational(1, 2)),
		       makeRational(1, 4)));

	assertTrue(eqv(sqr(makeRational(-1, 7)),
		       makeRational(1, 49)));
	assertTrue(eqv(sqr(makeRational(makeBignum("-1297684398542133568912839"),
					5)),
		       makeRational(makeBignum("1683984798219658952314406790914015952992379039921"),
				    25)));

    },
    'floats': function() {
	assertTrue(eqv(sqr(makeFloat(0.0)),
		       makeFloat(0.0)));
	assertTrue(eqv(sqr(makeFloat(.25)),
		       makeFloat(0.0625)));
	assertTrue(eqv(sqr(makeFloat(-.25)),
		       makeFloat(0.0625)));


	assertTrue(eqv(sqr(nan), nan));
	assertTrue(eqv(sqr(inf), inf));
	assertTrue(eqv(sqr(negative_inf), inf));
	assertTrue(eqv(sqr(negative_zero), makeFloat(0.0)));
    },

    'complex': function() {
	assertTrue(eqv(sqr(negative_i),
		       -1));
	assertTrue(eqv(sqr(i),
		       -1));
    }
});


describe('gcd', {
    'fixnum / fixnum' : function() {
	// FIXME: we're missing this
    },
    'fixnum / bignum': function() {
	// FIXME: we're missing this
    },

    'bignum / bignum' : function() {
	// FIXME: we're missing this
    },

    'bignum / rational': function() {
	// FIXME: we're missing this
    },

    'bignum / float' : function() {
	// FIXME: we're missing this
    },

    'bignum / complex' : function() {
	// FIXME: we're missing this
    },
    'fixnum / rational' : function() {
	// FIXME: we're missing this
    },
    'fixnum / floating' : function() {
	// FIXME: we're missing this
    },
    'fixnum / complex' : function() {
	// FIXME: we're missing this
    },
    'rational / rational' : function() {
	// FIXME: we're missing this
    },
    'rational / floating' : function() {
	// FIXME: we're missing this
    },
    'rational / complex' : function() {
	// FIXME: we're missing this
    },
    'floating / floating' : function() {
	// FIXME: we're missing this
    },
    'floating / complex' : function() {
	// FIXME: we're missing this
    },
    'complex / complex' : function() {
	// FIXME: we're missing this
    }
});




describe('lcm', {
    'fixnum / fixnum' : function() {
	// FIXME: add test case where value needs to become a bignum.
    },
    'fixnum / bignum': function() {
	// FIXME: we're missing this
    },

    'bignum / bignum' : function() {
	// FIXME: we're missing this
    },

    'bignum / rational': function() {
	// FIXME: we're missing this
    },

    'bignum / float' : function() {
	// FIXME: we're missing this
    },

    'bignum / complex' : function() {
	// FIXME: we're missing this
    },
    'fixnum / rational' : function() {
	// FIXME: we're missing this
    },
    'fixnum / floating' : function() {
	// FIXME: we're missing this
    },
    'fixnum / complex' : function() {
	// FIXME: we're missing this
    },
    'rational / rational' : function() {
	// FIXME: we're missing this
    },
    'rational / floating' : function() {
	// FIXME: we're missing this
    },
    'rational / complex' : function() {
	// FIXME: we're missing this
    },
    'floating / floating' : function() {
	// FIXME: we're missing this
    },
    'floating / complex' : function() {
	// FIXME: we're missing this
    },
    'complex / complex' : function() {
	// FIXME: we're missing this
    }
});


describe('integerSqrt', {
    'fixnums': function() {
	assertEquals(0, integerSqrt(0));
        assertEquals(makeComplex(0,1), integerSqrt(-1));
        assertEquals(makeComplex(0,11096), integerSqrt(-123123123));  
        assertEquals(0, integerSqrt(-0));
        assertEquals(0, integerSqrt(0));
        assertEquals(1, integerSqrt(1));
        assertEquals(2, integerSqrt(4));
        assertEquals(2, integerSqrt(5));
        assertEquals(14, integerSqrt(200));
        assertEquals(5000000000, integerSqrt(25000000005000000000));
    },

    'bignums': function() {        
        assertTrue(eqv(makeComplex(0, makeBignum("1")),
		       integerSqrt(makeBignum("-1"))));
        assertTrue(eqv(makeBignum("0"),
		       integerSqrt(makeBignum("0"))));
        assertTrue(eqv(makeBignum("1"),
		       integerSqrt(makeBignum("1"))));
        assertTrue(eqv(makeBignum("2"),
		       integerSqrt(makeBignum("4"))));
        assertTrue(eqv(makeBignum("2"),
		       integerSqrt(makeBignum("5"))));
        assertTrue(eqv(makeBignum("2"),
		       integerSqrt(makeBignum("6"))));
        assertTrue(eqv(makeBignum("2"),
		       integerSqrt(makeBignum("7"))));
        assertTrue(eqv(makeBignum("8"),
		       integerSqrt(makeBignum("70"))));
        assertTrue(eqv(makeBignum("26"), 
		       integerSqrt(makeBignum("700"))));
        assertTrue(eqv(makeBignum("92113"),
		       integerSqrt(makeBignum("8484848484"))));
        assertTrue(eqv(makeBignum("35136418"),
		       integerSqrt(makeBignum("1234567891234567"))));
        assertTrue(eqv(makeBignum("50000000000"),
		       integerSqrt(makeBignum("2500000000050000000000"))));
        assertTrue(eqv(makeBignum("999999999949999"), 
		       integerSqrt(makeBignum("999999999900000000000000000000"))));
        assertTrue(eqv(makeComplex(0, makeBignum("92113")),
		       integerSqrt(makeBignum("-8484848484"))));
        assertTrue(eqv(makeComplex(0, makeBignum("35136418")), 
		       integerSqrt(makeBignum("-1234567891234567"))));
        assertTrue(eqv(makeComplex(0, makeBignum("50000000000")),
		       integerSqrt(makeBignum("-2500000000050000000000"))));
        assertTrue(eqv(makeComplex(0, makeBignum("999999999949999")),
		       integerSqrt(makeBignum("-999999999900000000000000000000"))));
    },

    'rationals': function() {
        assertFails(function() { integerSqrt(makeRational(makeBignum("1"),makeBignum("4"))) });
        assertFails(function() { integerSqrt(makeRational(makeBignum("1"),makeBignum("5"))) });
        assertFails(function() { integerSqrt(makeRational(makeBignum("5"),makeBignum("2"))) });
       
        assertTrue(eqv(makeBignum("0"), 
		       integerSqrt(makeRational(makeBignum("0"),makeBignum("-1")))));
        assertTrue(eqv(makeBignum("2"), 
		       integerSqrt(makeRational(makeBignum("55555555555"),
						makeBignum("11111111111")))));
        assertTrue(eqv(makeBignum("5000000000"), 
		       integerSqrt(makeRational(makeBignum("25000000005000000000"),
						makeBignum("1")))));
        
        assertTrue(eqv(makeComplex(0, makeBignum("5000000000")), 
		       integerSqrt(makeRational(makeBignum("-25000000005000000000"),
						makeBignum("1")))));
    },

    'floats': function() {
        assertFails(function() { integerSqrt(nan) });
        assertFails(function() { integerSqrt(inf) });
        assertFails(function() { integerSqrt(negative_inf) });
        assertFails(function() { integerSqrt(makeFloat(0.1))});
        assertFails(function() { integerSqrt(makeFloat(-0.1))});
        assertFails(function() { integerSqrt(makeFloat(9999999999.000001))});	
        assertFails(function() { integerSqrt(makeFloat(negative_zero))});
        assertTrue(eqv(makeFloat(0.0),
		       integerSqrt(makeFloat(0.0))));
	assertTrue(eqv(makeFloat(1.0),
		       integerSqrt(makeFloat(1.0))));
        assertTrue(eqv(makeFloat(111.0),
 		       integerSqrt(makeFloat(12345.0))));
        assertTrue(eqv(makeComplex(0, makeFloat(111)), 
 		       integerSqrt(makeFloat(-12345.0))));
	assertTrue(eqv(integerSqrt(negative_zero),
		       negative_zero));
    },

    'complex': function() {
        assertFails(function() { integerSqrt(makeComplex(nan, 0))});
        assertFails(function() { integerSqrt(makeComplex(0, nan))});
        assertFails(function() { integerSqrt(makeComplex(nan, 1))});
        assertFails(function() { integerSqrt(makeComplex(1, nan))});
        assertFails(function() { integerSqrt(makeComplex(nan, inf))});
        assertFails(function() { integerSqrt(makeComplex(inf, nan))});
        assertFails(function() { integerSqrt(makeComplex(nan, negative_zero))});
        assertFails(function() { integerSqrt(makeComplex(negative_zero, nan))});
        assertFails(function() { integerSqrt(makeComplex(nan, nan))});
        assertFails(function() { integerSqrt(makeComplex(inf,inf))});
        assertFails(function() { integerSqrt(makeComplex(0,inf))});
        assertFails(function() { integerSqrt(makeComplex(inf,0))});
        assertFails(function() { integerSqrt(makeComplex(negative_inf,
							 negative_inf))});
        assertFails(function() { integerSqrt(makeComplex(makeBignum("0"),
							 makeBignum("2")))});
        assertFails(function() { integerSqrt(makeComplex(makeBignum("1"),
							 makeBignum("2")))});
        assertFails(function() { integerSqrt(makeComplex(makeRational(makeBignum("9919"),
								      makeBignum("9")),
							 0))});
        assertFails(function() { integerSqrt(makeComplex(makeRational(makeBignum("9919"),
								      makeBignum("9")),
							 makeBignum("200")))});
        assertFails(function() { integerSqrt(makeComplex(makeFloat(4.25),
							 makeRational(3,2)))});
        
        assertTrue(eqv(makeComplex(0,makeBignum("1")),
		       integerSqrt(makeComplex(makeBignum("-1"),
					       makeBignum("0")))));
        assertTrue(eqv(makeBignum("1"), 
		       integerSqrt(makeComplex(makeBignum("1"),
					       makeBignum("0")))));
        assertTrue(eqv(makeComplex(0, makeBignum("2")),
		       integerSqrt(makeComplex(makeBignum("-7"),
					       makeBignum("0"))))); 
        assertTrue(eqv(makeBignum("351"), 
		       integerSqrt(makeComplex(makeBignum("123456"),
					       makeBignum("0")))));
        assertTrue(eqv(makeComplex(0,makeBignum("351")),
		       integerSqrt(makeComplex(makeBignum("-123456"),
					       makeBignum("0")))));
    }
});

describe('toString', {
    'fixnums': function() {
	assertEquals("-123456789012345678901234567890",
		     makeBignum("-123456789012345678901234567890").toString());
	assertEquals("123456789012345678901234567890",
		     makeBignum("123456789012345678901234567890").toString());
    },
    'bignums': function() {
	assertEquals("0",
		     makeBignum("0").toString());
	assertEquals("-236579329853268932967423894623849289323568268954",
		     makeBignum("-236579329853268932967423894623849289323568268954").toString());
	assertEquals("312981532653268913529653216935216851268932416843269129356216894159681236850432163527231684321782317892513672317605612",
		     makeBignum("312981532653268913529653216935216851268932416843269129356216894159681236850432163527231684321782317892513672317605612").toString());
    },
    'rationals': function() {
	assertEquals("1/2",
		     makeRational(1, 2));

	assertEquals("2398742368955236823956823968/239856325892398441",
		     makeRational(makeBignum("2398742368955236823956823968"),
				  makeBignum("239856325892398441")));

	assertEquals("-2398742368955236823956823968/239856325892398441",
		     makeRational(makeBignum("-2398742368955236823956823968"),
				  makeBignum("239856325892398441")));
    },
    'floats': function() {
	assertEquals('0.0', makeFloat(0).toString());
	assertEquals('0.25', makeFloat(0.25).toString());
	assertEquals('1.2354e+200', makeFloat(1.2354e200).toString());
	assertEquals('1.2354e-200', makeFloat(1.2354e-200).toString());
	assertEquals('-1.2354e-200', makeFloat(-1.2354e-200).toString());
	assertEquals('-1.0', makeFloat(-1).toString());
	assertEquals("+nan.0", nan.toString());
	assertEquals("+inf.0", inf.toString());
	assertEquals("-inf.0", negative_inf.toString());
	assertEquals("-0.0", negative_zero.toString());

    },
    'complex': function() {
	assertEquals("1.0+0.0i", makeComplex(1, makeFloat(0)).toString());
	assertEquals("-1.0+0.0i", makeComplex(-1, makeFloat(0)).toString());
	assertEquals("0+1i", makeComplex(0, 1).toString());
	assertEquals("0-1i", makeComplex(0, -1).toString());
	assertEquals("0.0-0.0i", makeComplex(0, negative_zero).toString());
	assertEquals("3/4+5/6i", makeComplex(makeRational(3, 4),
					     makeRational(5, 6)).toString());
	assertEquals("3/4-5/6i", makeComplex(makeRational(3, 4),
					     makeRational(-5, 6)).toString());
	assertEquals("0.1-inf.0i", makeComplex(makeFloat(0.1),
					       negative_inf).toString());
	assertEquals("+nan.0+inf.0i", makeComplex(nan, inf).toString());
	assertEquals("+nan.0+nan.0i", makeComplex(nan, nan).toString());
	assertEquals("-inf.0-inf.0i", makeComplex(negative_inf, negative_inf).toString());
    }
});





describe('repeating decimals', {
    tests: function() {
	assertEquals(['1', '', '0'], toRepeatingDecimal(1, 1));
	assertEquals(['0', '5', '0'], toRepeatingDecimal(1, 2));
	assertEquals(['0', '', '3'], toRepeatingDecimal(1, 3));
	assertEquals(['0', '25', '0'], toRepeatingDecimal(1, 4));
	assertEquals(['0', '2', '0'], toRepeatingDecimal(1, 5));
	assertEquals(['0', '1', '6'], toRepeatingDecimal(1, 6));
	assertEquals(['0', '', '142857'], toRepeatingDecimal(1, 7));
	assertEquals(['0', '125', '0'], toRepeatingDecimal(1, 8));
	assertEquals(['0', '', '1'], toRepeatingDecimal(1, 9));
	assertEquals(['0', '1', '0'], toRepeatingDecimal(1, 10));
	assertEquals(['0', '', '09'], toRepeatingDecimal(1, 11));
	assertEquals(['0', '08', '3'], toRepeatingDecimal(1, 12));
	assertEquals(['0', '', '076923'], toRepeatingDecimal(1, 13));
	assertEquals(['0', '0', '714285'], toRepeatingDecimal(1, 14));
	assertEquals(['0', '0', '6'], toRepeatingDecimal(1, 15));
	assertEquals(['0', '0625', '0'], toRepeatingDecimal(1, 16));
	assertEquals(['0', '', '0588235294117647'], toRepeatingDecimal(1, 17));
	assertEquals(['5', '8', '144'], toRepeatingDecimal(3227, 555));
    },
    
    limitRendering: function() {
	var OPTIONS = {limit: 5};
	assertEquals(['1', '', '0'], toRepeatingDecimal(1, 1, OPTIONS));
	assertEquals(['0', '5', '0'], toRepeatingDecimal(1, 2, OPTIONS));
	assertEquals(['0', '', '3'], toRepeatingDecimal(1, 3, OPTIONS));
	assertEquals(['0', '25', '0'], toRepeatingDecimal(1, 4, OPTIONS));
	assertEquals(['0', '2', '0'], toRepeatingDecimal(1, 5, OPTIONS));
	assertEquals(['0', '1', '6'], toRepeatingDecimal(1, 6, OPTIONS));
	assertEquals(['0', '05882', '...'], 
		     toRepeatingDecimal(1, 17, {limit : 5}));

	assertEquals(['0', '125', '0'], toRepeatingDecimal(1, 8, {limit : 4}));
	assertEquals(['0', '125', '...'], toRepeatingDecimal(1, 8, {limit : 3}));
	assertEquals(['0', '12', '...'], toRepeatingDecimal(1, 8, {limit : 2}));
    	assertEquals(['0', '1', '...'], toRepeatingDecimal(1, 8, {limit : 1}));
    	assertEquals(['0', '', '...'], toRepeatingDecimal(1, 8, {limit : 0}));

	assertEquals(['10012718086',
		      '8577149703838870007766167',
		      '...'],
		     toRepeatingDecimal(makeBignum("239872983562893234879"),
					makeBignum("23956829852"),
					{limit:25}));

    }
    
});







describe('old tests from Moby Scheme', {
    testRationalReduction: function() {
	var n1 = makeRational(1,2);
	var n2 = makeRational(5, 10);
	var n3 = makeRational(5, 12);
	assertTrue(equals(n1, n2));
	assertTrue(! equals(n2, n3));
    },

    testEqv: function() {
	assertTrue(eqv(nan, nan));
	assertTrue(false == eqv(makeFloat(42),
				makeRational(42)));
	assertTrue(eqv(inf, inf));
	assertTrue(eqv(negative_inf, negative_inf));
    },
    


    testEqual: function(){
	var n1 = makeRational(2,1);
	var n2 = makeFloat(2.0);
	assertTrue(equals(n1, n2));
	
	var n3 = makeComplex(makeRational(2),makeRational(0));
	var n4 = makeComplex(makeRational(2),makeRational(1));
	assertTrue(equals(n1, n3));
	assertTrue(!equals(n3, n4));

	assertTrue(equals(makeRational(1, 2), makeRational(2, 4))); 

	assertTrue(false == equals(makeRational(1, 2),
				   makeRational(2, 5))); 


	assertTrue(false === equals(nan, nan));
	assertTrue(false === equals(nan, makeRational(3)));
	assertTrue(false === equals(makeRational(3), nan));

    },
    
    testAbs : function(){
	var n1 = makeRational(-2,1);
	var n2 = makeRational(4, 2);
	var n3 = makeComplex(makeRational(2),
				      makeRational(0));
	assertTrue(equals(abs(n1), n2));
	assertTrue(equals(abs(n3), n2));
    },
    
    testAdd : function(){
	assertTrue(equals(add(makeRational(2,1), makeRational(3,1)), 
			  makeRational(5,1)));
	assertTrue(equals(add(makeRational(2,1), makeFloat(2.1)), makeFloat(4.1)));
	assertTrue(equals(add(makeRational(2,1),
			      makeComplex(makeRational(2), makeRational(2))),
			  makeComplex(makeRational(4),makeRational(2))));
	assertTrue(equals(add(makeFloat(3.1), makeComplex(makeRational(2),makeRational(2))),
			  makeComplex(makeFloat(5.1), makeRational(2))));
	assertTrue(equals(add(makeComplex(makeRational(2),makeRational( 2)), makeComplex(makeRational(3),makeRational( 2))), makeComplex(makeRational(5),makeRational( 4))));
    },
    
    testDivisionByZero: function() {
	divide(1, 1);
	assertFails(function() {
	    divide(1, 0);
	});
	assertFails(function() { divide(makeFloat(1), 0); });
	assertEqv(divide(makeFloat(1),makeFloat(0)),
		  inf);
    },
    

    testAddFloats: function() {
	assertEquals(0.1, add(makeRational(0),
 				  makeFloat(0.1)).toFixnum());
    },

    
    testSubtract : function(){
	assertTrue(equals(subtract(subtract(0, makeRational(2,1)),
				   makeRational(3,1)),
			  makeRational(-5,1)));		
	assertTrue(equals(subtract(subtract(0, makeRational(2,1)), makeFloat(2.1)),
			  makeFloat(-4.1)));
	assertTrue(equals(subtract(subtract(0, makeRational(2,1)), makeComplex(makeRational(2),makeRational( 2))),
			  makeComplex(makeRational(-4),makeRational( -2))));
	assertTrue(equals(subtract(subtract(0, makeFloat(2.1)),
				   makeComplex(makeRational(2),makeRational( 2))),
			  makeComplex(makeFloat(-4.1),makeRational( -2))));
	assertTrue(equals(subtract(subtract(0, makeComplex(makeRational(2),makeRational( 2))), makeComplex(makeRational(3),makeRational( 2))),
			  makeComplex(makeRational(-5),makeRational( -4))));	
    },
    
    testMultiply : function(){
	assertTrue(equals(multiply(makeRational(2,1), makeRational(3,1)), makeRational(6,1)));
	assertTrue(equals(multiply(makeRational(2,1), makeFloat(2.1)), makeFloat(4.2)));
	assertTrue(equals(multiply(makeRational(2,1), makeComplex(makeRational(2),makeRational( 2))), makeComplex(makeRational(4),makeRational(4))));
	assertTrue(equals(multiply(makeFloat(2.1), makeComplex(makeRational(2),makeRational( 2))), makeComplex(makeFloat(4.2),makeFloat( 4.2))));
	assertTrue(equals(multiply(makeComplex(makeRational(2),makeRational( 2)), makeComplex(makeRational(3),makeRational( 2))), makeComplex(makeRational(2),makeRational( 10))));
    },
    
    
    testDivide : function(){
	var six = makeRational(6, 1);
	assertTrue(equals(divide(divide(six, makeRational(2,1)), makeRational(3,1)), 1));
	assertTrue(equals(divide(divide(six, makeFloat(1.5)), makeFloat(4.0)), 1));
	assertTrue(equals(divide(divide(makeFloat(150), 
					makeComplex(makeRational(3),makeRational( 4))),
				 makeComplex(makeRational(3),makeRational( -4))),
			  six));
	assertTrue(equals(divide(1, six),
			  makeRational(1, 6)));
    },
    
    
    testConjugate : function(){
	var n1 = makeRational(2,1);
	var n2 = makeFloat(2.1);
	assertTrue(equals(n1, conjugate(n1)));
	assertTrue(equals(n2, conjugate(n2)));
	assertTrue(equals(makeComplex(makeRational(1),makeRational( 2)), conjugate(makeComplex(makeRational(1),makeRational( -2)))));
    },
    
    testMagnitude : function(){
	var n1 = makeRational(2,1);
	var n2 = makeFloat(2.1);
	assertTrue(equals(n1, magnitude(n1)));
	assertTrue(equals(n2, magnitude(n2)));
	assertTrue(equals(makeComplex(makeRational(5),makeRational( 0)), magnitude(makeComplex(makeRational(3),makeRational( -4)))));
    },
    
    testComparison : function(){	
	assertTrue(greaterThan(makeRational(2,1),
			       makeRational(1,1)));
	assertTrue(greaterThan(makeFloat(2.1),
			       makeRational(2,1)));
	assertTrue(greaterThanOrEqual(makeFloat(2.0),
				      makeRational(2,1)));
	assertTrue(greaterThanOrEqual(makeComplex(makeFloat(2.0),makeRational( 0)),
				      makeRational(2,1)));


	assertTrue(lessThan(makeRational(2),
				      makeRational(3)));

	assertTrue(! lessThan(makeRational(3),
					makeRational(2)));
    },


    testComparisonMore: function() {
	assertTrue(! greaterThan(makeRational(2),
					   makeRational(3)));

	assertTrue(greaterThan(makeRational(3),
					 makeRational(2)));

	assertTrue(! greaterThan(makeRational(3),
					   makeRational(3)));

	assertTrue(lessThanOrEqual(makeRational(17),
				   makeRational(17)));

	assertTrue(lessThanOrEqual(makeRational(16),
				   makeRational(17)));

	assertTrue(!lessThanOrEqual(makeRational(16),
					      makeRational(15)));
    },

    
    testComparison2 : function () {
	var num = makeRational(0, 1);
	var upper = makeRational(480, 1);

	assertTrue(lessThan(makeRational(5, 1),
				      upper));
	assertTrue(lessThan(makeRational(6, 1),
				      upper));
	assertTrue(lessThan(makeRational(7, 1),
				      upper));
	assertTrue(lessThan(makeRational(8, 1),
				      upper));
	assertTrue(lessThan(makeRational(9, 1),
				      upper));

	for (var i = 0; i < 60; i++) {
	    assertTrue(lessThan
			(num, upper));
	    num = add(num, 1);
	}
    },

    
    testAtan : function(){
	assertTrue(equals(atan(1), divide(pi, 4)));
    },
    
    testLog : function(){
	assertTrue(equals(log(1), 0));		
	assertTrue(equals(log(makeComplex(makeRational(0),makeRational(1))), divide(multiply(pi, i), 2)));
	assertTrue(equals(log(makeFloat(-1)), multiply(pi, i)));
    },
    
    testAngle : function(){
	assertTrue(equals(angle(makeComplex(makeRational(0),makeRational(1))), divide(pi, 2)));
	assertTrue(equals(angle(makeComplex(makeRational(1),makeRational(1))), divide(pi, 4)));
	assertTrue(equals(angle(makeFloat(-1)), pi));
	assertTrue(equals(angle(makeComplex(makeRational(-1),makeRational( 1))), 
			  multiply(pi, makeFloat(0.75))));
	assertTrue(equals(angle(makeComplex(makeRational(-1),makeRational( -1))),
			  multiply(pi, makeFloat(-0.75))));
	assertTrue(equals(angle(makeComplex(makeRational(1),makeRational( -1))), 
			  multiply(pi, makeRational(-1, 4))));
    },
    
    testExp : function(){
	assertTrue(equals(exp(0), 1));
	assertTrue(equals(exp(1),
				   e));
	assertTrue(approxEquals(exp(makeRational(2)), 
					  sqr(e),
					  makeFloat(0.0001)));
    },
    
    
    testExpt : function(){
	var i = makeComplex(
	    makeRational(0),makeRational( 1));

	assertTrue(equals(
	    expt(i, i), 
	    exp(multiply(pi, makeRational(-1, 2)))));

 	assertTrue(equals(
 	    expt(makeFloat(2), 
 			makeFloat(3)), 
 	    makeFloat(8)));
	
 	assertTrue(equals(expt(makeComplex(makeRational(3,4),
 					   makeRational(7,8)),
 			       makeRational(2)),
 			  makeComplex(makeRational(-13, 64),
 				      makeRational(21, 16))));
    },
    
    
    testSin : function(){
	assertTrue(equals(sin(divide(pi, makeFloat(2))), 1));
    },
    
    testCos : function(){
	assertTrue(equals(cos(0), 1));
    },
    

    testSqr: function() {
	var n1 = makeRational(42);
	assertEquals(1764, toFixnum(sqr(n1)));
    },

    testIntegerSqrt: function() {
	var n1 = makeRational(36);
	var n2 = makeRational(6);
	
	assertEquals(n2, integerSqrt(n1));
	assertFails(function() { integerSqrt(makeFloat(3.5)); }); 
    },


    testSqrt : function(){
	assertTrue(equals(sqrt(makeFloat(4)), makeFloat(2)));
	assertTrue(equals(sqrt(makeFloat(-1)), makeComplex(makeRational(0),makeRational(1))));
    },
    
    testAcos : function(){
	assertTrue(equals(acos(1), 0));
	assertTrue(equals(acos(makeFloat(-1)), pi));
    },
    
    testAsin : function(){
	assertTrue(eqv(asin(0), 0));
 	assertTrue(equals(asin(-1),
 			  multiply(pi, makeRational(-1, 2))));
  	assertTrue(equals(asin(1),
  			  divide(pi, 2)));
  	assertTrue(equals(asin(makeRational(1, 4)),
  			  makeFloat(0.25268025514207865)));	
  	assertTrue(diffPercent(realPart(asin(makeComplex(1, 5))),
  			       makeFloat(0.1937931365549321))
		   < 1e-2);
   	assertTrue(diffPercent(imaginaryPart(asin(makeComplex(1, 5))),
   			       makeFloat(2.3309746530493123))
		   < 1e-2);
    },
    
    testTan : function(){
	assertTrue(equals(tan(0), 0));
    },
    
    testComplex_question_ : function(){
	assertTrue(isSchemeNumber(pi));
	assertTrue(isSchemeNumber(1));
	assertTrue(isSchemeNumber(makeFloat(2.718)));
	assertTrue(isSchemeNumber(makeComplex(0,1)));
    },



    testMakePolar : function() {
	assertTrue(equals(makeComplexPolar(makeRational(5),
					   makeRational(0)),
			  makeComplex(makeRational(5),
				      makeRational(0))));
	var n = makeComplexPolar(makeRational(5),
				       pi);
	var delta = makeFloat(0.0000001);
	assertTrue(approxEquals(imaginaryPart(n),
				makeRational(0),
				delta));
	assertTrue(approxEquals(realPart(n),
				makeRational(-5),
				delta));
    },
    
    testCosh : function(){
	assertTrue(equals(cosh(0), 1));
    },
    
    testSinh : function(){
	assertTrue(equals(sinh(0), 0));
    },
    
    testDenominator : function(){
	assertTrue(equals(denominator(makeRational(7,2)),
			  makeRational(2,1)));
	assertTrue(equals(denominator(makeFloat(3)),
			  makeFloat(1)));
    },
    
    testNumerator : function(){
	assertTrue(equals(numerator(makeRational(7,2)),
			  makeRational(7,1)));
	assertTrue(equals(numerator(makeFloat(3)),
			  makeFloat(3)));
    },


    testIsExact : function() {
	assertTrue(isExact(makeRational(3)));
	assertTrue(! isExact(makeFloat(3.0)));
	assertTrue(! isExact(makeFloat(3.5)));
    },


    testExactToInexact : function() {
	assertTrue(eqv(toInexact(makeRational(3)),
		       makeFloat(3.0)));
	assertTrue(isInexact(toInexact(makeRational(3))));
    },


    testInexactToExact : function() {
	assertTrue(equals(toExact(makeFloat(3)),
			  makeRational(3)));
	assertTrue(isExact(toExact(makeFloat(3))));
    },

    testFloatsAreInexact: function() {
	assertTrue(! isExact(makeFloat(3.0)));
    },

    
//     testOdd_question_ : function(){
// 	assertTrue(Kernel.odd_question_(1));
// 	assertTrue(! Kernel.odd_question_(0));
// 	assertTrue(Kernel.odd_question_(makeFloat(1)));
// 	assertTrue(Kernel.odd_question_(makeComplex(makeRational(1),makeRational( 0))));
// 	assertTrue(Kernel.odd_question_(makeRational(-1, 1)));
//     },
    
    testInfinityComputations : function() {
	assertTrue(equals(0, multiply(0, inf)));
    },

//     testEven_question_ : function(){
// 	assertTrue(Kernel.even_question_(0));
// 	assertTrue(! Kernel.even_question_(1));
// 	assertTrue(Kernel.even_question_(makeFloat(2)));
// 	assertTrue(Kernel.even_question_(makeComplex(makeRational(2),makeRational( 0))));
//     },
    
//     testPositive_question_ : function(){
// 	assertTrue(Kernel.positive_question_(1));
// 	assertTrue(!Kernel.positive_question_(0));
// 	assertTrue(Kernel.positive_question_(makeFloat(1.1)));
// 	assertTrue(Kernel.positive_question_(makeComplex(makeRational(1),makeRational(0))));
//     },
    
//     testNegative_question_ : function(){
// 	assertTrue(Kernel.negative_question_(makeRational(-5)));
// 	assertTrue(!Kernel.negative_question_(1));
// 	assertTrue(!Kernel.negative_question_(0));
// 	assertTrue(!Kernel.negative_question_(makeFloat(1.1)));
// 	assertTrue(!Kernel.negative_question_(makeComplex(makeRational(1),makeRational(0))));
//     },
    
    testCeiling : function(){
	assertTrue(equals(ceiling(1), 1));
	assertTrue(equals(ceiling(pi), makeFloat(4)));
	assertTrue(equals(ceiling(makeComplex(makeFloat(3.1),
					      makeRational(0))),
			  makeFloat(4)));
    },
    
    testFloor : function(){
	assertTrue(equals(floor(1), 1));
	assertTrue(equals(floor(pi), makeFloat(3)));
	assertTrue(equals(floor(makeComplex(makeFloat(3.1),
					    makeRational(0))), 
			  makeFloat(3)));
    },
    
    testImag_dash_part : function(){
	assertTrue(equals(imaginaryPart(1), 0));
	assertTrue(equals(imaginaryPart(pi), 0));
	assertTrue(equals(imaginaryPart(makeComplex(makeRational(0),
						    makeRational(1))),
			  1));
    },
    
    testReal_dash_part : function(){
	assertTrue(equals(realPart(1), 1));
	assertTrue(equals(realPart(pi), pi));
	assertTrue(equals(realPart(makeComplex(makeRational(0),
					       makeRational(1))), 0));
    },
    
    testInteger_question_ : function(){
	assertTrue(isInteger(1));
	assertTrue(isInteger(makeFloat(3.0)));
	assertTrue(!isInteger(makeFloat(3.1)));
	assertTrue(isInteger(makeComplex(makeRational(3),makeRational(0))));
	assertTrue(!isInteger(makeComplex(makeFloat(3.1),makeRational(0))));
    },
    
//     testMake_dash_rectangular: function(){
// 	assertTrue(equals(makeComplex(1, 1), 
// 			  makeComplex(makeRational(1),makeRational(1))));
//     },
    
//     testMaxAndMin : function(){
// 	var n1 = makeFloat(-1);
// 	var n2 = 0;
// 	var n3 = 1;
// 	var n4 = makeComplex(makeRational(4),makeRational(0));
// 	assertTrue(equals(n4, max(n1, [n2,n3,n4])));
// 	assertTrue(equals(n1, min(n1, [n2,n3,n4])));

// 	var n5 = makeFloat(1.1);
// 	assertEquals(n5, Kernel.max(n1, [n2, n3, n5]));
// 	assertEquals(n1, Kernel.min(n2, [n3, n4, n5, n1]));
//     },

    testLcm : function () {
	assertTrue(equals(makeRational(12),
			  lcm(makeRational(1),
			      [makeRational(2),
			       makeRational(3), 
			       makeRational(4)])));
    },

    testGcd : function () {
	assertTrue(equals(makeRational(1),
			  gcd(makeRational(1),
			      [makeRational(2), 
			       makeRational(3),
			       makeRational(4)])));
	
	assertTrue(equals(makeRational(5),
			  gcd(makeRational(100),
			      [makeRational(5), 
			       makeRational(10),
			       makeRational(25)])));
    },


    testIsRational : function() {
	assertTrue(isRational(makeRational(42)));
	assertTrue(isRational(makeFloat(3.1415)));
	assertTrue(isRational(pi));
	assertFalse(isRational(nan));
	assertFalse(isRational(inf));
	assertFalse(isRational(negative_inf));
	assertTrue(! isRational("blah"));
    },	

    
    testNumberQuestion : function() {
	assertTrue(isSchemeNumber(makeRational(42)));
	assertTrue(isSchemeNumber(42));
	assertFalse(isSchemeNumber(false));
	assertFalse(isSchemeNumber("blah again"));
    },


    testNumber_dash__greaterthan_string : function(){
	assertTrue("1" === (1).toString());
	assertEquals("5.0+0.0i",
		     (makeComplex(5, makeFloat(0))).toString());
	assertEquals("5+1i",
		     (makeComplex(5, 1)).toString());
	assertEquals("4-2i",
		     (makeComplex(4, -2)).toString());
    },
    
    testQuotient : function(){
	assertTrue(equals(quotient(makeFloat(3), makeFloat(4)), 0));	
	assertTrue(equals(quotient(makeFloat(4), makeFloat(3)), 1));

	assertTrue(equals(
	    quotient(makeRational(-36),
		     makeRational(7)),
	    makeRational(-5)));


	assertTrue(equals(
	    quotient(makeRational(-36),
		     makeRational(-7)),
	    makeRational(5)));

	
	assertTrue(equals(
	    quotient(makeRational(36),
		     makeRational(-7)),
	    makeRational(-5)));


	assertTrue(equals(
	    quotient(makeRational(36),
		     makeRational(7)),
	    makeRational(5)));



	assertTrue(eqv(1, quotient(7, 5)));
    },

    
    testRemainder : function(){
	assertTrue(equals(remainder(makeFloat(3), makeFloat(4)),
			  makeFloat(3)));	
	assertTrue(equals(remainder(makeFloat(4), makeFloat(3)),
			  makeFloat(1)));
    },

    
    testModulo : function() {
	var n1 = makeRational(17);
	var n2 = makeRational(3);
	var n3 = makeRational(2);
	assertEquals(n3, modulo(n1, n2));
	assertEquals(n2, modulo(n2, n1));
	assertTrue(equals(
	    makeRational(-3), 
	    modulo(makeRational(13),
		   makeRational(-4))));

	assertTrue(equals(
	    makeRational(3), 
	    modulo(makeRational(-13),
		   makeRational(4))));

	assertTrue(equals(
	    makeRational(-1), 
	    modulo(makeRational(-13),
		   makeRational(-4))));


	assertTrue(equals(
	    makeRational(0), 
	    modulo(makeRational(4),
		   makeRational(-2))));
    },

    
    testReal_question_ : function(){
	assertTrue(isReal(pi));
	assertTrue(isReal(1));
	assertTrue(!isReal(makeComplex(makeRational(0),makeRational(1))));
	assertTrue(isReal(makeComplex(makeRational(1),makeRational(0))));
	assertTrue(!isReal("hi"));
    },
    
    testRound : function(){
	assertTrue(equals(round(makeFloat(3.499999)), 
			  makeFloat(3)));
	assertTrue(equals(round(makeFloat(3.5)), 
			  makeFloat(4)));
	assertTrue(equals(round(makeFloat(3.51)),
			  makeFloat(4)));
	assertTrue(equals(round(makeRational(3)),
			  makeRational(3)));
	
	assertTrue(equals(round(makeRational(17, 4)),
			  makeRational(4)));
	
	
	assertTrue(equals(round(makeRational(-17, 4)),
			  makeRational(-4)));
    },
    

//     testSgn : function(){
// 	assertTrue(equals(sgn(makeFloat(4)), 1));
// 	assertTrue(equals(sgn(makeFloat(-4)), makeRational(-1)));
// 	assertTrue(equals(sgn(0), 0));
//     },
   
 
//     testZero_question_ : function(){
// 	assertTrue(Kernel.zero_question_(0));
// 	assertTrue(!Kernel.zero_question_(1));
// 	assertTrue(Kernel.zero_question_(makeComplex(makeRational(0),makeRational(0))));
//     }


});

