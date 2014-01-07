_ = require('jasmine-node');
R = require('../runtime-anf.js').PYRET_ANF;

console.log(R);

describe("Basic values", function() {
  it("should have an n field on numbers", function() {
    var rt = R.makeRuntime();
    expect(rt.makeNumber(5).n).toEqual(5);
  });



});


