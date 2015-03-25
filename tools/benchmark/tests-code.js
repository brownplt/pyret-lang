/*global jasmine, describe, it, runs, expect*/ //JSlint
var b = require('benchmark-pyret');
require('jasmine-node');

jasmine.getEnv().addReporter(new jasmine.ConsoleReporter(console.log));
var validProgram = '1';
var invalidProgram = '1 + true';
var longProgram = '';
var i;
for (i = 0; i < 1000; i++) {
  longProgram += '1 + ';
}
longProgram += '1';
debugger;

var PASSES = true;
var FAILS = false;

var benchmarks =
    [
      {program: validProgram, name: 'validProgram'},
      {program: invalidProgram, name: 'invalidProgram'}
    ];

var filename = 'auto-report-programs/0_empty.arr';

var benchmarkResults;
var runFileResults;

describe('checkResult', function () {
  it('returns true given a SuccessResult', function () {
    expect(b.test.testCheckResult(true)).toBe(true);
  });

  it('returns false given a FailureResult', function () {
    expect(b.test.testCheckResult(false)).toBe(false);
  });
});

describe('ensureSuccess', function () {
  it('passes on a valid program', function (done) {
    runs(function () {
      b.test.testEnsureSuccess(validProgram, {}, function (result) {
        expect(result).toBe(PASSES);
        done();
      });
    });
  });

  it('passes on a valid (long) program', function (done) {
    runs(function () {
      b.test.testEnsureSuccess(longProgram, {}, function (result) {
        expect(result).toBe(PASSES);
        done();
      });
    });
  });

  it('fails on an invalid program', function (done) {
    runs(function () {
      b.test.testEnsureSuccess(invalidProgram, {}, function (result) {
        expect(result).toBe(FAILS);
        done();
      });
    });
  });
});

describe('parsePyret', function () {
  it('passes on a valid program', function (done) {
    runs(function () {
      b.test.testDeferredFunction(validProgram, {}, 'parsePyret', function (result) {
        expect(result).toBe(PASSES);
        done();
      });
    });
  });

  it('passes on a valid (long) program', function (done) {
    runs(function () {
      b.test.testDeferredFunction(longProgram, {}, 'parsePyret', function (result) {
        expect(result).toBe(PASSES);
        done();
      });
    });
  });
});

describe('initializeGlobalRuntime', function () {
  it('sets a runtime to global.rt', function () {
    expect(b.test.testInitializeGlobalRuntime()).toBe(true);
  });
});

describe('loadParsedPyret', function () {
  it('passes on a valid program', function (done) {
    runs(function () {
      b.test.testDeferredFunction(validProgram, {}, 'loadParsedPyret', function (result) {
        expect(result).toBe(PASSES);
        done();
      });
    });
  });

  it('passes on a valid (long) program', function (done) {
    runs(function () {
      b.test.testDeferredFunction(longProgram, {}, 'loadParsedPyret', function (result) {
        expect(result).toBe(PASSES);
        done();
      });
    });
  });
});

describe('setup', function () {
  it('sets up parsed ast and loaded js module', function (done) {
    runs(function () {
      b.test.testSetup(validProgram, {}, function (result) {
        expect(result).toBe(true);
        done();
      });
    });
  });
});

describe('evalLoadedPyret', function () {
  it('passes on a valid program', function (done) {
    runs(function () {
      b.test.testDeferredFunction(validProgram, {}, 'evalLoadedPyret', function (result) {
        expect(result).toBe(PASSES);
        done();
      });
    });
  });

  it('passes on a valid (long) program', function (done) {
    runs(function () {
      b.test.testDeferredFunction(longProgram, {}, 'evalLoadedPyret', function (result) {
        expect(result).toBe(PASSES);
        done();
      });
    });
  });

  it('fails on an invalid program', function (done) {
    runs(function () {
      b.test.testDeferredFunction(invalidProgram, {}, 'evalLoadedPyret', function (result) {
        expect(result).toBe(FAILS);
        done();
      });
    });
  });
});

describe('createSuite', function () {
  it('creates an instance of Benchmark.Suite with the correct number of benchmarks', function () {
    expect(b.test.testCreateSuite()).toBe(true);
  });
});

describe('runBenchmarks', function () {
  it('returns array of objects with correct field types', function () {
    var isArray = (benchmarkResults instanceof Array);
    var fields = true;
    var i;
    for (i = 0; i < benchmarkResults.length; i++) {
      fields = fields
        && (typeof benchmarkResults[i].program === 'string')
        && (typeof benchmarkResults[i].name === 'string')
        && (typeof benchmarkResults[i].results === 'object')
        && (typeof benchmarkResults[i].success === 'boolean');
    }
    var passed = isArray && fields;

    expect(passed).toBe(true);
  });

  it('reports success for valid program, non-success for invalid program', function () {
    expect(benchmarkResults[0].success && !benchmarkResults[1].success).toBe(true);
  });

  it('reports proper results for a valid program', function () {
    var results = benchmarkResults[0].results;
    var passed = true;
    var name;
    for (name in results) {
      if (results.hasOwnProperty(name)) {
        passed = passed
          && (typeof results[name].hz === 'number')
          && (typeof results[name].rme === 'number')
          && (typeof results[name].samples === 'number');
      }
    }
    expect(passed).toBe(true);
  });
});

describe('runFile', function () {
  it('returns array of objects with correct field types', function () {
    var isArray = (runFileResults instanceof Array);
    var fields = true;
    var i;
    for (i = 0; i < runFileResults.length; i++) {
      fields = fields
        && (typeof runFileResults[i].program === 'string')
        && (typeof runFileResults[i].name === 'string')
        && (typeof runFileResults[i].results === 'object')
        && (typeof runFileResults[i].success === 'boolean')
        && (typeof runFileResults[i].options === 'object');
    }
    var passed = isArray && fields;

    expect(passed).toBe(true);
  });

  it('reports proper results for a valid program', function () {
    var results = runFileResults[0].results;
    var passed = true;
    var name;
    for (name in results) {
      if (results.hasOwnProperty(name)) {
        passed = passed
          && (typeof results[name].hz === 'number')
          && (typeof results[name].rme === 'number')
          && (typeof results[name].samples === 'number');
      }
    }
    expect(passed).toBe(true);
  });
});

console.log("Running tests of the benchmark-pyret framework...");
b.runBenchmarks(benchmarks, {}, false, function (r) {
  benchmarkResults = r;
  b.runFile(filename, {}, false, function (f) {
    runFileResults = f;
    jasmine.getEnv().execute();
  });
});

