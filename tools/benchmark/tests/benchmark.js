/* global describe, expect, it, runs, beforeEach, afterEach, waitsFor */

var r = require("requirejs");
define(["../benchmark-pyret"], function(b){

  var validProgram = '1';
  var invalidProgram = '1 + true';
  var longProgram = '';
  var i;
  for (i = 0; i < 1000; i++) {
    longProgram += '1 + ';
  }
  longProgram += '1';

  var LONG_TIMEOUT = 30000;
  var DEBUG_LOG = false;
  var USE_CACHED = true;

  var benchmarks =
  [
  {program: validProgram, name: 'validProgram'},
  {program: invalidProgram, name: 'invalidProgram'}
  ];

  var filename = '../auto-report-programs/0_empty.arr';

  function performTest () {
    describe('checkResult', function () {
      it('should return true given a SuccessResult', function () {
        expect(b.test.testCheckResult(true)).toBe(true);
      });

      it('should return false given a FailureResult', function () {
        expect(b.test.testCheckResult(false)).toBe(false);
      });
    });

    describe('ensureSuccess', function () {
      it('should pass on a valid program', function (done) {
        runs(function () {
          b.test.testEnsureSuccess(validProgram, {}, function (result) {
            expect(result).toBe(true);
            done();
          });
        });
      });

      it('should pass on a valid (long) program', function (done) {
        runs(function () {
          b.test.testEnsureSuccess(longProgram, {}, function (result) {
            expect(result).toBe(true);
            done();
          });
        });
      });

      it('should fail on an invalid program', function (done) {
        runs(function () {
          b.test.testEnsureSuccess(invalidProgram, {}, function (result) {
            expect(result).toBe(false);
            done();
          });
        });
      });
    });

    describe('initializeGlobalRuntime', function () {
      it('should set a runtime to global.rt', function () {
        expect(b.test.testInitializeGlobalRuntime()).toBe(true);
      });
    });

    describe('setup', function () {
      it('should set up parsed ast and loaded js module', function (done) {
        runs(function () {
          b.test.testSetup(validProgram, {}, function (result) {
            expect(result).toBe(true);
            done();
          });
        });
      });
    });

    describe('parsePyret', function () {
      it('should run on a valid program', function (done) {
        runs(function () {
          b.test.testBenchmarkFunction(validProgram, {}, 'parsePyret', function (result) {
            expect(result).toBe(true);
            done();
          });
        });
      });

      it('should run on a valid (long) program', function (done) {
        runs(function () {
          b.test.testBenchmarkFunction(longProgram, {}, 'parsePyret', function (result) {
            expect(result).toBe(true);
            done();
          });
        });
      });
    });

    describe('loadParsedPyret', function () {
      it('should run on a valid program', function (done) {
        runs(function () {
          b.test.testBenchmarkFunction(validProgram, {}, 'loadParsedPyret', function (result) {
            expect(result).toBe(true);
            done();
          });
        });
      });

      it('should run on a valid (long) program', function (done) {
        runs(function () {
          b.test.testBenchmarkFunction(longProgram, {}, 'loadParsedPyret', function (result) {
            expect(result).toBe(true);
            done();
          });
        });
      });
    });

    describe('evalLoadedPyret', function () {
      it('should run on a valid program', function (done) {
        runs(function () {
          b.test.testBenchmarkFunction(validProgram, {}, 'evalLoadedPyret', function (result) {
            expect(result).toBe(true);
            done();
          });
        });
      });

      it('should run on a valid (long) program', function (done) {
        runs(function () {
          b.test.testBenchmarkFunction(longProgram, {}, 'evalLoadedPyret', function (result) {
            expect(result).toBe(true);
            done();
          });
        });
      });
    });

    describe('runEvalPyret', function () {
      it('should run on a valid program', function (done) {
        runs(function () {
          b.test.testBenchmarkFunction(validProgram, {}, 'runEvalPyret', function (result) {
            expect(result).toBe(true);
            done();
          });
        });
      });

      it('should run on a valid (long) program', function (done) {
        runs(function () {
          b.test.testBenchmarkFunction(longProgram, {}, 'runEvalPyret', function (result) {
            expect(result).toBe(true);
            done();
          });
        });
      });
    });

    describe('createSuite', function () {
      it('should create an instance of Benchmark.Suite with the correct number of benchmarks', function () {
        expect(b.test.testCreateSuite()).toBe(true);
      });
    });

    describe('runBenchmarks', function () {
      var benchmarkResults;
      var okFlag = false;

      beforeEach(function() {
        b.test.testRunBenchmarks(benchmarks, {}, DEBUG_LOG, USE_CACHED, function (r) {
          benchmarkResults = r;
          okFlag = true;
        });
      });

      afterEach(function() {
        okFlag = false;
      });

      it('should return an array of objects with correct field types', function (done) {
        waitsFor(function () {
         return okFlag; 
        }, "okFlag should be set after call to test function", LONG_TIMEOUT);
        runs(function () {
          var isArray = (benchmarkResults instanceof Array);
          var fields = true;
          var i;
          for (i = 0; i < benchmarkResults.length; i++) {
            fields = fields &&
            (typeof benchmarkResults[i].program === 'string') &&
            (typeof benchmarkResults[i].name === 'string') &&
            (typeof benchmarkResults[i].results === 'object') &&
            (typeof benchmarkResults[i].success === 'boolean');
          }
          var passed = isArray && fields;

          expect(passed).toBe(true);
          done();
        });
      });

      it('should report success for valid program', function (done) {
        waitsFor(function () {
         return okFlag; 
        }, "okFlag should be set after call to test function", LONG_TIMEOUT);
        runs(function () {
            expect(benchmarkResults[0].success).toBe(true);
            done();
          });
      });

      it('should report non-success for invalid program', function (done) {
        waitsFor(function () {
         return okFlag; 
        }, "okFlag should be set after call to test function", LONG_TIMEOUT);
        runs(function () {
            expect(!benchmarkResults[1].success).toBe(true);
            done();
          });
      });

      it('should report proper results for a valid program', function (done) {
        waitsFor(function () {
         return okFlag; 
        }, "okFlag should be set after call to test function", LONG_TIMEOUT);
        runs(function () {
            var results = benchmarkResults[0].results;
            var passed = true;
            var name;
            for (name in results) {
              if (results.hasOwnProperty(name)) {
                passed = passed &&
                (typeof results[name].hz === 'number') &&
                (typeof results[name].rme === 'number') &&
                (typeof results[name].samples === 'number');
              }
            }
            expect(passed).toBe(true);
            done();
          });
      });
    });

    describe('runFile', function () {
      var runFileResults;
      var okFlag = false;

      beforeEach(function() {
        b.test.testRunFile(filename, {}, DEBUG_LOG, USE_CACHED, function (r) {
          runFileResults = r;
          okFlag = true;
        });
      });

      afterEach(function() {
        okFlag = false;
      });

      it('should return an array of objects with correct field types', function (done) {

        waitsFor(function () {
         return okFlag; 
        }, "okFlag should be set after call to test function", LONG_TIMEOUT);

        runs(function () {          
          var isArray = (runFileResults instanceof Array);
          var fields = true;
          var i;
          for (i = 0; i < runFileResults.length; i++) {
            fields = fields &&
            (typeof runFileResults[i].program === 'string') &&
            (typeof runFileResults[i].name === 'string') &&
            (typeof runFileResults[i].results === 'object') &&
            (typeof runFileResults[i].success === 'boolean') &&
            (typeof runFileResults[i].options === 'object');
          }
          var passed = isArray && fields;

          expect(passed).toBe(true);
          done();
        });

      });

      it('should report proper results for a valid program', function (done) {

        waitsFor(function () {
         return okFlag; 
        }, "okFlag should be set after call to test function", LONG_TIMEOUT);

        runs(function () {
          var results = runFileResults[0].results;
          var passed = true;
          var name;
          for (name in results) {
            if (results.hasOwnProperty(name)) {
              passed = passed &&
              (typeof results[name].hz === 'number') &&
              (typeof results[name].rme === 'number') &&
              (typeof results[name].samples === 'number');
            }
          }
          expect(passed).toBe(true);
          done();
        });
      });
    });
  }
  
  return { performTest: performTest };
});
