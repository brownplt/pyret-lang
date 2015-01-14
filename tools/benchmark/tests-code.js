var b = require('benchmark-pyret');
require('jasmine-node');

jasmine.getEnv().addReporter(new jasmine.ConsoleReporter(console.log));

var validProgram = '1';
var invalidProgram = '1 + true';
var nonParsableProgram = '...';


xdescribe('checkResult', function(){
  it('returns true given a SuccessResult', function(){
    expect(b.test.testCheckResult(true)).toBe(true);
  });

  it('returns false given a FailureResult', function(){
    expect(b.test.testCheckResult(false)).toBe(false);
  });
});

xdescribe('ensureSuccess', function() {

  it('passes on a valid program', function() {
  	var passed, flag;  	

  	runs(function(){
  		passed = false;
  		flag = false;
  		b.test.testEnsureSuccess(validProgram, {}, function(result){
  			passed = result;
  			flag = true;
  		})
  	});

  	waitsFor(function(){
  		return flag;
  	}, 'failure',5000);

  	runs(function(){
  		expect(passed).toBe(true);
  	});
  });

  it('fails on an invalid program', function() {
  	var passed, flag;  	

  	runs(function(){
  		passed = false;
  		flag = false;
  		b.test.testEnsureSuccess(invalidProgram,{},function(result){
  			passed = result;
  			flag = true;
  		})
  	});

  	waitsFor(function(){
  		return flag;
  	}, 'failure',5000);

  	runs(function(){
  		expect(passed).toBe(false);
  	});
  });

  it('fails on a nonparsable program', function() {
  	var passed, flag;  	

  	runs(function(){
  		passed = false;
  		flag = false;
  		b.test.testEnsureSuccess(nonParsableProgram,{},function(result){
  			passed = result;
  			flag = true;
  		})
  	});

  	waitsFor(function(){
  		return flag;
  	}, 'failure',5000);

  	runs(function(){
  		expect(passed).toBe(false);
  	});
  });
});

xdescribe('parsePyret', function(){
  it('passes on a valid program', function() {
  	var passed, flag;  	

  	runs(function(){
  		passed = false;
  		flag = false;
  		b.test.testDeferredFunction(validProgram, {}, 'parsePyret',function(result){
  			passed = result;
  			flag = true;
  		});
  	});

  	waitsFor(function(){
  		return flag;
  	}, 'failure',5000);

  	runs(function(){
  		expect(passed).toBe(true);
  	});
  });

  it('fails on a nonparsable program', function() {
  	var passed, flag;  	

  	runs(function(){
  		passed = false;
  		flag = false;
  		b.test.testDeferredFunction(nonParsableProgram, {}, 'parsePyret',function(result){
  			passed = result;
  			flag = true;
  		});
  	});

  	waitsFor(function(){
  		return flag;
  	}, 'failure',5000);

  	runs(function(){
  		expect(passed).toBe(false);
  	});
  });
});

xdescribe('compilePyret', function(){
  it('passes on a valid program', function() {
  	var passed, flag;  	

  	runs(function(){
  		passed = false;
  		flag = false;
  		b.test.testDeferredFunction(validProgram, {}, 'compilePyret',function(result){
  			passed = result;
  			flag = true;
  		});
  	});

  	waitsFor(function(){
  		return flag;
  	}, 'failure',5000);

  	runs(function(){
  		expect(passed).toBe(true);
  	});
  });
});

xdescribe('evaluatePyret', function(){
  it('passes on a valid program', function() {
  	var passed, flag;  	

  	runs(function(){
  		passed = false;
  		flag = false;
  		b.test.testDeferredFunction(validProgram, {}, 'evaluatePyret',function(result){
  			passed = result;
  			flag = true;
  		});
  	});

  	waitsFor(function(){
  		return flag;
  	}, 'failure',5000);

  	runs(function(){
  		expect(passed).toBe(true);
  	});
  });

  it('fails on an invalid program', function() {
  	var passed, flag;  	

  	runs(function(){
  		passed = false;
  		flag = false;
  		b.test.testDeferredFunction(invalidProgram, {}, 'evaluatePyret',function(result){
  			passed = result;
  			flag = true;
  		});
  	});

  	waitsFor(function(){
  		return flag;
  	}, 'failure',5000);

  	runs(function(){
  		expect(passed).toBe(false);
  	});
  });
});

xdescribe('initializeGlobalRuntime', function(){
	it('sets a runtime to global.rt', function(){
		expect(b.test.testInitializeGlobalRuntime()).toBe(true);
	})
})

xdescribe('parsePyretSetup', function(){
	it('sets the result of a valid program to global.ast', function(){
		expect(b.test.testParsePyretSetup(validProgram, {})).toBe(true);
	})
})

jasmine.getEnv().execute();