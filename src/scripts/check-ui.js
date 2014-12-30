/*global define */
/*jslint unparam: true, node: true*/

define(["js/ffi-helpers", "trove/option", "trove/srcloc", "./error-ui"], function(ffiLib, optionLib, srclocLib, errorUI) {
  function drawCheckResults(runtime, checkResults) {
    var ffi = ffiLib(runtime, runtime.namespace);
    var get = runtime.getField;
    var checkArray = ffi.toArray(checkResults);

    function drawSrcloc(s) {
      return s ? get(s, "format").app(true) : "";
    }

    return runtime.loadModules(runtime.namespace, [optionLib, srclocLib], function(option, srcloc) {
      var checkTotalAll = 0;
      var checkPassedAll = 0;
      var checkBlockCount = checkArray.length;
      var checkBlocksErrored = 0;
      var msg = "";

      checkArray.reverse().forEach(function(cr) {
	var checkTotal = 0;
	var checkPassed = 0;
	var name = get(cr, "name");
	var resultArray = ffi.toArray(get(cr, "test-results"));

	resultArray.reverse().forEach(function(tr) {
	  checkTotalAll += 1;
	  checkTotal += 1;

	  if(runtime.hasField(tr, "reason")) {
	    runtime.runThunk(function() {
	      return get(tr, "reason").app();
	    }, function(result) {
	      msg += "test at "
		+ drawSrcloc(get(tr, "loc"))
		+ " (" + get(tr, "code")
		+ "): failed, reason:\n"
		//Note(ben) indent this
		+ result.result
		+ "\n";
	    });
	  }
	  else {
	    checkPassedAll += 1;
	    checkPassed += 1;
	    msg += "test at "
	      + drawSrcloc(get(tr, "loc"))
	      + " (" + get(tr, "code")
	      + "): ok"
	      + "\n";
	  }
	});

	var thisBlockErrored = false;
	if(runtime.hasField(cr, "maybe-err")) {
	  var error = get(cr, "maybe-err");

	  if(get(option, "is-some").app(error)) {
	    thisBlockErrored = true;
	    checkBlocksErrored += 1;
	    msg += "Check block at "
	      + name
	      + " at "
	      + drawSrcloc(get(cr, "loc"))
	      + " ended in error (all tests may not have run)\n\n"
	      + errorUI.drawError(runtime, get(error, "value").val)
	      + "\n\n";
	  }
	}

	if(!thisBlockErrored) {
	  if(checkTotal > 1) {
	    msg += "  " + checkPassed + "/" + checkTotal
	      + " tests passed in check block: "
	      + name
	      + "\n\n";
	  }
	  else if(checkTotal === 1) {
	    if(checkPassed === 1) {
	      msg += "The test passed.\n\n";
	    }
	    else {
	      msg += "The test failed.\n\n";
	    }
	  }
	}
      });

      if (checkPassedAll === checkTotalAll && checkBlocksErrored === 0) {
	if (checkTotalAll > 0) {
	  if (checkTotalAll === 1) {
	    return "Looks shipshape, your test passed, mate!";
	  }

	  if (checkTotalAll === 2) {
	    return "Looks shipshape, both tests passed, mate!";
	  }

	  return "Looks shipshape, all " + checkTotalAll + " tests passed, mate!";
	}
      }

      if (checkBlocksErrored > 0) {
	return msg + checkPassedAll
	  + " tests passed and "
	  + (checkTotalAll - checkPassedAll)
	  + " failed in all check blocks.\n"
	  + "HOWEVER "
	  + checkBlocksErrored
	  + " check block(s) ended in error, so some tests may not have run.\n"
	  + "Check the output above to see what errors occured.";
      }

      if(checkBlockCount > 1) {
	return msg + checkPassedAll
	  + "/" + checkTotalAll
	  + " tests passed in all check blocks.";
      }

      return msg;
    });
  }

  return {
    drawCheckResults : drawCheckResults
  };
});
