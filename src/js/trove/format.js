
define(["js/runtime-util"], function(util) {

  return util.definePyretModule("format",
    [],
    [],
    {},
    function(RUNTIME, NAMESPACE) {
        // Stolen from https://github.com/dyoo/whalesong/blob/master\
        // /whalesong/js-assembler/runtime-src/baselib-format.js
        var formatRegexp1 = new RegExp('~[sSaA]', 'g');
        var formatRegexp2 = new RegExp("~[sSaAnevE%~]", "g");
        
        // format: string [X ...] string -> string
        // String formatting.  If an exception occurs, throws
        // a plain Error whose message describes the formatting error.
        var format = function(formatStr, args, functionName) {
            var throwFormatError = function() {
                functionName = functionName || 'format';
                var matches = formatStr.match(formatRegexp1);
                var expectedNumberOfArgs = (matches === null ? 0 : matches.length);
                var errorStrBuffer = [functionName + ': format string requires '
                                      + expectedNumberOfArgs
                                      + ' arguments, given ' + args.length + '; arguments were:',
                                      toWrittenString(formatStr)];
                var i;
                for (i = 0; i < args.length; i++) {
                  if(RUNTIME.isString(args[i])) {
                    errorStrBuffer.push( RUNTIME.toReprJS(args[i], RUNTIME.ReprMethods._tostring) );
                  }
                  else {
                    RUNTIME.ffi.throwTypeMismatch(args[i], "String");
                  }
                }

                throw new Error(errorStrBuffer.join(' '));
            };


            var buffer = args.slice(0);
            var onTemplate = function(s) {
                if (s === "~~") {
                    return "~";
                } else if (s === '~n' || s === '~%') {
                    return "\n";
                } else if (s === '~s' || s === "~S") {
                    if (buffer.length === 0) {
                        throwFormatError();
                    }
                    return RUNTIME.toReprJS(buffer.shift(), RUNTIME.ReprMethods._tostring);
                } else if (s === '~e' || s === "~E") {
                    // FIXME: we don't yet have support for the error-print
                    // handler, and currently treat ~e just like ~s.
                    if (buffer.length === 0) {
                        throwFormatError();
                    }
                    var arg = buffer.shift();
                    if (RUNTIME.isString(arg)) {
                      return RUNTIME.toReprJS(buffer.shift(), RUNTIME.ReprMethods._tostring); 
                    }
                    else {
                      RUNTIME.ffi.throwTypeMismatch(args[i], "String");
                    }
                }
                else if (s === '~v') {
                    if (buffer.length === 0) {
                        throwFormatError();
                    }
                    // fprintf must do something more interesting here by
                    // printing the dom representation directly...
                    return toWrittenString(buffer.shift());
                } else if (s === '~a' || s === "~A") {
                    if (buffer.length === 0) {
                        throwFormatError();
                    }
                    return RUNTIME.toReprJS(buffer.shift(), RUNTIME.ReprMethods._tostring);
                } else {
                    throw new Error(functionName + 
                                    ': string.replace matched invalid regexp');
                }
            };
            var result = formatStr.replace(formatRegexp2, onTemplate);
            if (buffer.length > 0) {
                throwFormatError();
            }
            return result;
        };

        return RUNTIME.makeObject({
          provide: RUNTIME.makeObject({
            format: RUNTIME.makeFunction(function(str, args) {
              RUNTIME.ffi.checkArity(2, arguments, "format");
              RUNTIME.checkString(str);
              return RUNTIME.makeString(format(RUNTIME.unwrap(str), RUNTIME.ffi.toArray(args)));
            }),
          }),
          answer: NAMESPACE.get("nothing")
        });
      });
});

