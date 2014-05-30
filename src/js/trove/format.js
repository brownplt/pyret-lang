
define(["js/runtime-util", "trove/lists", "js/ffi-helpers"], function(util, L, ffiLib) {

  return util.memoModule("format", function(RUNTIME, NAMESPACE) {
    var F = ffiLib(RUNTIME, NAMESPACE);
    // Stolen from https://github.com/dyoo/whalesong/blob/master\
    // /whalesong/js-assembler/runtime-src/baselib-format.js
    var formatRegexp1 = new RegExp('~[sSaA]', 'g');
    var formatRegexp2 = new RegExp("~[sSaAnevE%~]", "g");
    return RUNTIME.loadModules(NAMESPACE, [L], function(list) {
    
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
                  errorStrBuffer.push( RUNTIME.toReprJS(args[i], "tostring") );
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
                  return RUNTIME.toReprJS(buffer.shift(), "tostring");
              } else if (s === '~e' || s === "~E") {
                  // FIXME: we don't yet have support for the error-print
                  // handler, and currently treat ~e just like ~s.
                  if (buffer.length === 0) {
                      throwFormatError();
                  }
                  return RUNTIME.toReprJS(buffer.shift(), "tostring"); 
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
                  return RUNTIME.toReprJS(buffer.shift(), "tostring");
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

      function listToArr(lst) {
        var arr = [];
        if(!(RUNTIME.unwrap(RUNTIME.getField(list, "List").app(lst)))) {
          throw new Error("Expected list in listToArr, but got something else");
        }
        while(!(RUNTIME.unwrap(RUNTIME.getField(list, "is-empty").app(lst)))) {
          arr.push(RUNTIME.getField(lst, "first"));
          lst = RUNTIME.getField(lst, "rest");
        }
        return arr;
      }

      return RUNTIME.makeObject({
        provide: RUNTIME.makeObject({
          format: RUNTIME.makeFunction(function(str, args) {
            F.checkArity(2, arguments, "format");
            RUNTIME.checkString(str);
            return RUNTIME.makeString(format(RUNTIME.unwrap(str), listToArr(args)));
          }),
        }),
        answer: NAMESPACE.get("nothing")
      });
    });
  });
});

