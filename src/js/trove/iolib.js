({
    requires: [],
    provides: {
      values: {
        "prompt": "tany"
      }
    },
    nativeRequires: ["fs"],
    theModule: function(RUNTIME, NAMESPACE, uri, fs) {
      // https://github.com/nodejs/node/issues/28243#issuecomment-502402453
      function PromptIO(msg) {
        // only works on linux
        const stdin = fs.openSync("/dev/stdin","rs");
  
        fs.writeSync(process.stdout.fd, msg);
        let s = '';
        let buf = Buffer.alloc(1);
        fs.readSync(stdin, buf, 0, 1, null);
        while((buf[0] != 10) && (buf[0] != 13)) {
          s += buf;
          fs.readSync(stdin, buf, 0, 1, null);
        }
  
        return s;
      };
  
      var vals = {
          "prompt": RUNTIME.makeFunction(function(input) {
            RUNTIME.ffi.checkArity(1, arguments, "prompt", false);
            RUNTIME.checkString(input);
            return PromptIO(input);
          }, "prompt")
      };
  
      return RUNTIME.makeModuleReturn(vals, {});
    }
  })
  
  