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
        // console.log(RUNTIME);
        fs.writeSync(RUNTIME.stdout_fd, msg);
        let s = '';
        while (true) {
          let buf = Buffer.alloc(1);

          // TODO: figure out why this needs to be within a try-catch
          try {
            // TODO: make sure this line works on non-unix OS
            fs.readSync(RUNTIME.stdin_fd, buf, 0, 1, null);
          } catch (e) {
            if (e.code !== "EAGAIN") {
              throw e;
            }
          }

          // TODO: elaborate on more exit conditions
          if (buf.toString("utf-8") === "\n") {
            break;
          }

          s += buf.toString("utf-8");
        }
        return s.trim();
      };
  
      var vals = {
      // TODO: write a test like an actual software engineer
          "prompt": RUNTIME.makeFunction(function(input) {
            RUNTIME.ffi.checkArity(1, arguments, "prompt", false);
            RUNTIME.checkString(input);
            return PromptIO(input);
          }, "prompt")
      };
  
      return RUNTIME.makeModuleReturn(vals, {});
    }
  })
  
  