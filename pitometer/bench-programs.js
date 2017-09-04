define(["child_process", "time-helpers", "fs", "path"], function(childProcess, timeHelpers, fs, path) { 
  function run(makeMeasurements, options) {

    const { hrtimeToMicroseconds, maybeTime } = timeHelpers;
    const workDir = options.workDir;
    // For include, empty list includes all, populated list checks for _some_
    // label matching the given string (so union, no intersection)
    const config = options.config || { include: [], programsPath: "pitometer/programs/" };
    const programsPath = config.programsPath || "pitometer/programs/";
    const include = config.include || [];

    function echoRun(cmd, opts) {
      console.log(cmd);
      return childProcess.execSync(cmd, opts);
    }


    function compileAndTimeRun(program) {
      const toBuild = program.replace(/\.arr$/, ".jarr");
      const [compileSuccess, , ] = maybeTime(true, () => echoRun(
        `env EF='--flatness-threshold -1' make ${toBuild}`, {stdio: [0, 1, 2]}));

      return maybeTime(compileSuccess, () => echoRun(`node ${toBuild}`));
    }

    let paths = fs.readdirSync(programsPath);
    console.log("Running for these programs: ", paths);
    paths = paths.filter((p) => p.slice(-4) === ".arr");
    paths = paths.filter((p) => include === "*" || include.some((i) => (p.indexOf(i) !== -1)));
    console.log("Running for these programs after filters: ", paths);
    const results = [];
    echoRun("make phaseA", {stdio: [0, 1, 2]});
    paths.map((p) => {
      const programPath = path.join(programsPath, p);
      const [runSuccess, , time] = compileAndTimeRun(programPath);
      if(runSuccess) {
        results.push({
            labels: ["bench-program", p],
            measurement: hrtimeToMicroseconds(time),
            unit: "microseconds"
          });
      }
    });

    return makeMeasurements(results);
  }

  return {
    run: run
  };
});

