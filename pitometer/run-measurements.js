var require = require('requirejs');
require(["child_process", "fs", "command-line-args", "node-uuid", "./measurements"], function(childProcess, fs, commandLineArgs, uuid, measurements) {

  const optionDefinitions = [
    { name: 'outfile', alias: 'o', type: String, defaultValue: "pitometer/results.json" },
    { name: 'config', alias: 'c', type: String, defaultValue: "pitometer/config.json" }
  ];

  const options = commandLineArgs(optionDefinitions)

  console.log(options);

  const config = JSON.parse(String(fs.readFileSync(options["config"])));


  function parseBranches(str) {
    /* Example output:

$ git branch -r --contains d91dfc7876de3e2ecc589435f973f35be62435d0
  origin/bare
  origin/dag-accum
  origin/return-and-tail
  origin/return-stack


$ git branch -r --contains 119a5e636a09dcc7ad228ee2f7cafdad4a804e06
  origin/return-and-tail

    */
    return str
      .split("\n")
      .filter((s) => s !== "")
      .filter((s) => s.indexOf("HEAD") === -1)
      .map((s) => s.slice(9));
  }

  function measureCommit(runnerName, runner) {
    const runString = (cmd) => String(childProcess.execSync(cmd));
    const codeDate = new Date(runString("git show -s --format=%ci"));
    const commitInfo = runString("git show --stat");
    const commit = commitInfo.split("\n")[0].split(" ")[1];
    const branches = parseBranches(runString("git branch -r --contains"));
    const date = new Date();

    const repoData = {
      codeDate: codeDate,
      commitInfo: commitInfo,
      branches: branches,
      commit: commit,
      measurementDate: date
    };

    function makeMeasurements(measurements) {
      return measurements.map(function(m) {
        return {
          uuid: uuid.v4(),
          codeDate: codeDate.toISOString(),
          commitInfo: commitInfo,
          branches: branches,
          commit: commit,
          measurementDate: codeDate.toISOString(),
          labels: m.labels,
          measurement: m.measurement,
          unit: m.unit
        };
      });
    }

    return runner(makeMeasurements, {
      repoData: repoData,
      config: config['measurements'][runnerName]
    });

  }

  const allMeasurements = [];
  const measurementsToRun = Object.keys(measurements).filter((mname) => {
    return (mname in config["measurements"]) &&
           (!config["measurements"][mname]["disabled"]);
  });
  measurementsToRun.forEach((k) =>    
    allMeasurements.push.apply(allMeasurements, measureCommit(k, measurements[k]))
  )

  fs.writeFileSync(options.outfile, JSON.stringify(allMeasurements, null, "  "));

})
