define(["child_process", "time-helpers"], function(childProcess, timeHelpers) {

  function run(makeMeasurements, options) {

    const { hrtimeToMicroseconds, maybeTime } = timeHelpers;

    childProcess.execSync("make clean");
    const [installSuccess, , timeInstall] = maybeTime(true, () =>
      childProcess.execSync("npm install"));
    const [aSuccess, , timeA] = maybeTime(installSuccess, () =>
      childProcess.execSync("make phaseA"));
    const [bSuccess, , timeB] = maybeTime(aSuccess, () =>
      childProcess.execSync("make phaseB"));
    const [cSuccess, timeC] = maybeTime(bSuccess, () =>
      childProcess.execSync("make phaseC"));

    return makeMeasurements([
      aSuccess && {
        labels: ["js-wall-time", "compiler-build", "phaseA"],
        measurement: hrtimeToMicroseconds(timeA),
        unit: "microseconds"
      },
      bSuccess && {
        labels: ["js-wall-time", "compiler-build", "phaseB"],
        measurement: hrtimeToMicroseconds(timeB),
        unit: "microseconds"
      },
      cSuccess && {
        labels: ["js-wall-time", "compiler-build", "phaseC"],
        measurement: hrtimeToMicroseconds(timeC),
        unit: "microseconds"
      }
    ].filter((a) => a));
  }

  return {
    run: run
  };
});

