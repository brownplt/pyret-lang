function setup(worker, projectsDir) {
  const BrowserFS = require("browserfs");

  // How to use BrowserFS with Web Workers: https://github.com/jvilk/BrowserFS/issues/210
  BrowserFS.install(window);
  BrowserFS.configure({
    fs: "LocalStorage"
  }, function(e) {
    BrowserFS.FileSystem.WorkerFS.attachRemoteListener(worker);

    let fs = BrowserFS.BFSRequire("fs");
    if (fs.existsSync(projectsDir) === false) {
      fs.mkdirSync(projectsDir);
    }

    if (e) {
      throw e;
    }
  });

  return BrowserFS;
}

module.exports = function(worker, projectsDir) {
  let BrowserFS = setup(worker, projectsDir);
  return {
    BrowserFS: BrowserFS,
    worker: worker,
  };
};
