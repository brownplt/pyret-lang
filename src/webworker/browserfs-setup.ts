function setup(worker, projectsDir) {
  const BrowserFS = require("browserfs");
  window["BrowserFS"] = BrowserFS;

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
}

module.exports = function(worker, projectsDir) {
  setup(worker, projectsDir);
  return {
    BrowserFS: BrowserFS,
    worker: worker,
  };
};
