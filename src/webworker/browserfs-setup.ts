const BrowserFS = require("browserfs");

const fs = BrowserFS.BFSRequire('fs');

const path = BrowserFS.BFSRequire('path');

const install = (): void => {
  BrowserFS.install(window);
};

const configure = (worker: Worker, projectsDirectory: string): void => {
  BrowserFS.configure({
    fs: "LocalStorage"
  }, function(e: any) {
    if (e) {
      throw e;
    }

    if (!fs.existsSync(projectsDirectory)) {
      fs.mkdirSync(projectsDirectory);
    }

    BrowserFS.FileSystem.WorkerFS.attachRemoteListener(worker);
  });
};

module.exports = {
  BrowserFS: BrowserFS,
  fs: fs,
  path: path,
  install: install,
  configure: configure
};
