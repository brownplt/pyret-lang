const BrowserFS = require("browserfs");

const fs = BrowserFS.BFSRequire('fs');

const path = BrowserFS.BFSRequire('path');

const install = (): void => {
  BrowserFS.install(window);
};

const configure = (worker: Worker): void => {
  BrowserFS.configure({
    fs: "LocalStorage"
  }, function(e: any) {
    if (e) {
      throw e;
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
