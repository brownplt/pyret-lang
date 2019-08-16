export const BrowserFS = require('browserfs');

export const install = () => {
  BrowserFS.install(window);
};

export const configure = (worker: Worker) => {
  BrowserFS.configure(
    {
      fs: "LocalStorage"
    },
    (e: any) => {
      BrowserFS.FileSystem.WorkerFS.attachRemoteListener(worker);
      if (e) {
        throw e;
      }
    }
  );
};

export const fs = BrowserFS.BFSRequire('fs');
