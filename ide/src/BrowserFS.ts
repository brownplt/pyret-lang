export const BrowserFS = require('browserfs');

export const fs = BrowserFS.BFSRequire('fs');
export const path = BrowserFS.BFSRequire('path');

export const install = () => {
  BrowserFS.install(window);
};

export const configure = (worker: Worker) => {
  BrowserFS.configure(
    {
      fs: "LocalStorage"
    },
    (e: any) => {
      if (e) {
        throw e;
      }

      BrowserFS.FileSystem.WorkerFS.attachRemoteListener(worker);
    }
  );
};
