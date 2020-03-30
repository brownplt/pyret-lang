export const BrowserFS = require('browserfs'); // eslint-disable-line global-require

export const fs = BrowserFS.BFSRequire('fs');

export const path = BrowserFS.BFSRequire('path');

export const install = (): void => {
  BrowserFS.install(window);
};

export const configure = (worker: Worker, projectsDirectory: string): void => {
  BrowserFS.configure({
    fs: 'LocalStorage',
  }, (e: any) => {
    if (e) {
      throw e;
    }

    if (!fs.existsSync(projectsDirectory)) {
      fs.mkdirSync(projectsDirectory);
    }

    BrowserFS.FileSystem.WorkerFS.attachRemoteListener(worker);
    (window as any).bfs = BrowserFS.BFSRequire('fs');
  });
};
