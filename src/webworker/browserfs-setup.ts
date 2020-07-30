export const BrowserFS = require('browserfs'); // eslint-disable-line global-require

export const fs = BrowserFS.BFSRequire('fs');

export const path = BrowserFS.BFSRequire('path');

export const install = (): void => {
  BrowserFS.install(window);
};

export const configure = (worker: Worker /* , projectsDirectory: string */): void => {
  BrowserFS.configure({
    fs: 'MountableFileSystem',
    options: {
      '/projects': {
        fs: 'LocalStorage',
      },
      '/prewritten': {
        fs: 'InMemory',
      },
      '/compiled': {
        fs: 'InMemory',
      },
    },
  }, (e: any) => {
    if (e) {
      throw e;
    }

    // if (!fs.existsSync(projectsDirectory)) {
    //   fs.mkdirSync(projectsDirectory);
    // }

    // if (!fs.existsSync('/compiled/builtin')) {
    //   fs.mkdirSync('/compiled/builtin');
    // }

    BrowserFS.FileSystem.WorkerFS.attachRemoteListener(worker);
    (window as any).bfs = BrowserFS.BFSRequire('fs');
  });
};
