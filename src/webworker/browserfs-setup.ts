export const BrowserFS = require('browserfs'); // eslint-disable-line global-require

export const fs = BrowserFS.BFSRequire('fs');

export const path = BrowserFS.BFSRequire('path');

export const process = BrowserFS.BFSRequire('process');
process.versions = { node: '20.11.0' };

export const install = (): void => {
  BrowserFS.install(window);
};

export const configure = (worker: Worker /* , projectsDirectory: string */): void => {
  BrowserFS.configure({
    fs: 'MountableFileSystem',
    options: {
      '/projects/': {
        fs: 'LocalStorage',
      },
      '/projects/data': {
        fs: 'InMemory',
      },
      '/google-drive/': {
        fs: 'InMemory',
      },
      '/prewritten': {
        fs: 'InMemory',
      },
      '/compiled': {
        fs: 'InMemory',
      },
      '/tmp': {
        fs: 'InMemory',
      },
    },
  }, (e: any) => {
    if (e) {
      throw e;
    }

    BrowserFS.FileSystem.WorkerFS.attachRemoteListener(worker);
    (window as any).bfs = BrowserFS.BFSRequire('fs');
  });
};
