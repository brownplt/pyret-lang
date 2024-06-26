export const BrowserFS = require('browserfs'); // eslint-disable-line global-require

export const fs = BrowserFS.BFSRequire('fs');

export const path = BrowserFS.BFSRequire('path');

export const process = BrowserFS.BFSRequire('process');
process.versions = { node: '20.11.0' };

export const install = (): void => {
  BrowserFS.install(window);
};

export async function configure(instantiateWorker : (() => Promise<Worker>) /* , projectsDirectory: string */) : Promise<void> {
  return new Promise((resolve, reject) => {
      BrowserFS.configure({
        fs: 'MountableFileSystem',
        options: {
          '/projects/': {
            fs: "AsyncMirror",
            options: {
              sync: { fs: "InMemory" },
              async: {
                fs: 'IndexedDB',
                options: {
                  storeName: "parley"
                }
              }
            }
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
          reject(e);
          throw e;
        }
        instantiateWorker().then((w : Worker) => {
          BrowserFS.FileSystem.WorkerFS.attachRemoteListener(w);
          (window as any).bfs = BrowserFS.BFSRequire('fs');
          resolve();
        });

    });
  });
};
