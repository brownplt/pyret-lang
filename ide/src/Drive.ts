import { GoogleDriveFile } from './state';

const FOLDER_MIME = 'application/vnd.google-apps.folder';

function loadScript(src : string) {
  return new Promise((resolve) => {
    const script = document.createElement('script');
    script.src = src;
    script.async = false; // We need to simulate the load order of CPO here, unfortunately
    document.body.appendChild(script);
    script.addEventListener('load', () => {
      resolve(script);
    });
  });
}

let apiKey = 'API_KEY_UNINITIALIZED';

// This is a little dopey, but doing some feature detection via whether we are
// being served on a domain with the apiKey endpoint. This is OK public
// information to have; it's somethign that gets templated into every
// client-side Google API project.
const google : Promise<any> = new Promise((resolve, reject) => {
  fetch('/apiKey').then(async (response) => {
    console.log('API Key response', response);
    apiKey = await response.text();
    (window as any).apiKey = apiKey;
    await loadScript('/js/localSettings.js');
    (window as any).LOG_URL = false;
    (window as any).GIT_REV = false;
    (window as any).GIT_BRANCH = false;
    await loadScript('/js/log.js');
    await loadScript('/js/q.js');
    await loadScript('https://apis.google.com/js/client.js');
    await loadScript('https://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js');
    await loadScript('/js/google-apis/api-wrapper.js');
    resolve((window as any).gwrap);
  }).catch((error) => {
    console.log(`No /apiKey endpoint, so assuming we are running locally ${error}`);
    reject(error);
  });
});

class GoogleAPI {
  /**
   *  Load the client library. Return a promise to allow .then() in caller
   */
  load = async () => (await google).load({
    name: 'drive',
    version: 'v3',
    reauth: { immediate: true },
  });

  /**
   *  Sign in the user upon button click.
   */
  signIn = async () => (await google).load({
    name: 'drive',
    version: 'v3',
    reauth: { immediate: false },
  });

  /*
  createNewFile = (parentFolderId, fileName) => {
    var reqOpts = {
      'path': '/drive/v3/files',
      'method': 'POST',
      'body': {
        'parents': [parentFolderId],
        'mimeType': 'text/plain',
        'name': fileName
      }
    };
    return window.gapi.client.request(reqOpts);
  }
  */

  createDir = async (name : string, parent : string) => {
    await this.load();
    return (window as any).gapi.client.drive.files.create({
      requestBody: {
        name,
        mimeType: 'application/vnd.google-apps.folder',
        parents: [parent],
      },
    });
  };

  createFile = async (name : string, parent : string, contents : string) => {
    await this.load();
    const created = await (window as any).gapi.client.drive.files.create({
      name,
      parents: [parent],
    });
    console.log('Craeted a file', created);
    await this.saveFile(created.result, contents);
    return { ...created.result, body: contents, modifiedTime: new Date(Date.now()) };
  };

  saveFile = async (file : GoogleDriveFile, newContents: string) => {
    await this.load();
    return (window as any).gapi.client.request({
      path: `/upload/drive/v3/files/${file.id}?uploadType=media`,
      method: 'PATCH',
      params: {
        uploadType: 'media',
      },
      body: newContents,
    });
  };

  getFileStructureFor = async (folderId : string) => {
    async function recAccess(fileInfo : any) {
      console.log(google);
      // (window as any).gapi.auth.setToken({ access_token: null });
      const filesAndFolders = await (window as any).gapi.client.drive.files.list({
        supportsAllDrives: true,
        includeItemsFromAllDrives: true,
        q: `"${fileInfo.id}" in parents and not trashed`,
        fields: 'files(id, name, mimeType, modifiedTime, modifiedByMeTime, webContentLink, iconLink, thumbnailLink)',
      });
      const files = filesAndFolders.result.files.filter((f : any) => f.mimeType !== FOLDER_MIME)
        .map(async (f : any) => {
          const contents = await (window as any).gapi.client.drive.files.get({
            fileId: f.id,
            alt: 'media',
          });
          return { ...f, body: contents.body };
        });
      const folders = filesAndFolders.result.files.filter((f : any) => f.mimeType === FOLDER_MIME);
      return {
        ...fileInfo,
        files: await Promise.all(files),
        folders: await Promise.all(folders.map(async (f : any) => ({ ...f, ...(await recAccess(f)) }))),
      };
    }
    await this.load();
    const folder = await (window as any).gapi.client.drive.files.get({
      fileId: folderId,
    });
    return recAccess(folder.result);
  };
}
export default GoogleAPI;
