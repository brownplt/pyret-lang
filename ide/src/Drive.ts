const FOLDER_MIME = 'application/vnd.google-apps.folder';

function loadScript(src : string) {
  const script = document.createElement('script');
  script.src = src;
  script.async = false; // We need to simulate the load order of CPO here, unfortunately
  document.body.appendChild(script);
  return script;
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
    loadScript('/js/localSettings.js');
    (window as any).LOG_URL = false;
    (window as any).GIT_REV = false;
    (window as any).GIT_BRANCH = false;
    loadScript('/js/log.js');
    loadScript('/js/q.js');
    loadScript('https://apis.google.com/js/client.js');
    loadScript('https://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js');
    const apiWrapperScript = loadScript('/js/google-apis/api-wrapper.js');
    console.log(`Found API key: ${apiKey}`);
    apiWrapperScript.addEventListener('load', () => {
      console.log('Loaded API wrapper script ', (window as any).gwrap);
      resolve((window as any).gwrap);
    });
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

  getFileStructureFor = async (folderId : string) => {
    async function recAccess(id : string) {
      console.log(google);
      // (window as any).gapi.auth.setToken({ access_token: null });
      const filesAndFolders = await (window as any).gapi.client.drive.files.list({
        supportsAllDrives: true,
        includeItemsFromAllDrives: true,
        q: `"${id}" in parents and not trashed`,
        fields: 'files(id, name, mimeType, modifiedTime, modifiedByMeTime, webContentLink, iconLink, thumbnailLink)',
      });
      const files = filesAndFolders.result.files.filter((f : any) => f.mimeType !== FOLDER_MIME);
      const folders = filesAndFolders.result.files.filter((f : any) => f.mimeType === FOLDER_MIME);
      return {
        id,
        files,
        folders: await Promise.all(folders.map(async (f : any) => ({ ...f, ...(await recAccess(f.id)) }))),
      };
    }
    await this.load();
    return recAccess(folderId);
  };
}
export default GoogleAPI;