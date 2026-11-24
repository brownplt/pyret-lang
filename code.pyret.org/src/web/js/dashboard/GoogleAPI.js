var FOLDER_MIME = "application/vnd.google-apps.folder";
class GoogleAPI {
  /**
   *  Load the client library. Return a promise to allow .then() in caller
   */
  load = () => {
    return gwrap.load({name: 'drive',
      version: 'v3',
      reauth: {
        immediate: true
      }
    });
  }

  /**
   *  Sign in the user upon button click.
   */
  signIn = (event) => {
    return gwrap.load({name: 'drive',
      version: 'v3',
      reauth: {
        immediate: false
      }
    });
  }

  /**
   *  Sign out the user upon button click.
   */
  signOut = (event) => {
    throw "Can't sign out yet.";
  }

  createAppFolder = (appName) => {
    return window.gapi.client.drive.files.create({
      resource: {
        'name' : appName,
        'mimeType' : FOLDER_MIME
      }
    });
  }

  getAppFolderID = (appName) => {
    return window.gapi.client.drive.files.list({
      q: '("me" in owners) and not trashed and mimeType="application/vnd.google-apps.folder" and name ="' + appName + '"'
    });
  }

  getAppSharedFolderID = (appName) => {
    return window.gapi.client.drive.files.list({
      q: '("me" in owners) and not trashed and mimeType="application/vnd.google-apps.folder" and name ="' + appName + '.shared"'
    });
  }

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

  // If nameQuery is given, only files whose names match it are returned.
  // Drive's `name contains` matches prefixes of words in the name, across
  // *all* of the user's Pyret files (not just the first page of results).
  getRecentFilesByExtAndAppName = (appName, ext, nameQuery) => {
    var nameFilter = "";
    if(nameQuery) {
      var escaped = nameQuery.replace(/\\/g, "\\\\").replace(/'/g, "\\'");
      nameFilter = " and name contains '" + escaped + "'";
    }
    return Q.all([this.getAppFolderID(appName), this.getAppSharedFolderID(appName)])
      .then((resp) => {
        var savedFiles = resp[0].result.files;
        var sharedFiles = resp[1].result.files;
        var isSharedFile = "(appProperties has {key=\'originalProgramFlag\' and value=\'true\'})";
        if(sharedFiles.length > 0) {
          isSharedFile += "or '" + sharedFiles[0].id + "' in parents";
        }
        if(savedFiles.length === 0) { return this.getRecentFilesByExt(ext, nameFilter); }
        else {
          return window.gapi.client.drive.files.list({
            fields: "files(id, name, modifiedTime)",
            q: 'not trashed and not (' + isSharedFile + ') and (fileExtension="' + ext + '" or "' + savedFiles[0].id + '" in parents)' + nameFilter,
          });
        }
      })
  }

  /**
   * list files w/ extension [ext].
   */
  getRecentFilesByExt = (ext, nameFilter) => {
    return window.gapi.client.drive.files.list({
      fields: "files(id, name)",
      q: 'not trashed and fileExtension="' + ext + '"' + (nameFilter || ""),
    });
  }

  getAppDataFileID = (appDataFilename) => {
    return window.gapi.client.drive.files.list({
      q: 'not trashed and name="' + appDataFilename + '"',
      spaces: 'appDataFolder'
    });
  }

  createAppDataFile = (appDataFilename) => {
    return window.gapi.client.drive.files.create({
      resource: {
        name: appDataFilename,
        parents: ['appDataFolder']
      }
    });
  }

  getAppDataFileContent = (fileId) => {
    return window.gapi.client.drive.files.get({
      fileId: fileId,
      // Download a file — files.get with alt=media file resource
      alt: 'media'
    });
  }

  saveAppData = (fileId, appData) => {
    return window.gapi.client.drive.files.update({
      path: '/upload/drive/v3/files/' + fileId,
      method: 'PATCH',
      params: {
        uploadType: 'media'
      },
      body: JSON.stringify(appData)
    });
  }

  // Create and render a Google Picker object for selecting a file.
  createPicker = (appName, callback) => {
    window.gapi.load('picker', () => {
      this.getAppFolderID(appName).then((resp) => {
        var driveView = new window.google.picker.DocsView(window.google.picker.ViewId.DOCS).setIncludeFolders(true);
        if(resp.result.files.length !== 0) {
          driveView.setParent(resp.result.files[0].id);
        }
        window.picker = new window.google.picker.PickerBuilder()
          .setTitle("Select a Pyret document")
          .addView(driveView)
          .setOAuthToken(window.gapi.auth.getToken().access_token)
          .setCallback(callback)
          .setOrigin(window.location.protocol + '//' + window.location.host)
          .build();

        window.picker.setVisible(true);
      })
    });
  }

  getUsername = () => {
    var token = window.gapi.auth.getToken().access_token;
    return fetch('https://openidconnect.googleapis.com/v1/userinfo', {
      headers: { Authorization: 'Bearer ' + token }
    }).then((resp) => resp.json());
  }
}

export default GoogleAPI;
