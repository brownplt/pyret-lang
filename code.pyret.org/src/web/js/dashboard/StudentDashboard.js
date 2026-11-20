import React, { Component } from 'react';
import GoogleAPI from './GoogleAPI.js';
import {FILE_EXT, APP_NAME} from './config.js';
import File from './File';

const NOT_SIGNED_IN = 1;
const WAITING_FOR_SIGNIN = 2;
const SIGNED_IN = 3;
const INITIAL_LOAD = 4;


const WAITING_FOR_FILES = [];

// How long to wait after the last keystroke before asking Drive
const SEARCH_DEBOUNCE_MS = 300;

const PICKER_BATCH_SIZE = 50;

function parseFileIds(text) {
  return text.split(/[^A-Za-z0-9_-]+/).filter((id) => id.length > 0);
}


class StudentDashboard extends Component {
  constructor() {
    super();

    this.state = {
      signedIn: INITIAL_LOAD,
      files: [],
      activeTab: 'recent-files',
      newFileName: '',
      searchQuery: '',
      // Matches from Drive for searchQuery (beyond what's in files), and
      // whether we're still waiting on Drive for them
      searchResults: [],
      searching: false,
      userName: false,
      addedCount: false,
      manageOpen: false,
      idsPanel: false,
      copiedIds: '',
      copied: false,
      pastedIds: ''
    };

    this.api = new GoogleAPI();
    var apiLoaded = this.api.load();
    apiLoaded.then((resp) => {
      if(resp.hasAuth()) {
        this.setState({signedIn: SIGNED_IN});
        this.updateRecentFiles();
        this.api.getUsername().then((userInfo) => {
          this.setState({ userName: userInfo.email });
        });
      }
      else {
        this.setState({ signedIn: NOT_SIGNED_IN });
      }
    });
    apiLoaded.fail((e) => {
      this.setState({ signedIn: NOT_SIGNED_IN });
    });
  }

  handleStartCodingClick = (event) => {
    window.open("/editor", "_blank");
  }

  handleSignInClick = (event) => {
    this.setState({signedIn: WAITING_FOR_SIGNIN});
    this.api.signIn().then((resp) => {
      this.setState({signedIn: SIGNED_IN});
      this.api.getUsername().then((userInfo) => {
        this.setState({ userName: userInfo.emailAddresses[0].value });
      });
      this.updateRecentFiles();
    })
    .fail((resp) => {
      this.setState({ signedIn: NOT_SIGNED_IN });
    });
  }

  handleSignOutClick = (event) => {
    this.setState({signedIn: NOT_SIGNED_IN});
    window.location.replace('/logout');
  }

  handleTabClick = (event) => {
    this.setState({activeTab: event.target.id});
    if (event.target.id === 'recent-files') {
      this.updateRecentFiles();
    }
  }

  updateRecentFiles = () => {
    this.setState({files: WAITING_FOR_FILES});
    this.api.getRecentFilesByExtAndAppName(APP_NAME, FILE_EXT).then((resp) => {
      this.setState({files: resp.result.files});
    })
  }

  handleSearchChange = (event) => {
    const query = event.target.value;
    const trimmed = query.trim();
    clearTimeout(this.searchTimer);
    // Bumping this makes any in-flight response for an older query get dropped
    const searchId = this.searchId = (this.searchId || 0) + 1;
    this.setState({searchQuery: query, searchResults: [], searching: trimmed !== ''});
    if (trimmed === '') { return; }
    this.searchTimer = setTimeout(() => {
      const done = (results) => {
        if (this.searchId !== searchId) { return; }
        this.setState({searchResults: results, searching: false});
      };
      this.api.getRecentFilesByExtAndAppName(APP_NAME, FILE_EXT, trimmed).then(
        (resp) => done(resp.result.files || []),
        (err) => {
          console.error("File search failed: ", err);
          done([]);
        });
    }, SEARCH_DEBOUNCE_MS);
  }

  // The loaded files filter instantly as the user types (any substring of
  // the name); matches Drive finds beyond the loaded page get added after.
  getVisibleFiles = () => {
    const query = this.state.searchQuery.trim().toLowerCase();
    if (query === '') { return this.state.files; }
    const local = this.state.files.filter((f) => f.name.toLowerCase().includes(query));
    const seen = {};
    local.forEach((f) => { seen[f.id] = true; });
    return local.concat(this.state.searchResults.filter((f) => !seen[f.id]));
  }

  handleNewFilenameChange = (event) => {
    this.setState({newFileName: event.target.value});
  }

  handleCreateNewFile = (event) => {
    event.preventDefault();
    var w = window.open("about:blank", "_blank");
    if (this.state.newFileName) {
      this.api.getAppFolderID(APP_NAME).then((resp) => {
        var files = resp.result.files;

        // App Folder did not yet exist
        if (files.length === 0) {
          this.api.createAppFolder(APP_NAME).then((resp) => {
            return this.api.createNewFile(resp.result.id, this.state.newFileName + '.arr').then((resp)=> {
              w.location = EDITOR_REDIRECT_URL + resp.result.id;
//              window.open(EDITOR_REDIRECT_URL + resp.result.id, '_newtab');
            });
          });
        }

        // App Folder already existed
        else {
          return this.api.createNewFile(files[0].id, this.state.newFileName + '.arr').then((resp) => {
            w.location = EDITOR_REDIRECT_URL + resp.result.id;
//            window.open(EDITOR_REDIRECT_URL + resp.result.id, '_newtab');
          });
        }
      });
    }
  }

  handleSelectFileClick = (event) => {
    var w = window.open("about:blank", "_blank");
    this.api.getAppFolderID(APP_NAME).then((resp) => {
      var files = resp.result.files;
      if(files.length === 0) {
        var urlToOpen = "https://drive.google.com/drive/u/0/";
      }
      else {
        var urlToOpen = "https://drive.google.com/drive/u/0/folders/" + files[0].id;
      }
      w.location = urlToOpen;
    }).fail((err) => {
      w.close();
    });
    /*
    this.api.createPicker(APP_NAME, (data) => {
      if (data.action === window.google.picker.Action.PICKED) {
        var fileId = data.docs[0].id;
        window.open(EDITOR_REDIRECT_URL + fileId, "_blank");
        window.picker.setVisible(false);
      }
    });
    */
  }

  addFiles = (ids) => {
    if (ids.length === 0) { return this.pickBatches([null]); }
    return this.api.getVisibleFileIds().then((visible) => {
      var needed = ids.filter((id) => visible.indexOf(id) === -1);
      var batches = [];
      for (var i = 0; i < needed.length; i += PICKER_BATCH_SIZE) {
        batches.push(needed.slice(i, i + PICKER_BATCH_SIZE));
      }
      return this.pickBatches(batches);
    });
  }

  pickBatches = (batches) => {
    var added = 0;
    var pickBatch = (n) => {
      if (n >= batches.length) { return added; }
      var batchLabel = batches.length > 1 ? ' (' + (n + 1) + ' of ' + batches.length + ')' : '';
      return this.api.openAddFilesPicker(batches[n], FILE_EXT, 'Add files to ' + APP_NAME + batchLabel, batchLabel).then((picked) => {
        if (picked === null) { return added; }
        added += picked.length;
        return pickBatch(n + 1);
      });
    };
    return Q(pickBatch(0)).then((count) => {
      if (count > 0 || batches.length === 0) {
        this.setState({addedCount: count, manageOpen: false, idsPanel: false});
        this.updateRecentFiles();
      }
      return count;
    });
  }

  handleManageClick = (event) => {
    this.setState({manageOpen: true});
  }

  handleManageCloseClick = (event) => {
    this.setState({manageOpen: false, idsPanel: false});
  }

  handleManageOverlayClick = (event) => {
    if (event.target === event.currentTarget) { this.handleManageCloseClick(event); }
  }

  handleAddFilesClick = (event) => {
    this.addFiles([]);
  }

  handleCopyIdsClick = (event) => {
    event.preventDefault();
    this.api.getProgramFileIds(APP_NAME, FILE_EXT).then((ids) => {
      var text = ids.join(',');
      this.setState({idsPanel: 'copy', copiedIds: text, copied: false});
      if (window.navigator.clipboard) {
        window.navigator.clipboard.writeText(text).then(() => {
          this.setState({copied: true});
        }, () => {});
      }
    });
  }

  handlePasteIdsClick = (event) => {
    event.preventDefault();
    this.setState({idsPanel: 'paste'});
  }

  handlePastedIdsChange = (event) => {
    this.setState({pastedIds: event.target.value});
  }

  handleAddPastedIds = (event) => {
    event.preventDefault();
    var ids = parseFileIds(this.state.pastedIds);
    if (ids.length === 0) { return; }
    this.addFiles(ids).then((count) => {
      if (!this.state.manageOpen) { this.setState({pastedIds: ''}); }
    });
  }

  selectAll = (event) => {
    event.target.select();
  }

  // A simple callback implementation.
  pickerCallback = (data) => {
    if (data.action === window.google.picker.Action.PICKED) {
      var fileId = data.docs[0].id;
      window.open(EDITOR_REDIRECT_URL + fileId, "_blank");
    }
  }

  render = () => {
    const visibleFiles = this.getVisibleFiles();
    const isSearch = this.state.searchQuery.trim() !== '';
    return (
      <div className='wrap'>
        <div id='header' className=''>
          <div className='container'>
            <div className='left' aria-label='Welcome to Pyret'>
              <img src='/img/pyret-logo.png' aria-hidden='true' className='dashboard-logo'></img>
              <div className='header'>
                <h1 className='logo-text'>{APP_NAME}</h1>
                <h2 className={'person-text ' + (this.state.userName === false ? 'hidden' : '')}>{this.state.userName}</h2>
              </div>
            </div>
            <div className='button-wrapper right'>
              <button className={'auth-button ' + (this.state.signedIn !== NOT_SIGNED_IN ? '' : 'hidden')} onClick={this.handleSignOutClick} id='signout-button' >Sign out</button>
            </div>
            <div className='button-wrapper right start'>
              <button className={'start-button ' + (this.state.signedIn === SIGNED_IN ? '' : 'hidden')} onClick={this.handleStartCodingClick} id='start-button' >Open Editor</button>
            </div>
          </div>
        </div>
        <div className={'main middle container ' + (this.state.signedIn === NOT_SIGNED_IN ? '' : 'hidden')}>

          <div className={'middle large-logo-container'} aria-label='Pyret'>
            <img src="/img/pyret-logo.png" aria-hidden='true'></img>
          </div>

          <div className='clearfix'></div>


          <div className={'left'}>
            <p><button onClick={this.handleStartCodingClick} id='start-button' >Open Editor</button></p><p><em>to start coding immediately</em></p>
          </div>

          <div className={'right'}>
            <p><button className={'auth-button'} onClick={this.handleSignInClick} id='signin-button' >Sign in</button></p><p><em>to save and view programs</em></p>
          </div>

          <div className='clearfix'></div>

          <br/><br/>

          <p>You can also check out <a href="https://dcic-world.org/" aria-label="Pyret book">our book that uses Pyret</a> or <a href="http://www.bootstrapworld.org" aria-label="Bootstrap curricula">our curricula</a>.</p>

        </div>
        <div id='loading-spinner' className={this.state.signedIn === WAITING_FOR_SIGNIN ? '' : 'hidden'}>
          <h1>Waiting for login...</h1>
          <i className='fa fa-circle-o-notch fast-spin fa-3x fa-fw'></i>
        </div>
        <div id='file-picker-modal' className={'modal-wrap container ' + (this.state.signedIn === SIGNED_IN ? '' : 'hidden')}>
          <div id='file-picker-modal-tabs' className='cf'>
            <h2 id='recent-files' className={'tab floatable left ' + ((this.state.activeTab === 'recent-files') ? 'active' : '')} onClick={this.handleTabClick}>Recent Files</h2>
            <h2 id='new-file' className={'tab floatable left ' + ((this.state.activeTab === 'new-file') ? 'active' : '')} onClick={this.handleTabClick}>New File</h2>
            <div className='button-wrapper right'>
              <button id='manage' onClick={this.handleManageClick} >Manage</button>
              <button id='select-file' onClick={this.handleSelectFileClick} >View in Google Drive</button>
            </div>
          </div>
          <div id='file-search' className={(this.state.activeTab === 'new-file') ? 'hidden' : ''}>
            <input id='file-search-input' type='search' placeholder='Search your files by name' aria-label='Search your files by name'
              autoComplete='off' value={this.state.searchQuery} onChange={this.handleSearchChange} />
            <span id='file-search-status' role='status' className={this.state.searching ? '' : 'hidden'}>
              <i className='fa fa-circle-o-notch fast-spin fa-fw'></i> Searching all files...
            </span>
          </div>
          <div id='file-picker-modal-body' className={'modal-body ' + ((this.state.activeTab === 'new-file') ? 'hidden' : '')}>
            {this.state.addedCount === false ? null : <p id='added-files'>Added {this.state.addedCount} files.</p>}
            {
              this.state.files === WAITING_FOR_FILES ?
              (<div id='loading-spinner'>
                <h2>Loading files...</h2>
                <i className='fa fa-circle-o-notch fast-spin fa-3x fa-fw'></i>
              </div>)
              :
                visibleFiles.length > 0 ?
                    (<div className='file-list cf'>
                      {visibleFiles.map((f) => {return <File key={f.id} id={f.id} name={f.name} modifiedTime={f.modifiedTime} />;})}
                    </div>)
                  : isSearch ?
                    (this.state.searching ? <p><em>Searching...</em></p> : <p><em>No files with names matching "{this.state.searchQuery.trim()}".</em></p>)
                  :
                    <p><em>No Pyret files yet, use New File above to create one.</em></p>
            }
          </div>
          <div className={'modal-body ' + ((this.state.activeTab === 'new-file') ? '' : 'hidden')}>
            <form onSubmit={this.handleCreateNewFile}>
              <label className='input-label'>New file name:</label>
              <input className='form' type='text' value={this.state.newFileName} onChange={this.handleNewFilenameChange} />
              <span className='arr-ext'>.arr</span>
              <input id='new-file' className='button ' type='submit' value='Create' />
            </form>
          </div>
        </div>
        {!this.state.manageOpen ? null :
          <div id='manage-overlay' onClick={this.handleManageOverlayClick}>
            <div id='manage-modal'>
              <p>If you aren't seeing files you expect in the list, you can browse your Drive to add them. If you use another Pyret site, you can also copy file IDs from one site to another.</p>
              <p><button id='add-files' onClick={this.handleAddFilesClick} >Choose files to add</button></p>
              <div id='file-ids'>
                <p>
                  <a href='#' id='copy-ids' onClick={this.handleCopyIdsClick}>Copy file IDs</a> | <a href='#' id='paste-ids' onClick={this.handlePasteIdsClick}>Paste file IDs</a>
                </p>
                {this.state.idsPanel !== 'copy' ? null :
                  <div>
                    <p id='copied-ids-status'>{this.state.copied ? 'Copied.' : 'Copy these:'}</p>
                    <textarea id='copied-ids' readOnly value={this.state.copiedIds} onFocus={this.selectAll} />
                  </div>}
                {this.state.idsPanel !== 'paste' ? null :
                  <form onSubmit={this.handleAddPastedIds}>
                    <textarea id='pasted-ids' value={this.state.pastedIds} onChange={this.handlePastedIdsChange} />
                    <input id='add-pasted-ids' className='button' type='submit' value='Add files' />
                  </form>}
              </div>
              <p><button id='manage-close' onClick={this.handleManageCloseClick} >Close</button></p>
            </div>
          </div>}
        <div className='footer middle'>
          <p className='right'>
            <a target="_blank" href="https://www.pyret.org">pyret.org</a> | <a target="_blank" href={"https://" + process.env.APP_DOMAIN + "/faq/"}>Policies</a> | <a target="_blank" href="https://www.github.com/brownplt/code.pyret.org">Software</a></p>
        </div>

      </div>
    );
  }
}

export default StudentDashboard;
