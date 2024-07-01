/* Handles the main logic of the file system browser. */

// TODO (michael): improve accessibilty by enabling these rules
/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */
/* eslint-disable jsx-a11y/label-has-associated-control */

import React, { ChangeEvent, FormEvent } from 'react';
import { connect, ConnectedProps } from 'react-redux';
import TreeView, { flattenTree } from 'react-accessible-treeview';

import {
  Code,
  Database,
  Upload,
  FilePlus,
  Folder,
  FolderPlus,
  FolderMinus,
} from 'react-feather';
import * as control from './control';
import * as action from './action';

import { State, initialState } from './state';

// Allow us to upload directories (nonstandard, but wide support)
declare module "react" {
  interface InputHTMLAttributes<T> extends HTMLAttributes<T> {
      webkitdirectory?: string;
  }
}

type StateProps = {
  browseRoot: string,
  browsePath: string,
  currentFile: string,
};

function mapStateToProps(state: State): StateProps {
  return {
    browseRoot: state.browseRoot,
    browsePath: state.browsePath,
    currentFile: state.currentFile
  };
}

type DispatchProps = {
  setBrowsePath: (path: string) => void,
  onExpandChild: (path: string) => void,
};

function mapDispatchToProps(dispatch: (action: action.Action) => any): DispatchProps {
  return {
    setBrowsePath: (path: string) => dispatch({
      type: 'update',
      key: 'browsePath',
      value: path,
    }),
    onExpandChild: (path: string) => dispatch({
      type: 'update',
      key: 'currentFile',
      value: path,
    }),
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type FSBrowserProps = PropsFromRedux & DispatchProps & StateProps;

enum EditType {
  CreateFile,
  CreateDirectory
}

type FSBrowserState = {
  editType: EditType | undefined,
  editValue: string
};

function showPath(file : string) {
  if (!initialState.developerMode) {
    if (file.startsWith('.')) { return false; }
    if (file.endsWith('-segment')) { return false; }
  }
  return true;
}

function structureFromBrowseRoot(browseRoot : string) {
  const root = control.fs.readdirSync(browseRoot);
  const rootChildren = root.filter(showPath).map((child : string) => {
    const childPath = control.bfsSetup.path.join(browseRoot, child);
    const childStats = control.fs.statSync(childPath);
    if (childStats.isDirectory()) {
      return {
        id: childPath,
        name: child,
        metadata: { directory: true, path: childPath },
        children: structureFromBrowseRoot(childPath),
      };
    }
    return {
      id: childPath,
      metadata: { directory: false, path: childPath },
      name: child,
    };
  }).sort((a : any, b : any) => {
    const { directory: adir, path: apath } = a.metadata;
    const { directory: bdir, path: bpath } = b.metadata;
    if(adir && !bdir) { return -1; }
    if(!adir && bdir) { return 1; }
    if(apath < bpath) { return -1; }
    if(bpath < apath) { return 1; }
    return 0;
  });
  return rootChildren;
}

class FSBrowser extends React.Component<FSBrowserProps, FSBrowserState> {
  nameInputRef: HTMLInputElement | null;

  constructor(props: FSBrowserProps) {
    super(props);

    this.nameInputRef = null;

    this.state = {
      editType: undefined,
      editValue: '',
    };
  }

  /* Toggles the new file creation dialog. This is called when the file plus
     icon is clicked */
  toggleEditFile = (): void => {
    const { editType } = this.state;

    if (editType === EditType.CreateFile) {
      this.setState({
        editType: undefined,
      });
    } else {
      this.setState({
        editType: EditType.CreateFile,
      }, () => {
        const input = this.nameInputRef;
        if (input) {
          input.focus();
        }
      });
    }
  };

  /* Toggles the new directory creation dialog. This is called when the
     directory plus icon is clicked */
  toggleEditDirectory = (): void => {
    const { editType } = this.state;

    if (editType === EditType.CreateDirectory) {
      this.setState({
        editType: undefined,
      });
    } else {
      this.setState({
        editType: EditType.CreateDirectory,
      }, () => {
        const input = this.nameInputRef;
        if (input) {
          input.focus();
        }
      });
    }
  };

  /* Either creates a file or directory, based on what kind of creation dialog
     was open. Closes the dialog. This is called when you hit "enter" after
     typing in a file or directory name. See also: toggleEdit{Directory,File} */
  handleSubmit = (value: React.SyntheticEvent): void => {
    const { editValue, editType } = this.state;
    const { browsePath } = this.props;

    value.preventDefault();

    const name = editValue;

    if (editType === EditType.CreateFile) {
      const fullName = name.includes('.') ? name : `${name}.arr`;
      const path = control.bfsSetup.path.join(browsePath, fullName);
      control.createFile(path);
      // TODO(luna): Should have existence check, otherwise new file with same
      // name => truncate
      if (fullName.endsWith('.arr')) {
        control.fs.writeFileSync(path, 'include essentials2021');
      } else {
        control.createFile(path);
      }
    } else {
      const path = control.bfsSetup.path.join(browsePath, name);
      control.createDirectory(path);
    }

    this.setState({
      editType: undefined,
      editValue: '',
    });
  };

  onChange = (event: React.SyntheticEvent): void => {
    this.setState({
      editValue: (event.target as HTMLInputElement).value,
    });
  };

  mkdirp(base : string, dir : string) {
    const parts = dir.split(control.bfsSetup.path.sep);
    let sofar = base;
    parts.forEach(p => {
      sofar = control.bfsSetup.path.join(sofar, p);
      if(!control.bfsSetup.fs.existsSync(sofar)) {
        control.bfsSetup.fs.mkdirSync(sofar);
      }
    });
  }

  neededDirs(files : FileList) {
    const asArray = Array.from(files);
    const seenDirs : Set<string> = new Set();
    asArray.map(f => {
      const { dir } = control.bfsSetup.path.parse(f.webkitRelativePath);
      seenDirs.add(dir);
    });
    const toReturn = Array.from(seenDirs);
    toReturn.sort();
    return toReturn;
  }

  conflicts(base : string, files : FileList) {
    const asArray = Array.from(files);
    return asArray.filter(f => {
      const webkitpath = f.webkitRelativePath;
      const p = webkitpath === "" ? f.name : webkitpath;
      const toCheck = control.bfsSetup.path.join(base, p);
      return control.bfsSetup.fs.existsSync(toCheck);
    });
  }

  conflictMessage(conflicts : File[]) {
    let conflictPaths = conflicts.map(c => c.name);
    let more = "";
    if(conflictPaths.length > 6) {
      more = `\nand ${conflictPaths.length - 6} more...\n`;
      conflictPaths = conflictPaths.slice(0, 6);
    }
    const conflictString = conflictPaths.join("\n");
    return `This upload will overwrite the following files:\n${conflictString}${more}\nOverwrite them with the uploaded versions?`;

  }

  /* Opens a system-specific file uploading dialog, writing the result to the
     file system. */
  uploadFile = (event: FormEvent): void => {
    const { browsePath } = this.props;
    const target : HTMLInputElement = event.target as HTMLInputElement;
    if(!event.target || !target.files) { return; }
    const files = target.files;

    const conflicts = this.conflicts(browsePath, files);
    if(conflicts.length > 0) {
      const result = confirm(this.conflictMessage(conflicts));
      if(!result) {
        // NOTE(joe): Because we want to use onChange on just one input element, we need
        // to clear the state here. Otherwise, future clicks to the upload button with the
        // same file or directory won't do anything. This is a common case if someone reviews
        // the files after cancelling in the dialog about conflicts. Amusingly, setting
        // files to null has no effect, but setting value to the empty string does the
        // right clearing.
        // https://stackoverflow.com/questions/26634616/filereader-upload-same-file-again-not-working
        target.value = '';
        return;
      }
    }

    const toCreate = this.neededDirs(files);
    toCreate.forEach(c => {
      this.mkdirp(browsePath, c);
    });

    Array.from(files).forEach((file : File) => {
      if (file) {
        const reader = new FileReader();
  
        reader.onload = (e: any) => {
          const data = e.target.result;

          const fs = control.bfsSetup.fs;
          
          const webkitpath = file.webkitRelativePath;
          const p = webkitpath === "" ? file.name : webkitpath;
  
          fs.writeFileSync(
            control.bfsSetup.path.join(browsePath, p),
            Buffer.from(data),
          );
  
          this.forceUpdate();
        };
  
        reader.readAsArrayBuffer(file);
      }
    });
    // NOTE(joe): also clear this after a successful upload to keep things consistent
    target.value = '';
  };

  render() {
    const { editType, editValue } = this.state;

    const that = this;

    function makeEditor() {
      if (editType !== undefined) {
        return (
          <>
            <div style={{
              display: 'flex',
              flexDirection: 'row',
              alignItems: 'center',
            }}
            >
              <pre style={{
                paddingLeft: '1em',
                paddingRight: '1em',
              }}
              >
                {editType === EditType.CreateFile ? (
                  <div>New file name:</div>
                ) : (
                  <div style={{ display: 'flex', flexDirection: 'row' }}>
                    New folder name:
                  </div>
                )}
              </pre>
              <form
                onSubmit={that.handleSubmit}
                style={{
                  height: '100%',
                  flexGrow: 1,
                }}
              >
                <input
                  ref={(input) => { that.nameInputRef = input; }}
                  type="text"
                  value={editValue}
                  onChange={that.onChange}
                  style={{
                    border: 0,
                    padding: 0,
                    width: '100%',
                    height: '100%',
                  }}
                />
              </form>
            </div>
          </>
        );
      }

      return false;
    }
    const editor = makeEditor();

    const fsBrowserStructure = {
      name: '',
      children: structureFromBrowseRoot(that.props.browseRoot),
    };
    const flattened = flattenTree(fsBrowserStructure);
    const expandedIds = flattened.filter(val => {
      if(!val.metadata) { return false; }
      const meta = val.metadata;
      return that.props.browsePath.startsWith(meta.path! as string) && meta.directory;
    }).map(node => node.id);
    const selectedIds = flattened.filter(val => {
      if(!val.metadata) { return false; }
      const meta = val.metadata;
      return that.props.currentFile == meta.path;
    }).map(node => node.id);

    return (
      <div style={{ display: 'flex', flexDirection: 'column' }}>
        <div style={{ display: 'flex', flexDirection: 'column' }}>
          <div
            className="fs-browser-header"
            style={{
              display: 'flex',
              flexDirection: 'row',
              height: 'auto',
            }}
          >
            <div
              style={{
                cursor: 'pointer',
                display: 'flex',
                alignItems: 'center',
                paddingLeft: '1em',
                paddingRight: '1em',
              }}
            >
              {control.bfsSetup.path.parse(this.props.browseRoot).base || '/'}
            </div>
            <div style={{
              flexGrow: 1,
              display: 'flex',
              flexDirection: 'row',
              justifyContent: 'flex-end',
            }}
            >
              <label
                className="fs-browser-item"
                title="Upload file"
                style={{
                  width: '2.3em',
                  height: '100%',
                  display: 'flex',
                  justifyContent: 'center',
                  alignContent: 'center',
                  alignItems: 'center',
                }}
              >
                <input
                  type="file"
                  multiple={true}
                  webkitdirectory=""
                  onInput={this.uploadFile}
                  style={{
                    display: 'none',
                  }}
                />
                <Upload width="20px" />
              </label>
              <button
                className="fs-browser-item"
                onClick={this.toggleEditFile}
                title="New file"
                type="button"
              >
                <FilePlus width="20px" />
              </button>
              <button
                className="fs-browser-item"
                onClick={this.toggleEditDirectory}
                title="New folder"
                type="button"
              >
                <FolderPlus width="20px" />
              </button>
            </div>
          </div>
          {editor}
        </div>
        <div className="directory">
          <TreeView
            data={flattened}
            defaultExpandedIds={expandedIds}
            defaultSelectedIds={selectedIds}
            aria-label="directory tree"
            onNodeSelect={({ element }) => {
              if (typeof element.metadata?.path !== 'string') {
                console.error('Could not find path: ', element.metadata?.path);
                return;
              }
              if (element.metadata?.directory) {
                this.props.setBrowsePath(element.metadata?.path);
              } else {
                const { dir } = control.bfsSetup.path.parse(element.metadata?.path);
                this.props.setBrowsePath(dir);
                this.props.onExpandChild(element.metadata?.path);
              }
            }}
            nodeRenderer={({
              element,
              isExpanded,
              getNodeProps,
              level,
            }) => (
              // eslint-disable-next-line react/jsx-props-no-spreading
              <div {...getNodeProps()} style={{ paddingLeft: 20 * (level - 1) }}>
                {element.metadata?.directory ? (
                  <FolderIcon isOpen={isExpanded} />
                ) : (
                  <FileIcon filename={element.name} />
                )}

                {element.name}
              </div>
            )}
          />
        </div>
      </div>
    );
  }
}
function FolderIcon({ isOpen } : { isOpen : boolean}) {
  return isOpen ? (
    <FolderMinus color="#e8a87c" className="icon" width="16px" />
  ) : (
    <Folder color="#e8a87c" className="icon" width="16px" />
  );
}
function FileIcon({ filename } : { filename : string }) {
  const extension = filename.slice(filename.lastIndexOf('.') + 1);
  switch (extension) {
    case 'js':
      return <Code color="yellow" className="icon" width="16px" />;
    case 'arr':
      return <Code color="lightblue" className="icon" width="16px" />;
    case 'csv':
      return <Database color="lightgreen" className="icon" width="16px" />;
    default:
      return null;
  }
}

export default connector(FSBrowser);
