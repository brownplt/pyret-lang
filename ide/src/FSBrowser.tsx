/* Handles the main logic of the file system browser. FSBrowser acts as a
   container for FSItems (see FSItem.tsx) */

// TODO (michael): improve accessibilty by enabling these rules
/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */
/* eslint-disable jsx-a11y/label-has-associated-control */

import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import {
  Upload,
  FilePlus,
  FolderPlus,
} from 'react-feather';
import * as control from './control';
import * as action from './action';
import FSItem from './FSItem';

type StateProps = {
  browseRoot: string,
  browsePath: string
  developerMode: boolean,
};

function mapStateToProps(state: any): StateProps {
  return {
    browseRoot: state.browseRoot,
    browsePath: state.browsePath,
    developerMode: state.developerMode,
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
  editValue: string,
  selected: string | undefined,
};

class FSBrowser extends React.Component<FSBrowserProps, FSBrowserState> {
  /* Compares FSItemPairs (the output of createFSItemPair). This is used as a
     comparison function to sort FSItems, so that we always display FSItems in the
     same order. */
  static compareFSItemPair(a: [string, FSItem], b: [string, FSItem]) {
    if (a[0] < b[0]) {
      return -1;
    } if (a[0] > b[0]) {
      return 1;
    }
    return 0;
  }

  constructor(props: FSBrowserProps) {
    super(props);

    this.state = {
      editType: undefined,
      editValue: '',
      selected: undefined,
    };
  }

  get browsePathString() {
    const { browsePath } = this.props;
    return browsePath;
  }

  get browsingRoot() {
    const { browsePath, browseRoot } = this.props;
    return browsePath === browseRoot;
  }

  /* Moves the current directory up the tree, like `cd ..` */
  traverseUp = (): void => {
    const { browsePath, setBrowsePath } = this.props;

    const newPath = control.bfsSetup.path.join(browsePath, '..');

    this.setState({
      selected: undefined,
    });

    setBrowsePath(newPath);
  };

  /* Moves the current directory down the tree, like `cd childDirectory` */
  traverseDown = (childDirectory: string): void => {
    const { browsePath, setBrowsePath } = this.props;

    const newPath = control.bfsSetup.path.join(browsePath, childDirectory);

    this.setState({
      selected: undefined,
    });

    setBrowsePath(newPath);
  };

  /* Either opens a directory (if child is a directory), or opens a file (if
     child is a file) in the LHS of the editor. */
  expandChild = (child: string): void => {
    const { onExpandChild } = this.props;

    const fullChildPath = control.bfsSetup.path.join(this.browsePathString, child);
    const stats = control.fs.statSync(fullChildPath);

    if (stats.isDirectory()) {
      this.traverseDown(child);
    } else if (stats.isFile()) {
      this.setState({
        selected: child,
      });

      onExpandChild(fullChildPath);
    }
  };

  /* Creates a FSItem, returning a two-element array where the first is the
     path, and the second is the item. See also: compareFSItemPair. */
  createFSItemPair = (filePath: string): [string, any] => {
    const { browsePath } = this.props;
    const { selected } = this.state;

    return [
      filePath,
      <FSItem
        key={filePath}
        onClick={() => this.expandChild(filePath)}
        path={control.bfsSetup.path.join(browsePath, filePath)}
        selected={filePath === selected}
      />,
    ];
  };

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

  /* (unused), deletes the selected FSItem. When this was enabled it was too
     easy to delete the current directory and all of its contents.
     TODO(michael): add this back in a more sensible way. */
  deleteSelected = (): void => {
    const { selected } = this.state;
    const { browsePath } = this.props;

    if (selected === undefined) {
      control.removeDirectory(this.browsePathString);

      this.traverseUp();
    } else {
      control.removeFile(
        control.bfsSetup.path.join(browsePath, selected),
      );

      this.setState({
        selected: undefined,
      });
    }
  };

  /* Selects the current directory. This is only matters when determining what
     to delete (see deleteSelected) */
  selectCurrentDirectory = (): void => {
    this.setState({
      selected: undefined,
    });
  };

  /* Opens a system-specific file uploading dialog, writing the result to the
     file system. */
  uploadFile = (event: any): void => {
    const { browsePath } = this.props;

    const file = event.target.files[0];

    if (file) {
      const reader = new FileReader();

      reader.onload = (e: any) => {
        const data = e.target.result;
        const { name } = file;

        control.bfsSetup.fs.writeFileSync(
          control.bfsSetup.path.join(browsePath, name),
          data,
        );

        this.forceUpdate();
      };

      reader.readAsText(file);
    }
  };

  render() {
    const { editType, editValue, selected } = this.state;
    const { browsePath, developerMode } = this.props;

    const that = this;

    function makeEditor() {
      if (editType !== undefined) {
        return (
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
                <div>Name:</div>
              ) : (
                <div style={{ display: 'flex', flexDirection: 'row' }}>
                  Name:
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
        );
      }

      return false;
    }
    const editor = makeEditor();

    let fsitems;
    try  {
      fsitems = control.fs
        .readdirSync(this.browsePathString)
        .map(this.createFSItemPair)
        .sort(FSBrowser.compareFSItemPair)
        .map((x: [string, FSItem]) => x[1]);
    } catch (e) {
      console.error('Could not find path: ', e);
      return (
        <span>
          Could not find path
          {this.browsePathString}
        </span>
      );
    }

    return (
      <div style={{ display: 'flex', flexDirection: 'column' }}>
        <div style={{ display: 'flex', flexDirection: 'column' }}>
          <div
            className="fs-browser-item"
            style={{
              display: 'flex',
              flexDirection: 'row',
              height: 'auto',
            }}
          >
            <div
              onClick={this.selectCurrentDirectory}
              style={{
                cursor: 'pointer',
                fontFamily: 'monospace',
                display: 'flex',
                alignItems: 'center',
                paddingLeft: '1em',
                paddingRight: '1em',
                background: selected ? 'none' : 'darkgray',
              }}
            >
              {control.bfsSetup.path.parse(browsePath).base || '/'}
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
                  onChange={this.uploadFile}
                  style={{
                    display: 'none',
                  }}
                />
                <Upload />
              </label>
              <button
                className="fs-browser-item"
                onClick={this.toggleEditFile}
                title="New file"
                type="button"
              >
                <FilePlus />
              </button>
              <button
                className="fs-browser-item"
                onClick={this.toggleEditDirectory}
                title="New folder"
                type="button"
              >
                <FolderPlus />
              </button>
              {/* {!this.browsingRoot
                  && (
                  <button
                  className="fs-browser-item"
                  onClick={this.deleteSelected}
                  type="button"
                  >
                  <X />
                  </button>
                  )} */}
            </div>
          </div>
          {editor}
          {!this.browsingRoot && developerMode && (
          <FSItem
            onClick={this.traverseUp}
            path=".."
            selected={false}
          />
          )}
          { fsitems }
        </div>
      </div>
    );
  }
}

export default connector(FSBrowser);
