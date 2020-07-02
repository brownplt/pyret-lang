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
  X,
} from 'react-feather';
import * as control from './control';
import * as action from './action';
import FSItem from './FSItem';

type stateProps = {
  browseRoot: string,
  browsePath: string
};

function mapStateToProps(state: any): stateProps {
  return {
    browseRoot: state.browseRoot,
    browsePath: state.browsePath,
  };
}

type dispatchProps = {
  setBrowsePath: (path: string) => void,
  onExpandChild: (path: string) => void,
};

function mapDispatchToProps(dispatch: (action: action.Action) => any): dispatchProps {
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
type FSBrowserProps = PropsFromRedux & dispatchProps & stateProps;

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

  traverseUp = (): void => {
    const { browsePath, setBrowsePath } = this.props;

    const newPath = control.bfsSetup.path.join(browsePath, '..');

    this.setState({
      selected: undefined,
    });

    setBrowsePath(newPath);
  };

  traverseDown = (childDirectory: string): void => {
    const { browsePath, setBrowsePath } = this.props;

    const newPath = control.bfsSetup.path.join(browsePath, childDirectory);

    this.setState({
      selected: undefined,
    });

    setBrowsePath(newPath);
  };

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

  handleSubmit = (value: React.SyntheticEvent): void => {
    const { editValue, editType } = this.state;
    const { browsePath } = this.props;

    value.preventDefault();

    const name = editValue;
    const path = control.bfsSetup.path.join(browsePath, name);

    if (editType === EditType.CreateFile) {
      control.createFile(path);
    } else {
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

  selectCurrentDirectory = (): void => {
    this.setState({
      selected: undefined,
    });
  };

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
    const { browsePath } = this.props;

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
                type="button"
              >
                <FilePlus />
              </button>
              <button
                className="fs-browser-item"
                onClick={this.toggleEditDirectory}
                type="button"
              >
                <FolderPlus />
              </button>
              {!this.browsingRoot
                             && (
                             <button
                               className="fs-browser-item"
                               onClick={this.deleteSelected}
                               type="button"
                             >
                               <X />
                             </button>
                             )}
            </div>
          </div>
          {editor}
          {!this.browsingRoot && (
          <FSItem
            onClick={this.traverseUp}
            path=".."
            selected={false}
          />
          )}
          {
                        control.fs
                          .readdirSync(this.browsePathString)
                          .map(this.createFSItemPair)
                          .sort(FSBrowser.compareFSItemPair)
                          .map((x: [string, FSItem]) => x[1])
                    }
        </div>
      </div>
    );
  }
}

export default connector(FSBrowser);
