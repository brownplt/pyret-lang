/* An item (a directory or a file entry) in the file system browser. FSItems are
   created in FSBrowser.tsx. */

import React from 'react';
import {
  File,
  Folder,
} from 'react-feather';
import * as control from './control';

type FSItemProps = {
  onClick: () => void;
  path: string;
  selected: boolean;
};

type FSItemState = { hover: boolean };

export default class FSItem extends React.Component<FSItemProps, FSItemState> {
  constructor(props: FSItemProps) {
    super(props);
    this.state = { hover: false };
  }

  render() {
    const { path, selected, onClick } = this.props;

    const stats = control.fs.statSync(path);

    const { hover } = this.state;

    const label = (() => {
      if (stats.isDirectory()) {
        return (
          <Folder width="16" />
        );
      } if (stats.isFile()) {
        return (
          <File width="16" />
        );
      }
      return '?';
    })();

    let background = 'rgba(0, 0, 0, 0.3)';
    if (selected) background = 'darkgray';
    if (hover) background = 'rgba(0,0,0,0.5)';

    return (
      <button
        onClick={onClick}
        onMouseEnter={() => this.setState({ hover: true })}
        onMouseLeave={() => this.setState({ hover: false })}
        style={{
          background,
          border: 0,
          height: '2.7em',
          color: '#fff',
          textAlign: 'left',
          flex: 'none',
          cursor: 'pointer',
        }}
        type="button"
      >
        <div style={{
          display: 'flex',
          flexDirection: 'row',
          alignItems: 'center',
        }}
        >
          <div style={{
            paddingLeft: '.4em',
            paddingRight: '.6em',
          }}
          >
            {label}
          </div>
          <div>
            {control.bfsSetup.path.parse(path).base}
          </div>
        </div>
      </button>
    );
  }
}
