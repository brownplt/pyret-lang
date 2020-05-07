import React from 'react';
import { ReactSVG } from 'react-svg';
import * as control from './control';

type FSItemProps = {
  onClick: () => void;
  path: string;
  selected: boolean;
};

type FSItemState = {};

export default class FSItem extends React.Component<FSItemProps, FSItemState> {
  render() {
    const { path, selected, onClick } = this.props;

    const stats = control.fs.statSync(path);

    const label = (() => {
      if (stats.isDirectory()) {
        return (
          <ReactSVG src="smallFolderIcon.svg" />
        );
      } if (stats.isFile()) {
        return (
          <ReactSVG src="file.svg" />
        );
      }
      return '?';
    })();

    const background = selected ? 'darkgray' : 'rgba(0, 0, 0, 0.3)';

    return (
      <button
        onClick={onClick}
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
        }}
        >
          <div style={{
            width: '1em',
            paddingRight: '1em',
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
