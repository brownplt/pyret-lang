import React from 'react';
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
        return <div>&#x1f4c2;</div>;
      } if (stats.isFile()) {
        return <div>&#128441;</div>;
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
