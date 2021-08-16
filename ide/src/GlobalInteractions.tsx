/* Based on MenuBar.tsx. For Chatitor only, Copy/Clear */

import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { Action } from './action';
import { CHUNKSEP } from './chunk';
import { State } from './state';

type StateProps = {
  currentFileContents: string,
};

function mapStateToProps(state: State): StateProps {
  const { currentFileContents } = state;
  const cfc = currentFileContents ?? '';
  return { currentFileContents: cfc };
}

type DispatchProps = {
  clear: () => void,
};

function mapDispatchToProps(dispatch: (action: Action) => any): DispatchProps {
  return {
    clear() {
      dispatch({ type: 'chunk', key: 'clear' });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type MenuBarProps = PropsFromRedux & DispatchProps & StateProps;

function GlobalInteractions({ clear, currentFileContents }: MenuBarProps) {
  // Shared with MenuBar, could be de-duplicated
  const buttonStyle = {
    background: '#979797',
    height: '100%',
    border: 'none',
    color: 'rgb(255, 255, 255)',
    marginRight: '0.1em',
  };
  return (
    <div
      style={{
        display: 'flex',
        height: '100%',
      }}
    >
      <button
        style={buttonStyle}
        className="menu-button"
        onClick={() => {
          const noChunks = currentFileContents.replaceAll(CHUNKSEP, '\n');
          navigator.clipboard.writeText(noChunks);
        }}
        type="button"
      >
        Copy
      </button>
      <button className="menu-button" style={buttonStyle} onClick={clear} type="button">Clear</button>
    </div>
  );
}

export default connector(GlobalInteractions);
