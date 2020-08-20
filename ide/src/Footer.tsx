import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { State } from './state';

type StateProps = {
  linting: boolean,
  compiling: boolean | 'out-of-date',
  running: boolean,
};

function mapStateToProps(state: State): StateProps {
  const {
    linting,
    compiling,
    running,
  } = state;
  return {
    linting,
    compiling,
    running,
  };
}

type PropsFromReact = {};

type DispatchProps = {};

function mapDispatchToProps(/* dispatch: (action: Action) => any */): DispatchProps {
  return {};
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type FooterProps = PropsFromRedux & DispatchProps & StateProps & PropsFromReact;

function Footer({
  linting,
  compiling,
  running,
}: FooterProps) {
  const statusStyle = {
    marginLeft: '1em',
  };
  const activeColor = 'white';
  const inactiveColor = 'gray';
  return (
    <div
      style={{
        height: '2.7em',
        background: '#111',
        display: 'flex',
        flex: 'none',
        color: '#fff',
        alignItems: 'center',
      }}
    >
      <div
        style={{
          ...statusStyle,
          color: linting ? activeColor : inactiveColor,
        }}
      >
        Linting
      </div>
      <div
        style={{
          ...statusStyle,
          color: compiling ? activeColor : inactiveColor,
        }}
      >
        Compiling
      </div>
      <div
        style={{
          ...statusStyle,
          color: running ? activeColor : inactiveColor,
        }}
      >
        Running
      </div>
    </div>
  );
}

export default connector(Footer);
