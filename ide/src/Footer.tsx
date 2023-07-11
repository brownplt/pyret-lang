/* The footer at the bottom of the page. Displays the current state of the
   system (linting, compiling, or running). */

import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { RunningState, State } from './state';
import { NeverError } from './utils';

type StateProps = {
  footerMessage: string,
  running : RunningState,
};

function mapStateToProps(state: State): StateProps {
  const {
    footerMessage,
    running,
  } = state;
  return {
    footerMessage,
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

function Footer({ footerMessage, running }: FooterProps) {
  let runMessage;
  switch (running.type) {
    case 'idle':
      runMessage = '';
      break;
    case 'segments':
      runMessage = `Running ${running.done + 1}/${running.total}`;
      break;
    case 'text':
      runMessage = 'Running';
      break;
    default:
      throw new NeverError(running);
  }
  return (
    <div
      style={{
        height: '2.7em',
        background: '#111',
        display: 'flex',
        flex: 'none',
        color: '#fff',
        alignItems: 'center',
        justifyContent: 'space-between',
        padding: '0em 1em',
      }}
    >
      {footerMessage}
      <span style={{ padding: '0em 1em' }}>
        {runMessage}
      </span>
      <span style={{ float: 'right' }}>
        <a style={{ color: 'white' }} target="_blank" rel="noreferrer" href="https://docs.google.com/forms/d/e/1FAIpQLSe-V7yOimO599_EYlmlL_IYrSlyjOLu7EaSOYCpmD2_iFDKfA/viewform">Report an issue or give feedback here.</a>
      </span>
    </div>
  );
}

export default connector(Footer);
