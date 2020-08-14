/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */

import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { Dialog } from '@reach/dialog';
import '@reach/dialog/styles.css';
// import { interact } from './control';
// import { State } from './state';
// import { Action } from './action';

type StateProps = {};

function mapStateToProps(/* state: State */): StateProps {
  return {};
}

type DispatchProps = {};

function mapDispatchToProps(/* dispatch: (action: any) => void */): DispatchProps {
  return {};
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromReact = {
  reactor: {
    $brand: 'reactor',
    'get-value': () => any,
    'draw': () => any,
    '$interactNoPauseResume': (insertNode?: (node: any) => void) => any,
  },
  convert: (value: any) => any,
};

type PropsFromRedux = ConnectedProps<typeof connector>;
type Props = PropsFromRedux & DispatchProps & StateProps & PropsFromReact;

function Reactor({ reactor, convert }: Props) {
  const [showDialog, setShowDialog] = React.useState(false);
  const [node, setNode]: [any, (node: any) => void] = React.useState(false);
  const open = () => setShowDialog(true);
  const close = () => setShowDialog(false);

  function getInitialValue() {
    try {
      return convert(reactor.draw());
    } catch (e) {
      console.log('failed draw with', e);
      return convert(reactor['get-value']());
    }
  }
  const value = getInitialValue();
  return (
    <div
      onClick={() => {
        try {
          reactor.$interactNoPauseResume((newNode) => {
            setNode(newNode);
            open();
          });
        } catch (e) {
          console.log('failed with', e);
        }
      }}
    >
      {value}
      <Dialog
        isOpen={showDialog}
        onDismiss={close}
        aria-label="Reactor"
      >
        <button
          className="close-button"
          onClick={close}
          type="button"
        >
          <span aria-hidden>×</span>
        </button>
        <div
          ref={((div) => {
            if (div !== null && node !== false) {
              div.appendChild(node);
            }
          })}
        />
      </Dialog>
    </div>
  );
}

export default connector(Reactor);
