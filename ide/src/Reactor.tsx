/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */

import React, { useRef } from 'react';
import { connect, ConnectedProps } from 'react-redux';
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
  const divref = useRef(null);

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
      ref={divref}
      onClick={() => {
        try {
          reactor.$interactNoPauseResume((node) => {
            console.log('here is the node, ', node, divref);
            const div = divref.current;

            if ((div as any) !== null) {
              while ((div as any).firstChild) {
                (div as any).removeChild((div as any).lastChild);
              }

              (div as any).appendChild(node);
            }
          });
        } catch (e) {
          console.log('failed with', e);
        }
      }}
    >
      {value}
    </div>
  );
}

export default connector(Reactor);
