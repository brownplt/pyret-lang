/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */

import React from 'react';
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
    '$interactNoPauseResume': () => any,
  },
  convert: (value: any) => any,
};

type PropsFromRedux = ConnectedProps<typeof connector>;
type Props = PropsFromRedux & DispatchProps & StateProps & PropsFromReact;

function Reactor({ reactor, convert }: Props) {
  function getInitialValue() {
    try {
      return convert(reactor.draw());
    } catch (e) {
      return convert(reactor['get-value']());
    }
  }
  const value = getInitialValue();
  return (
    <div
      onClick={() => {
        try {
          reactor.$interactNoPauseResume();
          // // (window as any).stopify.newRTS('lazy');
          // console.log('stopify', (window as any).stopify);
          // console.log('reactor', reactor);
          // const code = (window as any).stopify.compiler.compileEval('reactor.interact();', {
          //   captureMethod: 'lazy',
          //   newMethod: 'direct',
          //   jsArgs: 'faithful',
          //   sourceMap: {
          //     getLine: () => null,
          //   },
          // });
          // console.log(code);
          // // eslint-disable-next-line no-eval
          // eval(code);
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
