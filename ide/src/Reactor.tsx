/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */

import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { Rnd } from 'react-rnd';
import { X } from 'react-feather';
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
    '$interactNoPauseResume': (insertNode: (node: any, title: string, setupClose: (close: () => void) => void) => void) => any,
    '$shutdown': () => void,
  },
  convert: (value: any) => any,
};

type PropsFromRedux = ConnectedProps<typeof connector>;
type Props = PropsFromRedux & DispatchProps & StateProps & PropsFromReact;

function Reactor({ reactor, convert }: Props) {
  const [node, setNode]: [any, (node: any) => void] = React.useState(false);
  const [title, setTitle]: [string, (title: string) => void] = React.useState('Reactor');
  const [open, setOpen]: [boolean, (open: boolean) => void] = React.useState(false as boolean);
  const close = () => {
    reactor.$shutdown();
    setOpen(false);
  };

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
    <div>
      <div
        onClick={() => {
          if (open === false) {
            try {
              reactor.$interactNoPauseResume(
                (newNode, newTitle, setupClose) => {
                  setupClose(close);
                  setNode(newNode);
                  setTitle(newTitle);
                  setOpen(true);
                },
              );
            } catch (e) {
              console.log('failed with', e);
            }
          }
        }}
      >
        {value}
      </div>
      {open && (
        <Rnd
          style={{
            background: 'white',
            border: '2px solid #222222',
          }}
          minWidth="auto"
          minHeight="auto"
          dragHandleClassName="reactor-drag-handle"
          disableDragging={!open}
        >
          <div
            style={{
              background: 'gray',
              color: 'white',
              fontSize: '2em',
              paddingRight: '0.5em',
              display: 'flex',
              alignItems: 'center',
              cursor: 'move',
            }}
            className="reactor-drag-handle"
            id="reactor-drag-handle"
          >
            <button
              type="button"
              onClick={close}
              style={{
                width: '2em',
                height: '2em',
                background: 'white',
                margin: '0.5em',
                padding: '0',
              }}
            >
              <X />
            </button>
            {title}
          </div>
          <div
            style={{
              padding: '1em',
            }}
            ref={((div) => {
              if (div !== null && node !== false) {
                div.appendChild(node);
                (div.children[0].children[0] as any).focus();
                console.log(div);
              }
            })}
          />
        </Rnd>
      )}
    </div>
  );
}

export default connector(Reactor);
