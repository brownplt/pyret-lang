/* A widget for displaying reactors. Reactor documentation:
   https://www.pyret.org/docs/latest/reactors.html.

   Reactors are created in RenderedValue.tsx

   This component is quite hacky. The actual HTML and event listeners of a
   running reactor are created in
   pyret-lang/src/runtime/{reactors.arr.ts,world.js,world-lib.js}. This
   component just wraps that value in a draggable dialog.

   Known issues:
   - it isn't clear that reactors are clickable.
   - keyboard event listeners do not always keep focus correctly. This is a problem with
     the draggable container we are using. focus-trap-react may be useful here.
   - an open reactor does not close itself on re-run of the program
   - reactors can not be fullscreened
   - reactors can be dragged off the visible range of the page
   - reactors do not have a maximum size */

/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */

import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { Rnd } from 'react-rnd';
import { X } from 'react-feather';
import { runStopify } from '../runner';
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
    '$interact': (insertNode: (node: any, title: string, setupClose: (close: () => void) => void) => void) => any,
    '$shutdown': () => void,
  },
  RenderedValue: React.ReactType,
};

type PropsFromRedux = ConnectedProps<typeof connector>;
type Props = PropsFromRedux & DispatchProps & StateProps & PropsFromReact;

function Reactor({ reactor, RenderedValue }: Props) {
  const [node, setNode]: [any, (node: any) => void] = React.useState(false);
  const [title, setTitle]: [string, (title: string) => void] = React.useState('Reactor');
  const [open, setOpen]: [boolean, (open: boolean) => void] = React.useState(false as boolean);
  const [value, setValue]: [any, (newValue: any) => void] = React.useState(undefined);
  const close = () => {
    runStopify(() => { reactor.$shutdown(); }).then(() => setOpen(false));
  };

  function runDraw(r : any) : Promise<any> {
    return runStopify(() => r.draw());
  }

  function setInitialValue() {
    try {
      runDraw(reactor).then((v : { value: any }) => {
        console.log('setInitialValue: covert', v.value);
        setValue(v.value);
      });
    } catch (e) {
      console.log('failed draw with', e);
      setValue(reactor['get-value']());
    }
  }
  if (typeof value === 'undefined') {
    setInitialValue();
  }
  return (
    <div>
      <div
        style={{
          border: '1px solid black'
        }}
        onClick={() => {
          if (open === false) {
            try {
              runStopify(() => {
                reactor.$interact(
                  (newNode, newTitle, setupClose) => {
                    setupClose(close);
                    setNode(newNode);
                    setTitle(newTitle);
                    setOpen(true);
                  },
                );
              });
            } catch (e) {
              console.log('failed with', e);
            }
          }
        }}
      >
        <p>⚛ Click to start</p>
        {typeof value === 'undefined' ? 'Initializing...' : <RenderedValue value={value} />}
      </div>
      {open && (
        <Rnd
          style={{
            background: 'white',
            border: '2px solid #222222',
            zIndex: 9999,
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
                // (div.children[0].children[0] as any).focus();
              }
            })}
          />
        </Rnd>
      )}
    </div>
  );
}

export default connector(Reactor);
