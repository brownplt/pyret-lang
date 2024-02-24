/* Creates the run button at the top right of the header. Handles running the
   program when the button is clicked. Also handles animations (graying out /
   blueness) when the program is compiling.

   The run dropdown is part of this component, but the code that creates it is
   found in DropDown.tsx, though its contents /are/ created here. */

import React from 'react';

import {
  connect,
  ConnectedProps,
} from 'react-redux';

import {
  EditorMode,
  State,
} from './state';

import {
  Action,
} from './action';

import {
  Chunk,
} from './chunk';

import Dropdown from './Dropdown';
import DropdownOption from './DropdownOption';

import * as control from './control';

type StateProps = {
  stopify: boolean,
  dropdownVisible: boolean,
  editorLoopDropdownVisible: boolean,
  typeCheck: boolean,
  isRunning: boolean,
  compiling: boolean | 'out-of-date',
  chunks: Chunk[],
  editorMode: EditorMode,
  developerMode: boolean,
};

function mapStateToProps(state: State): StateProps {
  const {
    runKind,
    dropdownVisible,
    editorLoopDropdownVisible,
    typeCheck,
    running,
    compiling,
    chunks,
    editorMode,
    developerMode,
  } = state;

  return {
    stopify: runKind === control.backend.RunKind.Async,
    dropdownVisible,
    editorLoopDropdownVisible,
    typeCheck,
    isRunning: running.type !== 'idle',
    compiling,
    chunks,
    editorMode,
    developerMode,
  };
}

type PropsFromReact = {
};

type DispatchProps = {
  run: (editorMode: EditorMode) => void,
  // runSession: () => void,
  stop: () => void,
  stopSession: () => void,
  setStopify: (stopify: boolean) => void,
  setTypeCheck: (typeCheck: boolean) => void,
  setDropdownVisible: (dropdownVisible: boolean) => void,
};

function mapDispatchToProps(dispatch: (action: Action) => void): DispatchProps {
  return {
    run: (editorMode) => {
      dispatch({ type: 'run' });
    },
    // runSession: () => dispatch({ type: 'run', key: 'runSegments' }),
    stop: () => dispatch({ type: 'enqueueEffect', effect: { effectKey: 'stop' } }),
    stopSession: () => dispatch({ type: 'stopSession' }),
    setStopify: (stopify: boolean) => {
      if (stopify) {
        dispatch({ type: 'update', key: 'runKind', value: control.backend.RunKind.Async });
      } else {
        dispatch({ type: 'update', key: 'runKind', value: control.backend.RunKind.Sync });
      }
    },
    setTypeCheck: (typeCheck: boolean) => {
      dispatch({ type: 'update', key: 'typeCheck', value: typeCheck });
    },
    setDropdownVisible: (dropdownVisible: boolean) => {
      dispatch({ type: 'update', key: 'dropdownVisible', value: dropdownVisible });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type Props = PropsFromRedux & DispatchProps & StateProps & PropsFromReact;

function Run({
  run,
  // runSession,
  stop,
  stopSession,
  setStopify,
  setTypeCheck,
  setDropdownVisible,
  stopify,
  dropdownVisible,
  editorLoopDropdownVisible,
  typeCheck,
  isRunning: running,
  compiling,
  editorMode,
  developerMode,
}: Props) {
  const dropdown = dropdownVisible && developerMode && (
    <Dropdown>
      { developerMode && (
        <DropdownOption
          id="OptionStopifyButton"
          enabled={stopify}
          onClick={() => setStopify(!stopify)}
        >
          Stopify
        </DropdownOption>
      )}
      <DropdownOption
        id="OptionTypeCheckButton"
        enabled={typeCheck}
        onClick={() => setTypeCheck(!typeCheck)}
      >
        Type Check
      </DropdownOption>
    </Dropdown>
  );

  let percent = '100%';

  if (compiling) {
    percent = '75%';
  }

  if (running) {
    percent = '100%';
  }

  let buttonBackground = `linear-gradient(90deg, #317bcf ${percent}, gray ${percent})`;

  if (running) {
    buttonBackground = 'green';
  }

  let dropdownBackground = '#317bcf';

  if (compiling) {
    dropdownBackground = 'gray';
  }

  if (running) {
    dropdownBackground = 'green';
  }

  return (
    <div
      style={{
        height: '100%',
      }}
    >
      {stopify && running ? (
        <button
          id="StopButton"
          className="stop-available"
          onClick={editorMode === EditorMode.Chatitor ? stopSession : stop}
          type="button"
        >
          Stop
        </button>
      ) : (
        <button
          className="stop-unavailable"
          type="button"
        >
          Stop
        </button>
      )}
      <div
        className="run-container"
      >
        <button
          id="RunButton"
          className="run-ready"
          type="button"
          onClick={() => run(editorMode)}
          style={{
            background: buttonBackground,
          }}
        >
          Update responses
        </button>
        { developerMode && (
          <button
            type="button"
            className="run-options"
            onClick={() => setDropdownVisible(!dropdownVisible)}
            onBlur={() => {
              setDropdownVisible(false);
            }}
            style={{
              background: dropdownBackground,
            }}
          >
            &#8628;
            {dropdown}
          </button>
        )}
      </div>
    </div>
  );
}

export default connector(Run);
