import React from 'react';

import {
  connect,
  ConnectedProps,
} from 'react-redux';

import {
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
  autoRun: boolean,
  typeCheck: boolean,
  running: boolean,
  compiling: boolean | 'out-of-date',
  linting: boolean,
  chunks: Chunk[],
};

function mapStateToProps(state: State): StateProps {
  const {
    runKind,
    dropdownVisible,
    autoRun,
    typeCheck,
    running,
    compiling,
    linting,
    chunks,
  } = state;

  return {
    stopify: runKind === control.backend.RunKind.Async,
    dropdownVisible,
    autoRun,
    typeCheck,
    running,
    compiling,
    linting,
    chunks,
  };
}

type PropsFromReact = {
};

type DispatchProps = {
  run: () => void,
  stop: () => void,
  setAutoRun: (autoRun: boolean) => void,
  setStopify: (stopify: boolean) => void,
  setTypeCheck: (typeCheck: boolean) => void,
  setDropdownVisible: (dropdownVisible: boolean) => void,
};

function mapDispatchToProps(dispatch: (action: Action) => void): DispatchProps {
  return {
    run: () => dispatch({ type: 'enqueueEffect', effect: 'saveFile' }),
    stop: () => dispatch({ type: 'enqueueEffect', effect: 'stop' }),
    setAutoRun: (autoRun: boolean) => {
      dispatch({ type: 'update', key: 'autoRun', value: autoRun });
    },
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
      if (dropdownVisible) {
        dispatch({ type: 'update', key: 'focusedChunk', value: undefined });
      }
      dispatch({ type: 'update', key: 'dropdownVisible', value: dropdownVisible });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type Props = PropsFromRedux & DispatchProps & StateProps & PropsFromReact;

function Run({
  run,
  stop,
  setAutoRun,
  setStopify,
  setTypeCheck,
  setDropdownVisible,
  stopify,
  dropdownVisible,
  autoRun,
  typeCheck,
  running,
  compiling,
  linting,
  chunks,
}: Props) {
  const dropdown = dropdownVisible && (
    <Dropdown>
      <DropdownOption
        enabled={autoRun}
        onClick={() => setAutoRun(!autoRun)}
      >
        Auto Run
      </DropdownOption>
      <DropdownOption
        enabled={stopify}
        onClick={() => setStopify(!stopify)}
      >
        Stopify
      </DropdownOption>
      <DropdownOption
        enabled={typeCheck}
        onClick={() => setTypeCheck(!typeCheck)}
      >
        Type Check
      </DropdownOption>
    </Dropdown>
  );

  let percent = '100%';

  if (linting) {
    let chunksToLint = 0;

    chunks.forEach((chunk) => {
      if (chunk.errorState.status === 'notLinted') {
        chunksToLint += 1;
      }
    });

    const n = (50 / chunks.length) * (chunks.length - chunksToLint);

    percent = `${n}%`;
  }

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

  if (linting || compiling) {
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
          className="stop-available"
          onClick={stop}
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
          className="run-ready"
          type="button"
          onClick={run}
          style={{
            background: buttonBackground,
          }}
        >
          Run
        </button>
        <button
          type="button"
          className="run-options"
          onClick={() => setDropdownVisible(!dropdownVisible)}
          onBlur={() => setDropdownVisible(false)}
          style={{
            background: dropdownBackground,
          }}
        >
          &#8628;
          {dropdown}
        </button>
      </div>
    </div>
  );
}

export default connector(Run);
