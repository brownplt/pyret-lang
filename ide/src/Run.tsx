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
  State,
  EditorResponseLoop,
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
  running: boolean,
  compiling: boolean | 'out-of-date',
  linting: boolean,
  chunks: Chunk[],
  editorResponseLoop: EditorResponseLoop,
};

function mapStateToProps(state: State): StateProps {
  const {
    runKind,
    dropdownVisible,
    editorLoopDropdownVisible,
    typeCheck,
    running,
    compiling,
    linting,
    chunks,
    editorResponseLoop,
  } = state;

  return {
    stopify: runKind === control.backend.RunKind.Async,
    dropdownVisible,
    editorLoopDropdownVisible,
    typeCheck,
    running,
    compiling,
    linting,
    chunks,
    editorResponseLoop,
  };
}

type PropsFromReact = {
};

type DispatchProps = {
  run: () => void,
  stop: () => void,
  setStopify: (stopify: boolean) => void,
  setTypeCheck: (typeCheck: boolean) => void,
  setDropdownVisible: (dropdownVisible: boolean) => void,
  setEditorLoopDropdownVisible: (visible: boolean) => void,
  setEditorResponseLoop: (ed: EditorResponseLoop) => void,
};

function mapDispatchToProps(dispatch: (action: Action) => void): DispatchProps {
  return {
    run: () => dispatch({ type: 'enqueueEffect', effect: { effectKey: 'saveFile' } }),
    stop: () => dispatch({ type: 'enqueueEffect', effect: { effectKey: 'stop' } }),
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
    setEditorLoopDropdownVisible: (visible: boolean) => {
      dispatch({ type: 'update', key: 'editorLoopDropdownVisible', value: visible });
    },
    setEditorResponseLoop: (ed: EditorResponseLoop) => {
      dispatch({ type: 'update', key: 'editorResponseLoop', value: ed });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type Props = PropsFromRedux & DispatchProps & StateProps & PropsFromReact;

function Run({
  run,
  stop,
  setStopify,
  setTypeCheck,
  setDropdownVisible,
  setEditorLoopDropdownVisible,
  setEditorResponseLoop,
  stopify,
  dropdownVisible,
  editorLoopDropdownVisible,
  typeCheck,
  running,
  compiling,
  linting,
  chunks,
  editorResponseLoop,
}: Props) {
  // TODO(alex): Better UI for selection
  const editorLoopDropdown = editorLoopDropdownVisible && (
    <Dropdown>
      <DropdownOption
        enabled={editorResponseLoop === EditorResponseLoop.Manual}
        onClick={() => setEditorResponseLoop(EditorResponseLoop.Manual)}
      >
        Manual
      </DropdownOption>

      <DropdownOption
        enabled={editorResponseLoop === EditorResponseLoop.AutoCompile}
        onClick={() => setEditorResponseLoop(EditorResponseLoop.AutoCompile)}
      >
        AutoCompile
      </DropdownOption>

      <DropdownOption
        enabled={editorResponseLoop === EditorResponseLoop.AutoCompileRun}
        onClick={() => setEditorResponseLoop(EditorResponseLoop.AutoCompileRun)}
      >
        AutoCompileRun
      </DropdownOption>

    </Dropdown>
  );
  const dropdown = dropdownVisible && (
    <Dropdown>
      <DropdownOption
        // eslint-disable-next-line react/jsx-boolean-value
        enabled={editorLoopDropdownVisible}
        onClick={() => setEditorLoopDropdownVisible(!editorLoopDropdownVisible)}
      >
        Editor Response Loop
        {editorLoopDropdown}
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
          onBlur={() => {
            setDropdownVisible(false);
            setEditorLoopDropdownVisible(false);
          }}
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
