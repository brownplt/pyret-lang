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
  BackendCmd,
  EditorMode,
  EditorResponseLoop,
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
  running: boolean,
  compiling: boolean | 'out-of-date',
  chunks: Chunk[],
  editorResponseLoop: EditorResponseLoop,
  editorMode: EditorMode,
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
    editorResponseLoop,
    editorMode,
  } = state;

  return {
    stopify: runKind === control.backend.RunKind.Async,
    dropdownVisible,
    editorLoopDropdownVisible,
    typeCheck,
    running,
    compiling,
    chunks,
    editorResponseLoop,
    editorMode,
  };
}

type PropsFromReact = {
};

type DispatchProps = {
  compile: () => void,
  run: () => void,
  runSession: () => void,
  stop: () => void,
  stopSession: () => void,
  setStopify: (stopify: boolean) => void,
  setTypeCheck: (typeCheck: boolean) => void,
  setDropdownVisible: (dropdownVisible: boolean) => void,
  setEditorLoopDropdownVisible: (visible: boolean) => void,
  setEditorResponseLoop: (ed: EditorResponseLoop) => void,
};

function mapDispatchToProps(dispatch: (action: Action) => void): DispatchProps {
  return {
    compile: () => dispatch({ type: 'enqueueEffect', effect: { effectKey: 'initCmd', cmd: BackendCmd.Compile } }),
    run: () => dispatch({ type: 'run', key: 'runProgram' }),
    runSession: () => dispatch({ type: 'run', key: 'runSegments' }),
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
  compile,
  run,
  runSession,
  stop,
  stopSession,
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
  editorResponseLoop,
  editorMode,
}: Props) {
  // TODO(alex): Better UI for selection
  const editorLoopDropdown = editorLoopDropdownVisible && (
    <Dropdown>
      <DropdownOption
        id="OptionERLManual"
        enabled={editorResponseLoop === EditorResponseLoop.Manual}
        onClick={() => setEditorResponseLoop(EditorResponseLoop.Manual)}
      >
        Manual
      </DropdownOption>

      <DropdownOption
        id="OptionERLAutoCompile"
        enabled={editorResponseLoop === EditorResponseLoop.AutoCompile}
        onClick={() => setEditorResponseLoop(EditorResponseLoop.AutoCompile)}
      >
        AutoCompile
      </DropdownOption>

      <DropdownOption
        id="OptionERLAutoCompileRun"
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
        id="OptionEditorResponseLoop"
        // eslint-disable-next-line react/jsx-boolean-value
        enabled={editorLoopDropdownVisible}
        onClick={() => setEditorLoopDropdownVisible(!editorLoopDropdownVisible)}
      >
        Editor Response Loop
        {editorLoopDropdown}
      </DropdownOption>
      <DropdownOption
        id="OptionStopifyButton"
        enabled={stopify}
        onClick={() => setStopify(!stopify)}
      >
        Stopify
      </DropdownOption>
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
        {editorMode !== EditorMode.Chatitor
          ? (
            <button
              id="CompileButton"
              className="compile-ready"
              type="button"
              onClick={compile}
              style={{
                background: buttonBackground,
              }}
            >
              {
              // TODO(alex): figure out button style/margins
              // TODO(alex): compile button has a persisting black outline (unlike the run button)
              // TODO(alex): figure out compilation/run-ready progress bar
            }
              Compile
            </button>
          )
          : ''}
        <button
          id="RunButton"
          className="run-ready"
          type="button"
          onClick={editorMode === EditorMode.Chatitor ? runSession : run}
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
