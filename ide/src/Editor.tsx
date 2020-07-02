import React from 'react';
import './App.css';
import { connect, ConnectedProps } from 'react-redux';
import SplitterLayout from 'react-splitter-layout';
import { Chunk } from './chunk';
import * as State from './state';
import { EditorMode } from './state';
import Interaction from './Interaction';
import { TestResult } from './Check';
import DefChunks from './DefChunks';
import SingleCodeMirrorDefinitions from './SingleCodeMirrorDefinitions';
import Menu from './Menu';
import MenuBar from './MenuBar';
import Footer from './Footer';
import Dropdown from './Dropdown';
import DropdownOption from './DropdownOption';
import Header from './Header';
import InteractionError from './InteractionError';
import * as control from './control';
import 'react-splitter-layout/lib/index.css';
import * as action from './action';

type stateProps = {
  browseRoot: string,
  currentFileContents: undefined | string,
  definitionsHighlights: number[][],
  fontSize: number,
  stopify: boolean,
  checks: any[],
  interactions: { key: any, name: any, value: any }[],
  interactionErrors: any[],
  editorMode: EditorMode,
  chunks: Chunk[],
  running: boolean,
  compiling: boolean | 'out-of-date',
  linting: boolean,
  dropdownVisible: boolean,
  autoRun: boolean,
  typeCheck: boolean,
};

function mapStateToProps(state: State.State): stateProps {
  return {
    browseRoot: state.browseRoot,
    currentFileContents: state.currentFileContents,
    definitionsHighlights: state.definitionsHighlights,
    fontSize: state.fontSize,
    stopify: state.runKind === control.backend.RunKind.Async,
    checks: state.checks,
    interactions: state.interactions,
    interactionErrors: state.interactionErrors,
    editorMode: state.editorMode,
    chunks: state.chunks,
    running: state.running,
    compiling: state.compiling,
    linting: state.linting,
    dropdownVisible: state.dropdownVisible,
    autoRun: state.autoRun,
    typeCheck: state.typeCheck,
  };
}

type dispatchProps = {
  stop: () => void,
  run: () => void,
  updateContents: (contents: string) => void,
  setEditorMode: (mode: EditorMode) => void,
  setAutoRun: (autoRun: boolean) => void,
  setStopify: (stopify: boolean) => void,
  setTypeCheck: (typeCheck: boolean) => void,
  setDropdownVisible: (dropdownVisible: boolean) => void,
};

function mapDispatchToProps(dispatch: (action: action.Action) => any): dispatchProps {
  return {
    stop: () => dispatch({ type: 'enqueueEffect', effect: 'stop' }),
    run: () => dispatch({ type: 'enqueueEffect', effect: 'compile' }),
    updateContents: (contents: string) => dispatch({
      type: 'update',
      key: 'currentFileContents',
      value: contents,
    }),
    setEditorMode: (mode: EditorMode) => {
      dispatch({ type: 'update', key: 'editorMode', value: mode });
    },
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
      dispatch({ type: 'update', key: 'dropdownVisible', value: dropdownVisible });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;

control.installFileSystem();
control.loadBuiltins();

// type EditorProps = {};

type EditorProps = PropsFromRedux & dispatchProps & stateProps;

export class Editor extends React.Component<EditorProps, any> {
  // run = State.handleRun(this)
  // update = State.handleUpdate(this)
  // onTextEdit = State.handleTextEdit(this)
  // onChunkEdit = State.handleChunkEdit(this)
  // onTraverseDown = State.handleTraverseDown(this)
  // onTraverseUp = State.handleTraverseUp(this)
  // onExpandChild = State.handleExpandChild(this)
  // setEditorMode = State.handleSetEditorMode(this)
  // toggleDropdownVisibility = State.handleToggleDropdownVisibility(this)
  // toggleAutoRun = State.handleToggleAutoRun(this)
  // toggleStopify = State.handleToggleStopify(this)
  // toggleTypeCheck = State.handleToggleTypeCheck(this)
  // onDecreaseFontSize = State.handleDecreaseFontSize(this)
  // onIncreaseFontSize = State.handleIncreaseFontSize(this)
  // onResetFontSize = State.handleResetFontSize(this)
  // removeDropdown = State.handleRemoveDropdown(this)
  // setMessage = State.handleSetMessage(this)
  // stop = State.handleStop(this)

  // get isPyretFile() {
  //    return /\.arr$/.test(this.currentFile);
  // }

  // get currentFile() {
  //    return control.bfsSetup.path.join(
  //        ...this.state.currentFileDirectory,
  //        this.state.currentFileName);
  // }

  // get currentFileName() {
  //    return this.state.currentFileName;
  // }

  // get currentFileDirectory() {
  //    return control.bfsSetup.path.join(...this.state.currentFileDirectory);
  // }

  // get stopify() {
  //    return this.state.runKind === control.backend.RunKind.Async;
  // }

  // loadBuiltins = (e: React.MouseEvent<HTMLElement>): void => {
  //    control.loadBuiltins();
  // };

  // removeRootDirectory = (e: React.MouseEvent<HTMLElement>): void => {
  //    control.removeRootDirectory();
  // };

  makeHeaderButton = (text: string, enabled: boolean, onClick: () => void) => (
    <button
      className={(enabled ? 'run-option-enabled' : 'run-option-disabled')}
      onClick={onClick}
      type="button"
    >
      {text}
    </button>
  );

  makeDefinitions() {
    const {
      editorMode,
      currentFileContents,
      updateContents,
      definitionsHighlights,
    } = this.props;

    if (editorMode === EditorMode.Text) {
      return (
        <SingleCodeMirrorDefinitions
          text={currentFileContents || ''}
          onEdit={(contents: string) => updateContents(contents)}
          highlights={definitionsHighlights}
        />
      );
    }

    if (editorMode === EditorMode.Chunks) {
      return (
        <DefChunks />
      );
    }

    throw new Error('Unknown editor mode');
  }

  render() {
    const {
      fontSize,
      checks,
      interactions,
      interactionErrors,
      stopify,
      stop,
      run,
      running,
      compiling,
      linting,
      dropdownVisible,
      autoRun,
      typeCheck,
      setAutoRun,
      setStopify,
      setTypeCheck,
      setDropdownVisible,
    } = this.props;

    const interactionValues = (
      <div style={{ fontSize }}>
        <pre className="checks-area">
          { checks && checks.map((c: any) => <TestResult check={c} />)}
        </pre>
        <pre className="interactions-area">
          {
            interactions.map(
              (i: any) => (
                <Interaction
                  key={i.key}
                  name={i.name}
                  value={i.value}
                />
              ),
            )
          }
        </pre>
      </div>
    );

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

    /* const builtinsLoader =
     *     <button onClick={control.loadBuiltins}>
     *         Load Builtins
     *     </button>;
     */
    // const menu = (
    //   <Menu>
    //     <Tab
    //       name="fsBrowser"
    //       icon={
    //         <ReactSVG src="folderIcon.svg" />
    //       }
    //     >
    //       <FSBrowser />
    //     </Tab>
    //     <Tab
    //       name="options"
    //       icon={
    //         <ReactSVG src="gearIcon.svg" />
    //       }
    //     >
    //       {textEditor}
    //       {chunkEditor}
    //       {/* {builtinsLoader} */}
    //       <FontSize key="FontSize" />
    //     </Tab>
    //   </Menu>
    // );

    const rightHandSide = (
      <div className="interactions-area-container">
        {interactionErrors.length > 0 ? (
          <SplitterLayout
            vertical
            percentage
          >
            {interactionValues}
            <InteractionError fontSize={fontSize}>
              {interactionErrors}
            </InteractionError>
          </SplitterLayout>
        ) : interactionValues}
      </div>
    );

    const definitions = this.makeDefinitions();

    return (
      <div className="page-container">
        <Header>
          <div>
            <MenuBar />
          </div>
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
              >
                Run
              </button>
              <button
                type="button"
                className="run-options"
                onClick={() => setDropdownVisible(!dropdownVisible)}
                onBlur={() => setDropdownVisible(false)}
              >
                &#8628;
                {dropdown}
              </button>
            </div>
          </div>
        </Header>
        <div className="code-container">
          <Menu />
          <SplitterLayout
            vertical={false}
            percentage
          >
            <div
              className="edit-area-container"
              style={{ fontSize }}
            >
              {definitions}
            </div>
            {rightHandSide}
          </SplitterLayout>
        </div>
        <Footer message={`linting; ${linting}; compiling: ${compiling}; running: ${running}`} />
      </div>
    );
  }
}

export default connector(Editor);
