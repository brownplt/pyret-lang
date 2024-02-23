/* Wires the ide together. Most of the interesting functionality of the IDE (chunks,
   run button, etc.) come from this class' imports instead of being implemented here.

   That being said, this class does do some things. For instance, it:
   - creates a copy (ctrl-c) event handler
   - lays out imported components */

import React from 'react';
import './App.css';
import { connect, ConnectedProps } from 'react-redux';
import SplitterLayout from 'react-splitter-layout';
import { Chunk } from './chunk';
import * as State from './state';
import { EditorMode, MessageTabIndex } from './state';
import { RTMessages } from './rtMessages';
import SingleCodeMirrorDefinitions from './SingleCodeMirrorDefinitions';
import Menu from './Menu';
import MenuBar from './MenuBar';
import Footer from './Footer';
import Header from './Header';
import Run from './Run';
import * as action from './action';
import 'react-tabs/style/react-tabs.css';
import 'react-splitter-layout/lib/index.css';
import { NeverError } from './utils';
import Chatitor from './Chatitor';
import Examplaritor from './Examplaritor';
import FileSync from './FileSync';

type StateProps = {
  browseRoot: string,
  browsePath: string,
  currentFileContents: undefined | string,
  definitionsHighlights: number[][],
  fontSize: number,
  headerMessage: string,
  rtMessages: RTMessages,
  editorMode: EditorMode,
  menuTabVisible: false | number,
  chunks: Chunk[],
  compiling: boolean | 'out-of-date',
  messageTabIndex: MessageTabIndex,
  projectState: State.ProjectState,
};

function mapStateToProps(state: State.State): StateProps {
  return {
    browseRoot: state.browseRoot,
    browsePath: state.browsePath,
    currentFileContents: state.currentFileContents,
    definitionsHighlights: state.definitionsHighlights,
    fontSize: state.fontSize,
    headerMessage: state.headerMessage,
    editorMode: state.editorMode,
    menuTabVisible: state.menuTabVisible,
    chunks: state.chunks,
    compiling: state.compiling,
    rtMessages: state.rtMessages,
    messageTabIndex: state.messageTabIndex,
    projectState: state.projectState,
  };
}

type DispatchProps = {
  runProgram: () => void,
  update: (kv : Partial<State.State>) => void,
  updateContents: (contents: string) => void,
  setEditorMode: (mode: EditorMode) => void,
  setMessageTabIndex: (index: number) => void,
  loadFile: () => void,
};

function mapDispatchToProps(dispatch: (action: action.Action) => any): DispatchProps {
  return {
    runProgram: () => dispatch({ type: 'run', key: 'runProgram' }),
    update: (kv) => dispatch({ type: 'update', key: 'updater', value: (s : State.State) => ({ ...s, ...kv }) }),
    updateContents: (contents: string) => dispatch({
      type: 'update',
      key: 'currentFileContents',
      value: contents,
    }),
    setEditorMode: (mode: EditorMode) => {
      dispatch({ type: 'update', key: 'editorMode', value: mode });
    },
    setMessageTabIndex: (index: number) => {
      if (index === MessageTabIndex.ErrorMessages) {
        dispatch({
          type: 'update',
          key: 'messageTabIndex',
          value: MessageTabIndex.ErrorMessages,
        });
      } else if (index === MessageTabIndex.RuntimeMessages) {
        dispatch({
          type: 'update',
          key: 'messageTabIndex',
          value: MessageTabIndex.RuntimeMessages,
        });
      } else {
        throw new Error(`Unknown message tab index: ${index}`);
      }
    },
    loadFile() {
      dispatch({ type: 'enqueueEffect', effect: { effectKey: 'loadFile' } });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;

type EditorProps = PropsFromRedux & DispatchProps & StateProps;

class Editor extends React.Component<EditorProps, any> {
  makeDefinitions() {
    const {
      editorMode,
      currentFileContents,
      updateContents,
      definitionsHighlights,
      runProgram: run,
    } = this.props;

    switch (editorMode) {
      case EditorMode.Chatitor:
        return (
          <Chatitor />
        );
      case EditorMode.Text:
        return (
          <SingleCodeMirrorDefinitions
            text={currentFileContents || ''}
            onEdit={(contents: string) => updateContents(contents)}
            onInit={(editor: CodeMirror.Editor) => this.props.update({ definitionsEditor: editor })}
            highlights={definitionsHighlights}
            run={run}
          />
        );
      case EditorMode.Examplaritor:
        return (
          <Examplaritor />
        );
      default:
        throw new NeverError(editorMode);
    }
  }

  render() {
    const {
      fontSize,
      headerMessage,
      editorMode,
      projectState,
    } = this.props;

    if (projectState.type === 'gdrive-pending') {
      return <div className="loading">Loading from Drive...</div>;
    }

    const rightHandSide = (
      <div className="interactions-area-container">
        <Chatitor />
      </div>
    );

    const definitions = this.makeDefinitions();

    const mainSplit = editorMode === EditorMode.Chatitor ? (
      <div
        className="edit-area-container"
        style={{ fontSize, width: '100%' }}
      >
        {definitions}
      </div>
    ) : (
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
    );

    let mainContent;
    if (projectState.type === 'gdrive' && projectState.structure.files.length === 0) {
      mainContent = <div>There are no files; use the Files menu to create one to get started.</div>;
    } else {
      mainContent = mainSplit;
    }

    return (
      <div className="page-container">
        <FileSync />
        <Header>
          <div>
            <MenuBar />
          </div>
          <span>{headerMessage}</span>
          <Run />
        </Header>
        <div className="code-container">
          <Menu />
          {mainContent}
        </div>
        <Footer />
      </div>
    );
  }
}

export default connector(Editor);
