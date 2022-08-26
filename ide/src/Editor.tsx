/* Wires the ide together. Most of the interesting functionality of the IDE (chunks,
   run button, etc.) come from this class' imports instead of being implemented here.

   That being said, this class does do some things. For instance, it:
   - creates a copy (ctrl-c) event handler
   - lays out imported components */

import React from 'react';
import './App.css';
import { connect, ConnectedProps } from 'react-redux';
import {
  Tab,
  Tabs,
  TabList,
  TabPanel,
} from 'react-tabs';
import SplitterLayout from 'react-splitter-layout';
import { Chunk } from './chunk';
import * as State from './state';
import { EditorMode, MessageTabIndex } from './state';
import RHS from './RHS';
import RTMessageDisplay from './RTMessageDisplay';
import { RTMessages } from './rtMessages';
import SingleCodeMirrorDefinitions from './SingleCodeMirrorDefinitions';
import Menu from './Menu';
import MenuBar from './MenuBar';
import Footer from './Footer';
import Header from './Header';
import InteractionError from './InteractionError';
import Run from './Run';
import * as action from './action';
import 'react-tabs/style/react-tabs.css';
import 'react-splitter-layout/lib/index.css';
import Embeditor from './embeditor/Embeditor';
import { NeverError } from './utils';
import Chatitor from './Chatitor';
import FailureComponent from './FailureComponent';
import GoogleDrive from './Drive';
import FileSync from './FileSync';
import { populateFromDrive } from './reducer';

type StateProps = {
  browseRoot: string,
  browsePath: string,
  currentFileContents: undefined | string,
  definitionsHighlights: number[][],
  fontSize: number,
  interactionErrors: any[],
  rtMessages: RTMessages,
  editorMode: EditorMode,
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
    interactionErrors: state.interactionErrors,
    editorMode: state.editorMode,
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
  constructor(props : EditorProps) {
    super(props);
    const drive = new GoogleDrive();
    const params = new URLSearchParams(window.location.search);
    const folderId = params.get('folder');

    if (folderId !== null) {
      this.props.update({
        projectState: { type: 'gdrive-pending' },
      });
      drive.getFileStructureFor(folderId)
        .then((structure) => {
          populateFromDrive(structure);
          this.props.update({
            projectState: { type: 'gdrive', structure },
            browsePath: `/google-drive/${folderId}/${structure.name}`,
          });
          this.props.loadFile();
          console.log('Structure is: ', structure);
        });
    } else {
      this.props.loadFile();
    }
  }

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
      case EditorMode.Embeditor:
        return (
          <Embeditor />
        );
      case EditorMode.Text:
        return (
          <SingleCodeMirrorDefinitions
            text={currentFileContents || ''}
            onEdit={(contents: string) => updateContents(contents)}
            highlights={definitionsHighlights}
            run={run}
          />
        );
      default:
        throw new NeverError(editorMode);
    }
  }

  render() {
    const {
      fontSize,
      interactionErrors,
      rtMessages,
      messageTabIndex,
      setMessageTabIndex,
      editorMode,
      projectState,
    } = this.props;

    if (projectState.type === 'gdrive-pending') {
      return <div>Loading from drive</div>;
    }
    const interactionValues = (
      <RHS />
    );

    // TODO(alex): interaction errors DOM node not extending the entire plane
    //   Caused by the tab panel implementation which shrinks to the size of the content
    const rhsMessages = (
      <Tabs
        selectedIndex={messageTabIndex}
        onSelect={(tabIndex) => setMessageTabIndex(tabIndex)}
      >
        <TabList>
          <Tab>Message</Tab>
          <Tab>Errors</Tab>
        </TabList>

        <TabPanel>
          <RTMessageDisplay />
        </TabPanel>

        <TabPanel className="interaction-error">
          <InteractionError fontSize={fontSize}>
            {interactionErrors.map((f) => {
              let failure;
              try {
                failure = JSON.parse(f);
              } catch (e) {
                return f;
              }
              return <FailureComponent failure={failure} />;
            })}
          </InteractionError>
        </TabPanel>
      </Tabs>
    );

    const hasMessages = (interactionErrors.length > 0) || (rtMessages.messages.length > 0);

    const rightHandSide = (
      <div className="interactions-area-container">
        {hasMessages ? (
          <SplitterLayout
            vertical
            percentage
          >
            {interactionValues}
            {rhsMessages}
          </SplitterLayout>
        ) : interactionValues}
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

    return (
      <div className="page-container">
        <FileSync />
        <Header>
          <div>
            <MenuBar />
          </div>
          <Run />
        </Header>
        <div className="code-container">
          <Menu />
          {mainSplit}
        </div>
        <Footer />
      </div>
    );
  }
}

export default connector(Editor);
