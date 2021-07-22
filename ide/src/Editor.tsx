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
import { BackendCmd, EditorMode, MessageTabIndex } from './state';
import RHS from './RHS';
import RTMessageDisplay from './RTMessageDisplay';
import { RTMessages } from './rtMessages';
import DefChunks from './DefChunks';
import SingleCodeMirrorDefinitions from './SingleCodeMirrorDefinitions';
import Menu from './Menu';
import MenuBar from './MenuBar';
import Footer from './Footer';
import Header from './Header';
import InteractionError from './InteractionError';
import Run from './Run';
import * as control from './control';
import * as action from './action';
import 'react-tabs/style/react-tabs.css';
import 'react-splitter-layout/lib/index.css';
import EditorPlayground from './editor-playground/EditorPlayground';
import { NeverError } from './utils';
import Chatitor from './Chatitor';
import GlobalInteractions from './GlobalInteractions';

/*
function compileAndRun(name : string, source : string) : Promise<RunResult> {
  const p1 = control.backend.compile(name, source, options...);
  const p2 = p1.then((compileResult) => {
    return control.backend.run(name);
  });
  return p2;
}

function editorWhenYouClickRun() {
  const segments = getSegments();
  for(let [name, source] of segments) {
    const result = await compileAndRun(name, source);
    updateReduxState(result);
  }
}
*/

type StateProps = {
  browseRoot: string,
  currentFileContents: undefined | string,
  definitionsHighlights: number[][],
  fontSize: number,
  interactionErrors: any[],
  rtMessages: RTMessages,
  editorMode: EditorMode,
  chunks: Chunk[],
  compiling: boolean | 'out-of-date',
  linting: boolean,
  messageTabIndex: MessageTabIndex,
};

function mapStateToProps(state: State.State): StateProps {
  return {
    browseRoot: state.browseRoot,
    currentFileContents: state.currentFileContents,
    definitionsHighlights: state.definitionsHighlights,
    fontSize: state.fontSize,
    interactionErrors: state.interactionErrors,
    editorMode: state.editorMode,
    chunks: state.chunks,
    compiling: state.compiling,
    linting: state.linting,
    rtMessages: state.rtMessages,
    messageTabIndex: state.messageTabIndex,
  };
}

type DispatchProps = {
  run: () => void,
  updateContents: (contents: string) => void,
  setEditorMode: (mode: EditorMode) => void,
  setMessageTabIndex: (index: number) => void,
};

function mapDispatchToProps(dispatch: (action: action.Action) => any): DispatchProps {
  return {
    run: () => dispatch({ type: 'enqueueEffect', effect: { effectKey: 'initCmd', cmd: BackendCmd.Run } }),
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
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;

/* TODO(michael): these two lines would probably fit better in store.ts */
control.installFileSystem();
control.loadBuiltins();

type EditorProps = PropsFromRedux & DispatchProps & StateProps;

export class Editor extends React.Component<EditorProps, any> {
  componentDidMount() {
    document.body.addEventListener('copy', this.makeCopyHandler());
  }

  makeDefinitions() {
    const {
      editorMode,
      currentFileContents,
      updateContents,
      definitionsHighlights,
      run,
    } = this.props;

    switch (editorMode) {
      case EditorMode.Chatitor:
        return (
          <Chatitor />
        );
      case EditorMode.Embeditor:
        return (
          <EditorPlayground />
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
      case EditorMode.Chunks:
        return (
          <DefChunks />
        );
      default:
        throw new NeverError(editorMode);
    }
  }

  /* Returns a function suitable as a callback to a copy (ctrl-c) event handler.
     Ensures that highlighted text over multiple chunks is properly copied. Also
     ensures that the 'get shareable link' button copies its link when clicked. */
  makeCopyHandler() {
    const that = this;

    return (e: any) => {
      const {
        chunks,
        editorMode,
      } = that.props;

      if (editorMode === EditorMode.Text) {
        // We can rely on the browser's native copy here, since there's only one CodeMirror.
        return;
      }

      const shareableLink = document.getElementById('shareableLink');
      if (shareableLink) {
        // Rely on browser's native copy for the shareable link box. It should be autofocused.
        return;
      }

      /* If we're not in text mode, and the shareable link box isn't visible,
         then try to copy all of the highlighted text out of the chunks. */

      let data = '';

      chunks.forEach((chunk, i) => {
        const { editor } = chunk;

        if (!('getDoc' in editor)) {
          // TODO(luna): CHUNKSTEXT
          console.error('uninitialized editor makeCopyHandler(?)');
          return;
        }

        const doc = editor.getDoc();
        const selection = doc.getSelection();

        if (selection === '') {
          return;
        }

        data += selection;

        if (i !== chunks.length - 1) {
          data += '#.CHUNK#\n';
        }
      });

      e.clipboardData.setData('text/plain', data);
      e.preventDefault();
    };
  }

  render() {
    const {
      fontSize,
      interactionErrors,
      rtMessages,
      messageTabIndex,
      setMessageTabIndex,
      editorMode,
    } = this.props;

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
            {interactionErrors}
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
        <Header>
          <div>
            <MenuBar />
          </div>
          {editorMode === EditorMode.Chatitor ? <GlobalInteractions /> : <></>}
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
