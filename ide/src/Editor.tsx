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
import { EditorMode } from './state';
import RHS from './RHS';
import RTMessageDisplay from './RTMessageDisplay';
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

type StateProps = {
  browseRoot: string,
  currentFileContents: undefined | string,
  definitionsHighlights: number[][],
  fontSize: number,
  interactionErrors: any[],
  editorMode: EditorMode,
  chunks: Chunk[],
  compiling: boolean | 'out-of-date',
  linting: boolean,
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
  };
}

type DispatchProps = {
  updateContents: (contents: string) => void,
  setEditorMode: (mode: EditorMode) => void,
};

function mapDispatchToProps(dispatch: (action: action.Action) => any): DispatchProps {
  return {
    updateContents: (contents: string) => dispatch({
      type: 'update',
      key: 'currentFileContents',
      value: contents,
    }),
    setEditorMode: (mode: EditorMode) => {
      dispatch({ type: 'update', key: 'editorMode', value: mode });
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

        if (editor === false) {
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
    } = this.props;

    const interactionValues = (
      <RHS />
    );

    const rhsMessages = (
      <Tabs defaultIndex={interactionErrors.length > 0 ? 1 : 0}>
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

    const hasMessages = interactionErrors.length > 0;

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

    return (
      <div className="page-container">
        <Header>
          <div>
            <MenuBar />
          </div>
          <Run />
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
        <Footer />
      </div>
    );
  }
}

export default connector(Editor);
