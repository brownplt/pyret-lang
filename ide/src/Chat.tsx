/* The Chat component, one or more of which can be contained within the
   Chat component.

   Handles most of the chunk editor specific UI: selecting chunks; deleting
   chunks; handling edit events; handling key events, like arrow up, arrow down,
   enter, and backspace; lint/compile error message displays; inline mode value
   displays.

   Editing interactions are balanced between the underlying CodeMirror object
   and this component. For instance, we do not yet handle an "undo" feature that
   works across chunks, instead relying on a per-codemirror-instance undo. We
   do, however, override some of CodeMirror's default functionality, like
   highlighting (via click and drag). This is handled entirely by this
   component and the Redux store. */
import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { UnControlled as ReactCM } from 'react-codemirror2';
import { State } from './state';
import { CMEditor, isWrapFirst, isWrapLast } from './utils';

import {
  Chunk,
  isInitializedEditor,
  UninitializedEditor,
} from './chunk';

import {
  Action,
  ChunksUpdate,
} from './action';

import {
  isRHSCheck,
  RHSCheck,
  isLocation,
  isTrace,
} from './rhsObject';
import RHSObjectComponent from './RHSObjectComponent';
import FailureComponent from './FailureComponent';
import CheckResults from './CheckResults';

type StateProps = {
  chunks: Chunk[],
  enterNewline: boolean,
  technicallyOutdated: boolean,
};

function mapStateToProps(state: State, ownProps: any): StateProps {
  const {
    chunks,
    enterNewline,
    firstTechnicallyOutdatedSegment,
  } = state;

  const {
    index,
  } = ownProps;

  const technicallyOutdated = index >= firstTechnicallyOutdatedSegment;

  return {
    chunks,
    enterNewline,
    technicallyOutdated,
  };
}

type PropsFromReact = {
  index: number,
  focusNewChat: () => void,
};

type DispatchProps = {
  run: () => void,
  setChunks: (chunks: ChunksUpdate) => void,
  deleteChunk: (index: number) => void,
  insertChunk: (index: number) => void,
};

function mapDispatchToProps(dispatch: (action: Action) => any): DispatchProps {
  return {
    run() {
      dispatch({ type: 'run', key: 'runSegments' });
    },
    setChunks(chunks: ChunksUpdate) {
      dispatch({ type: 'update', key: 'chunks', value: chunks });
    },
    deleteChunk(index: number) {
      dispatch({ type: 'chunk', key: 'delete', index });
    },
    insertChunk(index: number) {
      dispatch({
        type: 'chunk', key: 'insert', index, grabFocus: true,
      });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type ChatProps = PropsFromRedux & DispatchProps & StateProps & PropsFromReact;

class Chat extends React.Component<ChatProps, any> {
  /* A React component updates every time its props change. Since each chunk
     receives, as props, all other chunks, this would cause a lot of redundant
     re-rendering. This function attempts to determine when such prop updates
     can be ignored. It will probably need to be changed when new props are
     added or removed from this component. */
  shouldComponentUpdate(newProps: ChatProps) {
    const n = newProps;
    const o = this.props;

    if (n.technicallyOutdated !== o.technicallyOutdated) {
      return true;
    }

    const nChunk = n.chunks[n.index];
    const oChunk = o.chunks[o.index];
    const nResult = nChunk.results;
    const oResult = oChunk.results;
    if (nResult.status !== oResult.status) {
      return true;
    }
    if (nChunk.outdated !== oChunk.outdated) {
      return true;
    }
    if (nResult.status === 'succeeded' && oResult.status === 'succeeded' && nResult.objects !== oResult.objects) {
      return true;
    }

    if (n.index === o.index
      && nChunk.editor.getValue() === oChunk.editor.getValue()) {
      return false;
    }

    if (nChunk.editor.getValue() === oChunk.editor.getValue()
        && nResult.status === oResult.status) {
      return false;
    }

    return true;
  }

  /* Fires imperitive updates at the underlying CodeMirror object to highlight
     selected text and mark errors. Also focuses this component if is focused in
     the Redux store. */
  componentDidUpdate() {
    const {
      chunks,
      index,
    } = this.props;

    const {
      editor,
      results,
    } = chunks[index];

    if ('getDoc' in editor && results.status === 'succeeded') {
      const marks = editor.getDoc().getAllMarks();
      marks.forEach((m) => m.clear());
    } else if ('getDoc' in editor && results.status === 'failed') {
      const { highlights } = results;
      const marks = editor.getDoc().getAllMarks();
      marks.forEach((m) => m.clear());
      if (highlights.length > 0) {
        for (let i = 0; i < highlights.length; i += 1) {
          // NOTE(luna): Now, highlights are created by FailureComponent - is
          // this rendering them twice? Is this working at all?
          const doc = editor.getDoc();
          const [l1, ch1, l2, ch2] = highlights[i];
          doc.markText(
            {
              line: l1 - 1,
              ch: ch1,
            },
            {
              line: l2 - 1,
              ch: ch2,
            },
            { className: 'styled-background-error' },
          );
        }
      }
    }
  }

  /* Called in response to an edit event, where `value` is the chunk's text
     after the edit. Marks updated chunks as not linted so that the running
     infastructure knows to lint them before compiling. */
  scheduleUpdate() {
    const {
      chunks,
      index,
      setChunks,
    } = this.props;

    const { editor } = chunks[index];

    if ('getDoc' in editor) {
      const marks = editor.getDoc().getAllMarks();
      marks.forEach((m) => m.clear());
    }

    setChunks({
      chunk: {
        ...chunks[index],
        editor,
        outdated: true,
      },
      modifiesText: true,
    });
  }

  /* Called in response to an arrow up event. Checks if the cursor is on the top
     line of a chunk and, if so, focuses the previous chunk. */
  handleArrowUp(editor: CMEditor, event: Event) {
    const {
      index,
      chunks,
    } = this.props;
    const pos = editor.getCursor();
    if (pos.line === 0 && isWrapFirst(editor, pos) && index > 0) {
      const newEditor = chunks[index - 1].editor;
      if ('focus' in newEditor) {
        newEditor.focus();
      }
      event.preventDefault();
    }
  }

  /* Called in response to an arrow down event. Checks if the cursor is on the
     bottom line of a chunk and, if so, focuses the subsequent chunk. */
  handleArrowDown(editor: CMEditor, event: Event) {
    const {
      index,
      chunks,
      focusNewChat,
    } = this.props;
    const pos = editor.getCursor();
    if (pos.line === chunks[index].editor.getValue().split('\n').length - 1 && isWrapLast(editor, pos)) {
      if (index < chunks.length - 1) {
        const newEditor = chunks[index + 1].editor;
        if ('focus' in newEditor) {
          newEditor.focus();
        }
      } else {
        focusNewChat();
      }
      event.preventDefault();
    }
  }

  /* Called in response to an Enter key event. Uses the Pyret mode for
     CodeMirror to determine if the cursor is at the end of a complete statement
     and, if so, instructs the linting infastructure to create a new chunk upon
     lint success. If shift+enter is pressed, no new chunk will be made. In
     either case, a run is triggered by saving the file. */
  handleEnter(editor: CMEditor, event: KeyboardEvent) {
    const {
      enterNewline,
      run,
    } = this.props;
    const pos = editor.getCursor();
    // eslint-disable-next-line
    const token = editor.getTokenAt(pos);
    const lineEndToken = editor.getTokenAt({ line: pos.line, ch: 99999 });
    // An enter anywhere on a single-line chat in which the ENTIRE chat is
    // codemirror-parsible
    // eslint-disable-next-line
    const singleLineEnter = editor.getValue().split('\n').length === 1 && lineEndToken.state.lineState.tokens.length === 0;
    const smartEnterCondition = singleLineEnter || token.state.lineState.tokens.length === 0;
    const smartEnter = smartEnterCondition && !enterNewline;
    if ((smartEnter || event.ctrlKey) && !event.shiftKey) {
      editor.getInputField().blur();
      run();
      event.preventDefault();
    }
  }

  handleBlur(editor: CMEditor) {
    const { index } = this.props;
    if (editor.getValue().trim() === '') {
      // Prepare chunk for reasonable state if restored by GLOBAL undo by doing
      // a LOCAL undo (presumably undoing a backspace)
      if (isInitializedEditor(editor)) {
        editor.undo();
      }
      this.deleteChunk(index);
    }
  }

  insertAbove() {
    const { insertChunk: insert, index } = this.props;
    insert(index);
  }

  /* Delete this chunk and move every chunk below it up by one */
  deleteChunk(index: number) {
    const { deleteChunk, run } = this.props;
    deleteChunk(index);
    run();
  }

  /* Called in response to a Delete key event. Deletes chunks in different ways
     depending on where the cursor is and which chunks (if any) are selected.

     This is a whole lot like handleBackspace, but with slightly different
     functionality to mimic the differences between deleting something with Delete
     versus deleting something with Backspace */
  handleDelete(event: Event) {
    const {
      chunks,
      index,
    } = this.props;
    if (chunks[index].editor.getValue().trim() === '') {
      this.deleteChunk(index);
      event.preventDefault();
    }
  }

  /* Called in response to a Backspace key event. Deletes chunks in different ways
     depending on where the cursor is and which chunks (if any) are selected.

     This is a whole lot like handleDelete, but with slightly different
     functionality to mimic the differences between deleting something with Delete
     versus deleting something with Backspace */
  handleBackspace(event: Event) {
    const {
      chunks, index,
    } = this.props;
    if (chunks[index].editor.getValue().trim() === '') {
      this.deleteChunk(index);
      event.preventDefault();
    }
  }

  handleMount(editor: CMEditor, initialEditor: CMEditor | UninitializedEditor) {
    const {
      setChunks,
      chunks,
      index,
    } = this.props;

    const marks = editor.getDoc().getAllMarks();
    marks.forEach((m) => m.clear());
    editor.setSize(null, 'auto');

    // Use value of ghost UninitializedEditor in real editor
    editor.setValue(initialEditor.getValue());
    if ('grabFocus' in initialEditor && initialEditor.grabFocus) {
      editor.getInputField().focus();
    }

    if (isInitializedEditor(initialEditor)) {
      editor.setHistory(initialEditor.getHistory());
    }

    setChunks({
      chunk: {
        ...chunks[index],
        editor,
      },
      modifiesText: false,
    });
  }

  render() {
    const {
      chunks, index, technicallyOutdated,
    } = this.props;
    let chunkResultsPart = <></>;
    let displayCheckMark = false;
    const chunk = chunks[index];
    const { editor: chunkEditor, results, outdated } = chunk;

    if (results.status === 'failed' && 'getDoc' in chunkEditor) {
      chunkResultsPart = (
        <div className="chat-result">
          {results.failures.map((failure, i) => (
            <div
              // eslint-disable-next-line
              key={i}
              style={{ border: `2px ${technicallyOutdated ? 'dashed' : 'solid'} #dc4064` }}
              className={`chatitor-rhs ${outdated ? 'outdated' : ''}`}
              title={technicallyOutdated ? 'value might be changed by earlier definition changes' : ''}
            >
              <FailureComponent failure={failure} editor={chunkEditor} />
            </div>
          ))}
        </div>
      );
    } else if (results.status === 'succeeded') {
      let rhs;
      const rhsObjects = results.objects;
      const partiallyOutdated = technicallyOutdated;
      // TODO(luna): more principled
      // const isDataDefinition = rhsObjects.filter((r) => !isLocation(r)).length === 0
      //         && rhsObjects.filter((r) => isLocation(r) && r.name.startsWith('is-')).length > 0;
      const shown = rhsObjects.filter((r) => (
        // location for function is mostly noise
        !(isLocation(r) && typeof r.value === 'function')
        // checks handled separately and grouped
        && !isRHSCheck(r)
        // undefined shows up sometimes go figure
        && !(isTrace(r) && typeof r.value === 'undefined')));
      const checks = rhsObjects.filter((r) => isRHSCheck(r));
      if (shown.length + checks.length === 0) {
        if (partiallyOutdated) {
          // TODO(luna): This styling feels... wrong, even though it follows the
          // rules of the other ones. Think about how it should look?
          rhs = <div style={{ float: 'right' }} className={`chatitor-rhs pending partially-outdated ${outdated ? 'outdated' : ''}`}> . . . </div>;
        } else {
          displayCheckMark = true;
        }
      } else {
        const values = shown.map((val) => (
          <RHSObjectComponent
            key={val.key ?? 'no key for val?'}
            rhsObject={val}
            isSelected={false}
            className={`chatitor-rhs ${partiallyOutdated ? 'partially-outdated' : ''}`}
            title={partiallyOutdated ? 'value might be changed by earlier definition changes' : ''}
            outdated={outdated}
          />
        ));
        const checkSummary = checks.length > 0
          ? (
            <CheckResults
            // Would love to have TypeScript obviate this `as`
              checks={checks as RHSCheck[]}
              outdated={outdated}
              className={`chatitor-rhs ${partiallyOutdated ? 'partially-outdated' : ''}`}
              title={partiallyOutdated ? 'value might be changed by earlier definition changes' : ''}
            />
          ) : '';
        rhs = [...values, checkSummary];
      }

      chunkResultsPart = (
        <div className="chat-result">
          {rhs}
        </div>
      );
    }

    const chunkEditorPart = (
      <div className="chat-editor-wrapper">
        <ReactCM
          editorDidMount={(editor) => this.handleMount(editor as CMEditor, chunkEditor)}
          options={{
            mode: 'pyret',
            theme: 'default',
            lineWrapping: true,
          }}
          onBeforeChange={() => {
            this.scheduleUpdate();
          }}
          onKeyDown={((editor: CMEditor, event: KeyboardEvent) => {
            // Ctrl-Z, Ctrl-Y/Ctrl-Shift-Z should not occur on document/chunks
            event.stopPropagation();
            switch ((event as any).key) {
              case 'Enter':
                this.handleEnter(editor, event);
                break;
              case 'Backspace':
                this.handleBackspace(event);
                break;
              case 'Delete':
                this.handleDelete(event);
                break;
              case 'ArrowUp':
                this.handleArrowUp(editor, event);
                break;
              case 'ArrowDown':
                this.handleArrowDown(editor, event);
                break;
              case 'Escape':
                editor.getInputField().blur();
                break;
              default:
            }
          }) as any}
          onBlur={((editor: CMEditor) => this.handleBlur(editor)) as any}
          className={displayCheckMark ? 'chat checkmark' : 'chat'}
        />
      </div>
    );

    const addButtonTitle = 'Insert new chat here';

    return (
      <>
        <button title={addButtonTitle} className="insert-arrow" onClick={() => this.insertAbove()} type="button">
          +
        </button>
        <div className="chat-and-result">
          { chunkEditorPart }
          { chunkResultsPart }
        </div>
      </>
    );
  }
}

export default connector(Chat);
