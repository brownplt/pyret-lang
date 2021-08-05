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
  emptyChunk,
  UninitializedEditor,
} from './chunk';

import {
  Action,
  ChunksUpdate,
} from './action';

import {
  RHSObjects,
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
  chunkToRHS: Map<string, RHSObjects>,
  thisChunkRHSObjects: RHSObjects,
  enterNewline: boolean,
};

function mapStateToProps(state: State, ownProps: any): StateProps {
  const {
    chunks,
    enterNewline,
    chunkToRHS,
  } = state;

  const {
    index,
  } = ownProps;

  const thisChunkRHSObjects = chunkToRHS.get(chunks[index].id) ?? { outdated: true, objects: [] };

  return {
    chunks,
    thisChunkRHSObjects,
    enterNewline,
    chunkToRHS,
  };
}

type PropsFromReact = {
  index: number,
  focusNewChat: () => void,
};

type DispatchProps = {
  run: () => void,
  setChunks: (chunks: ChunksUpdate) => void,
  setChunkToRHS: (chunkToRHS: Map<string, RHSObjects>) => void,
};

function mapDispatchToProps(dispatch: (action: Action) => any): DispatchProps {
  return {
    run() {
      dispatch({ type: 'runSession', key: 'runProgram' });
    },
    setChunks(chunks: ChunksUpdate) {
      dispatch({ type: 'update', key: 'chunks', value: chunks });
    },
    setChunkToRHS(chunkToRHS: Map<string, RHSObjects>) {
      dispatch({ type: 'update', key: 'chunkToRHS', value: chunkToRHS });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type ChatProps = PropsFromRedux & DispatchProps & StateProps & PropsFromReact;

class Chat extends React.Component<ChatProps, any> {
  /* Used to autofocus this component when necessary */
  private input: React.RefObject<ReactCM>;

  constructor(props: ChatProps) {
    super(props);
    this.input = React.createRef();
  }

  /* A React component updates every time its props change. Since each chunk
     receives, as props, all other chunks, this would cause a lot of redundant
     re-rendering. This function attempts to determine when such prop updates
     can be ignored. It will probably need to be changed when new props are
     added or removed from this component. */
  shouldComponentUpdate(newProps: ChatProps) {
    const n = newProps;
    const o = this.props;

    if (n.thisChunkRHSObjects.outdated !== o.thisChunkRHSObjects.outdated) {
      return true;
    }

    if (n.thisChunkRHSObjects.objects !== o.thisChunkRHSObjects.objects) {
      return true;
    }

    if (n.chunks[n.index].errorState !== o.chunks[o.index].errorState) {
      return true;
    }

    if (n.chunks[n.index].selection !== o.chunks[o.index].selection) {
      return true;
    }

    if (n.index === o.index
      && n.chunks[n.index].editor.getValue() === o.chunks[o.index].editor.getValue()) {
      return false;
    }

    if (n.chunks[n.index].editor.getValue() === o.chunks[o.index].editor.getValue()
        && n.chunks[n.index].errorState === o.chunks[o.index].errorState) {
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
      errorState,
    } = chunks[index];

    if ('getDoc' in editor && errorState.status === 'succeeded') {
      const marks = editor.getDoc().getAllMarks();
      marks.forEach((m) => m.clear());
    } else if ('getDoc' in editor && errorState.status === 'failed') {
      const { highlights } = errorState;
      const marks = editor.getDoc().getAllMarks();
      marks.forEach((m) => m.clear());
      if (highlights.length > 0) {
        for (let i = 0; i < highlights.length; i += 1) {
          const doc = editor.getDoc();
          // lint errors are relative to the start of a chunk, compile errors
          // are relative to the start of the program
          if (errorState.effect === 'lint') {
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
          } else if (errorState.effect === 'compile') {
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
  }

  /* Called in response to an edit event, where `value` is the chunk's text
     after the edit. Marks updated chunks as not linted so that the running
     infastructure knows to lint them before compiling. */
  scheduleUpdate() {
    const {
      chunks,
      index,
      setChunks,
      chunkToRHS,
      setChunkToRHS,
    } = this.props;

    const { editor, id } = chunks[index];

    if ('getDoc' in editor) {
      const marks = editor.getDoc().getAllMarks();
      marks.forEach((m) => m.clear());
    }

    setChunks({
      chunk: {
        ...chunks[index],
        editor,
        errorState: { status: 'notLinted' },
      },
      modifiesText: true,
    });

    const withInvalidation = new Map(chunkToRHS);
    withInvalidation.set(id, {
      ...(withInvalidation.get(id) ?? { outdated: true, objects: [] }),
      outdated: true,
    });
    setChunkToRHS(withInvalidation);
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
    const {
      index,
    } = this.props;
    if (editor.getValue().trim() === '') {
      this.deleteChunk(index);
    }
  }

  insertAbove() {
    const {
      chunks,
      setChunks,
      index,
    } = this.props;
    const newChunks = [
      ...chunks.slice(0, index),
      emptyChunk({
        editor: { getValue: () => '', grabFocus: true },
      }),
      ...chunks.slice(index, chunks.length),
    ];
    setChunks({
      chunks: newChunks,
      modifiesText: true,
    });
  }

  /* Delete this chunk and move every chunk below it up by one */
  deleteChunk(index: number) {
    const {
      chunks,
      setChunks,
      run,
    } = this.props;
    const newChunks = [
      ...chunks.slice(0, index),
      ...chunks.slice(index + 1, chunks.length),
    ];
    setChunks({
      chunks: newChunks,
      modifiesText: true,
    });
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
      chunks, index,
    } = this.props;
    const { editor: initialEditor } = chunks[index];

    const {
      thisChunkRHSObjects,
    } = this.props;

    let chunkResultsPart = <></>;
    let displayCheckMark = false;
    const chunk = chunks[index];
    const { editor: chunkEditor } = chunk;

    if (chunk.errorState.status === 'failed' && 'markText' in chunkEditor) {
      chunkResultsPart = (
        <div
          style={{
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'flex-end',
            marginBottom: '0.5em',
          }}
        >
          {chunk.errorState.failures.map((failure, i) => (
            // eslint-disable-next-line
            <div className="chatitor-rhs" key={i}>
              <FailureComponent failure={failure} editor={chunkEditor} />
            </div>
          ))}
        </div>
      );
    } else {
      let rhs;
      const rhsObjects = thisChunkRHSObjects.objects;
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
        if (thisChunkRHSObjects.outdated) {
          rhs = <div style={{ float: 'right' }} className="chatitor-rhs pending"> . . . </div>;
        } else {
          displayCheckMark = true;
        }
      } else {
        const values = shown.map((val) => (
          <RHSObjectComponent
            key={val.key ?? 'no key for val?'}
            rhsObject={val}
            isSelected={false}
            className="chatitor-rhs"
            outdated={thisChunkRHSObjects.outdated}
          />
        ));
        const checkSummary = checks.length > 0
          ? (
            <CheckResults
            // Would love to have TypeScript obviate this `as`
              checks={checks as RHSCheck[]}
              outdated={thisChunkRHSObjects.outdated}
            />
          ) : '';
        rhs = [...values, checkSummary];
      }

      chunkResultsPart = (
        <div
          style={{
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'flex-end',
          }}
        >
          {rhs}
        </div>
      );
    }

    const chunkEditorPart = (
      <div style={{ width: '100%' }}>
        <ReactCM
          editorDidMount={(editor) => this.handleMount(editor as CMEditor, initialEditor)}
          options={{
            mode: 'pyret',
            theme: 'default',
            lineWrapping: true,
          }}
          onBeforeChange={() => {
            this.scheduleUpdate();
          }}
          onKeyDown={((editor: CMEditor, event: KeyboardEvent) => {
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

    return (
      <>
        <button className="insert-arrow" onClick={() => this.insertAbove()} type="button">
          &#10170;
        </button>
        { chunkEditorPart }
        { chunkResultsPart }
      </>
    );
  }
}

export default connector(Chat);
