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
import { UnControlled as CodeMirror } from 'react-codemirror2';
import { BackendCmd, State, EditorResponseLoop } from './state';

import {
  Chunk,
  getStartLineForIndex,
  removeAllSelections,
} from './chunk';

import {
  Action,
  ChunksUpdate,
} from './action';

import { Effect } from './effect';
import {
  RHSObjects,
  isRHSCheck,
  RHSCheck,
  isLocation,
} from './rhsObject';
import RHSObjectComponent from './RHSObjectComponent';
import LinkedCodeMirror from './LinkedCodeMirror';
import FailureComponent from './FailureComponent';
import CheckResults from './CheckResults';

type StateProps = {
  chunks: Chunk[],
  rhs: RHSObjects,
  firstSelectedChunkIndex: false | number,
  currentFile: string,
  chunkToRHS: RHSObjects[],
  thisChunkRHSObjects: RHSObjects,
  displayResultsInline: boolean,
  editorResponseLoop: EditorResponseLoop,
};

function mapStateToProps(state: State, ownProps: any): StateProps {
  const {
    chunks,
    rhs,
    firstSelectedChunkIndex,
    currentFile,
    displayResultsInline,
    editorResponseLoop,
    chunkToRHS,
  } = state;

  const {
    index,
  } = ownProps;

  const thisChunkRHSObjects = chunkToRHS[index] ?? { outdated: true, objects: [] };

  return {
    chunks,
    rhs,
    firstSelectedChunkIndex,
    currentFile,
    thisChunkRHSObjects,
    displayResultsInline,
    editorResponseLoop,
    chunkToRHS,
  };
}

type PropsFromReact = {
  index: number,
  parent: CodeMirror.Doc,
  focusNewChat: () => void,
};

type DispatchProps = {
  run: () => void,
  setChunks: (chunks: ChunksUpdate) => void,
  setChunkToRHS: (chunkToRHS: RHSObjects[]) => void,
  enqueueEffect: (effect: Effect) => void,
  setShouldAdvanceCursor: (value: boolean) => void,
  setFirstSelectedChunkIndex: (value: false | number) => void,
};

function mapDispatchToProps(dispatch: (action: Action) => any): DispatchProps {
  return {
    run() {
      dispatch({ type: 'runSession', key: 'runProgram' });
    },
    setChunks(chunks: ChunksUpdate) {
      dispatch({ type: 'update', key: 'chunks', value: chunks });
    },
    setChunkToRHS(chunkToRHS: RHSObjects[]) {
      dispatch({ type: 'update', key: 'chunkToRHS', value: chunkToRHS });
    },
    enqueueEffect(effect: Effect) {
      dispatch({ type: 'enqueueEffect', effect });
    },
    setShouldAdvanceCursor(value: boolean) {
      dispatch({ type: 'update', key: 'shouldAdvanceCursor', value });
    },
    setFirstSelectedChunkIndex(value: false | number) {
      dispatch({ type: 'update', key: 'firstSelectedChunkIndex', value });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type ChatProps = PropsFromRedux & DispatchProps & StateProps & PropsFromReact;

class Chat extends React.Component<ChatProps, any> {
  /* Used to autofocus this component when necessary */
  private input: React.RefObject<CodeMirror>;

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
      startLine,
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
                line: l1 - startLine - 1,
                ch: ch1,
              },
              {
                line: l2 - startLine - 1,
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

    const { editor } = chunks[index];

    if ('getDoc' in editor) {
      const marks = editor.getDoc().getAllMarks();
      marks.forEach((m) => m.clear());
    }

    const newChunks = [...chunks];
    newChunks[index] = {
      ...newChunks[index],
      editor,
      errorState: { status: 'notLinted' },
    };
    for (let i = index; i < newChunks.length; i += 1) {
      newChunks[i] = {
        ...newChunks[i],
        startLine: getStartLineForIndex(newChunks, i),
      };
    }

    setChunks({
      chunks: newChunks,
      modifiesText: true,
    });

    const withInvalidation = [...chunkToRHS];
    withInvalidation[index] = {
      ...(withInvalidation[index] ?? { outdated: true, objects: [] }),
      outdated: true,
    };
    setChunkToRHS(withInvalidation);
  }

  /* Called in response to an arrow up event. Checks if the cursor is on the top
     line of a chunk and, if so, focuses the previous chunk. */
  handleArrowUp(editor: any, event: Event) {
    const {
      index,
      chunks,
    } = this.props;
    const pos = (editor as any).getCursor();
    if (pos.line === chunks[index].startLine && index > 0) {
      const newEditor = chunks[index - 1].editor;
      if ('focus' in newEditor) {
        newEditor.focus();
      }
      event.preventDefault();
    }
  }

  /* Called in response to an arrow down event. Checks if the cursor is on the
     bottom line of a chunk and, if so, focuses the subsequent chunk. */
  handleArrowDown(editor: any, event: Event) {
    const {
      index,
      chunks,
      focusNewChat,
    } = this.props;
    const pos = (editor as any).getCursor();
    if (pos.line === chunks[index].startLine + chunks[index].editor.getValue().split('\n').length - 1) {
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
  handleEnter(editor: CodeMirror.Editor & CodeMirror.Doc, event: Event) {
    const {
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
    if (singleLineEnter || token.state.lineState.tokens.length === 0) {
      editor.getInputField().blur();
      run();
      event.preventDefault();
    }
  }

  handleBlur(editor: CodeMirror.Editor & CodeMirror.Doc) {
    const {
      index,
    } = this.props;
    if (editor.getValue().trim() === '') {
      this.deleteChunk(index);
    }
  }

  /* Delete this chunk and move every chunk below it up by one */
  deleteChunk(index: number) {
    const {
      chunks,
      setChunks,
      enqueueEffect,
    } = this.props;
    const newChunks = [
      ...chunks.slice(0, index),
      ...chunks.slice(index + 1, chunks.length)];
    for (let i = index; i < newChunks.length; i += 1) {
      newChunks[i] = {
        ...newChunks[i],
        startLine: getStartLineForIndex(newChunks, i),
      };
    }
    setChunks({
      chunks: newChunks,
      modifiesText: true,
    });
    enqueueEffect({ effectKey: 'initCmd', cmd: BackendCmd.Run });
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

  /* Called in response to a mouse down key event. Sets this chunk as the
     focused chunk and removes all chunk selections. */
  handleMouseDown(event: any) {
    const {
      index,
      chunks,
      setShouldAdvanceCursor,
      setFirstSelectedChunkIndex,
      setChunks,
    } = this.props;

    /* Instruct Redux to not move the cursor into the next chunk upon lint
       success. This only matters if a lint is happening at the time of clicking.
       Without this, focus would first jump to this chunk (indented), but then jump
       away to the chunk past the linted chunk (unintended). */
    setShouldAdvanceCursor(false);

    if (event.buttons !== 1) {
      return;
    }

    const newChunks = removeAllSelections(chunks);
    setChunks({
      chunks: newChunks,
      modifiesText: false,
    });
    setFirstSelectedChunkIndex(index);
  }

  render() {
    const {
      chunks, index, parent,
    } = this.props;
    const { editor: initialEditor, startLine } = chunks[index];

    const handleMount = (editor: CodeMirror.Editor) => {
      const {
        setChunks,
      } = this.props;

      const marks = editor.getDoc().getAllMarks();
      marks.forEach((m) => m.clear());
      editor.setSize(null, 'auto');

      // Turn ghost UninitializedEditor into a real editor if the editor has mounted
      // This check might be extraneous
      if (editor.getValue() !== initialEditor.getValue()) {
        editor.setValue(initialEditor.getValue());
      }

      setChunks({
        chunk: {
          ...chunks[index],
          editor,
        },
        modifiesText: false,
      });
    };

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
            width: '100%',
            justifyContent: 'flex-end',
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
      const isDataDefinition = rhsObjects.filter((r) => !isLocation(r)).length === 0
              && rhsObjects.filter((r) => isLocation(r) && r.name.startsWith('is-')).length > 0;
      const isFunctionDefinition = rhsObjects.length === 1 && isLocation(rhsObjects[0]) && typeof rhsObjects[0].value === 'function';
      if (rhsObjects.length === 0 || isDataDefinition || isFunctionDefinition) {
        if (thisChunkRHSObjects.outdated) {
          rhs = <div style={{ float: 'right' }} className="chatitor-rhs pending"> . . . </div>;
        } else {
          displayCheckMark = true;
        }
      } else if (rhsObjects.length === 1) {
        const val = rhsObjects[0];
        rhs = (
          <RHSObjectComponent
            rhsObject={val}
            isSelected={false}
            className="chatitor-rhs"
            outdated={thisChunkRHSObjects.outdated}
          />
        );
      } else if (rhsObjects.filter((r) => !isRHSCheck(r)).length === 0) {
        rhs = (
          <CheckResults
            // Would love to have TypeScript obviate this `as`
            checks={rhsObjects as RHSCheck[]}
            outdated={thisChunkRHSObjects.outdated}
          />
        );
      } else {
        rhs = rhsObjects.map((val) => (
          <RHSObjectComponent
            key={val.key ?? 'oeunth'}
            rhsObject={val}
            isSelected={false}
            className="chatitor-rhs"
            outdated={thisChunkRHSObjects.outdated}
          />
        ));
      }

      chunkResultsPart = (
        <div
          style={{
            display: 'flex',
            width: '100%',
            justifyContent: 'flex-end',
            marginBottom: '0.5em',
          }}
        >
          {rhs}
        </div>
      );
    }

    const chunkEditorPart = (
      <div style={{ width: '100%' }}>
        <LinkedCodeMirror
          parent={parent}
          start={startLine}
          end={startLine + initialEditor.getValue().split('\n').length}
          onMouseDown={(editor: any, e: any) => {
            this.handleMouseDown(e);
          }}
          editorDidMount={handleMount}
          options={{
            mode: 'pyret',
            theme: 'default',
            lineWrapping: true,
          }}
          onBeforeChange={() => {
            this.scheduleUpdate();
          }}
          onKeyDown={(editor, event) => {
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
          }}
          onBlur={(editor) => this.handleBlur(editor)}
          className={displayCheckMark ? 'chat checkmark' : 'chat'}
        />
      </div>
    );

    return (
      <>
        { chunkEditorPart }
        { chunkResultsPart }
      </>
    );
  }
}

export default connector(Chat);
