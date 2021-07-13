/* The DefChunk component, one or more of which can be contained within the
   DefChunks component.

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
  findChunkFromSrcloc,
} from './chunk';

import {
  Action,
  ChunksUpdate,
} from './action';

import { Effect } from './effect';
import {
  RHSObject,
  RHSObjects,
  getRow,
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
  thisChunkRHSObjects: RHSObject[],
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
  } = state;

  const {
    index,
  } = ownProps;

  const thisChunkRHSObjects: RHSObject[] = [];

  // TODO(alex): Map runtime messages?
  rhs.objects.forEach((rhsObject) => {
    const correspondingChunk = findChunkFromSrcloc(
      chunks,
      [
        `file://${currentFile}`,
        getRow(rhsObject),
      ],
      currentFile,
    );

    if (index === correspondingChunk) {
      thisChunkRHSObjects.push(rhsObject);
    }
  });

  return {
    chunks,
    rhs,
    firstSelectedChunkIndex,
    currentFile,
    thisChunkRHSObjects,
    displayResultsInline,
    editorResponseLoop,
  };
}

type PropsFromReact = {
  index: number,
  focused: boolean,
  parent: CodeMirror.Doc,
};

type DispatchProps = {
  setChunks: (chunks: ChunksUpdate) => void,
  enqueueEffect: (effect: Effect) => void,
  setShouldAdvanceCursor: (value: boolean) => void,
  setRHS: () => void,
  setFirstSelectedChunkIndex: (value: false | number) => void,
};

function mapDispatchToProps(dispatch: (action: Action) => any): DispatchProps {
  return {
    setChunks(chunks: ChunksUpdate) {
      dispatch({ type: 'update', key: 'chunks', value: chunks });
    },
    enqueueEffect(effect: Effect) {
      dispatch({ type: 'enqueueEffect', effect });
    },
    setShouldAdvanceCursor(value: boolean) {
      dispatch({ type: 'update', key: 'shouldAdvanceCursor', value });
    },
    setRHS() {
      dispatch({ type: 'update', key: 'rhs', value: 'make-outdated' });
    },
    setFirstSelectedChunkIndex(value: false | number) {
      dispatch({ type: 'update', key: 'firstSelectedChunkIndex', value });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type DefChunkProps = PropsFromRedux & DispatchProps & StateProps & PropsFromReact;

class DefChunk extends React.Component<DefChunkProps, any> {
  /* Used to autofocus this component when necessary */
  private input: React.RefObject<CodeMirror>;

  constructor(props: DefChunkProps) {
    super(props);
    this.input = React.createRef();
  }

  /* A React component updates every time its props change. Since each chunk
     receives, as props, all other chunks, this would cause a lot of redundant
     re-rendering. This function attempts to determine when such prop updates
     can be ignored. It will probably need to be changed when new props are
     added or removed from this component.

     Known issues: line numbers from CodeMirror objects do not always update
     when chunks are re-ordered. */
  shouldComponentUpdate(newProps: DefChunkProps) {
    const n = newProps;
    const o = this.props;

    if (n.thisChunkRHSObjects !== o.thisChunkRHSObjects) {
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
      rhs,
      setRHS,
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

    if (!rhs.outdated) {
      setRHS();
    }
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
    } = this.props;
    const pos = (editor as any).getCursor();
    if (pos.line === chunks[index].startLine + chunks[index].editor.getValue().split('\n').length - 1 && index < chunks.length - 1) {
      const newEditor = chunks[index + 1].editor;
      if ('focus' in newEditor) {
        newEditor.focus();
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
      enqueueEffect,
    } = this.props;
    const pos = editor.getCursor();
    const token = editor.getTokenAt(pos);
    if ((event as any).shiftKey || editor.getValue().split('\n').length === 1 || token.state.lineState.tokens.length === 0) {
      enqueueEffect({ effectKey: 'initCmd', cmd: BackendCmd.Run });
      editor.getInputField().blur();
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
        />
      </div>
    );

    const chunkResultsPart = (
      <div>
        {(() => {
          const {
            thisChunkRHSObjects,
            displayResultsInline,
          } = this.props;

          if (displayResultsInline) {
            const chunk = chunks[index];
            const { editor } = chunk;

            if (chunk.errorState.status === 'failed' && 'markText' in editor) {
              return (
                <div style={{ textAlign: 'right', display: 'block' }}>
                  {chunk.errorState.failures.map((failure, i) => (
                    // eslint-disable-next-line
                    <div className="chatitor-rhs" key={i}>
                      <FailureComponent failure={failure} editor={editor} />
                    </div>
                  ))}
                </div>
              );
            }

            let rhs;
            // TODO(luna): more principled
            const isDataDefinition = thisChunkRHSObjects.filter((r) => !isLocation(r)).length === 0
              && thisChunkRHSObjects.filter((r) => isLocation(r) && r.name.startsWith('is-')).length > 0;
            if (thisChunkRHSObjects.length === 0) {
              rhs = <div style={{ float: 'right' }} className="chatitor-rhs pending"> . . . </div>;
            } else if (thisChunkRHSObjects.length === 1) {
              const val = thisChunkRHSObjects[0];
              rhs = (
                <RHSObjectComponent
                  key={getRow(val)}
                  rhsObject={val}
                  isSelected={false}
                  className="chatitor-rhs"
                />
              );
            } else if (thisChunkRHSObjects.filter((r) => !isRHSCheck(r)).length === 0) {
              rhs = (
                <CheckResults
                  key={getRow(thisChunkRHSObjects[0])}
                  // Would love to have TypeScript obviate this `as`
                  checks={thisChunkRHSObjects as RHSCheck[]}
                />
              );
            } else if (isDataDefinition) {
              rhs = <></>;
            } else {
              console.log(thisChunkRHSObjects);
              throw new Error('unfolded multiple RHS (logged above)');
            }

            return (
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

          return false;
        })()}
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

export default connector(DefChunk);
