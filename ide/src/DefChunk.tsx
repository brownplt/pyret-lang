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
import { Controlled as CodeMirror } from 'react-codemirror2';
import { BackendCmd, State, EditorResponseLoop } from './state';

import backendCmdFromState from './editor_loop';

import {
  Chunk,
  Selection,
  getStartLineForIndex,
  emptyChunk,
  lintSuccessState,
  removeSelection,
  removeAllSelections,
  selectAll,
  isEmptySelection,
  removeSelectedText,
  emptySelection,
  getChunkSelectedText,
  compareLineAndCh,
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
} from './rhsObject';
import RHSObjectComponent from './RHSObjectComponent';

type StateProps = {
  chunks: Chunk[],
  focusedChunk: number | undefined,
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
    focusedChunk,
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
    focusedChunk,
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
};

type DispatchProps = {
  setFocusedChunk: (index: number) => void,
  setChunks: (chunks: ChunksUpdate) => void,
  enqueueEffect: (effect: Effect) => void,
  setShouldAdvanceCursor: (value: boolean) => void,
  setRHS: () => void,
  setFirstSelectedChunkIndex: (value: false | number) => void,
};

function mapDispatchToProps(dispatch: (action: Action) => any): DispatchProps {
  return {
    setFocusedChunk(index: number) {
      dispatch({ type: 'update', key: 'focusedChunk', value: index });
    },
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

/* Determines which chunks to delete, as well as which chunk (if any) should be
   focused after the deletion

   Arguments:
     chunks: the chunks to delete from
     index: the currently focused chunk

   Returns: {
     chunks: a new array of chunks, like the input, but possibly with some deletions
     shouldPreventDefault: true if the default backspace / delete event should be prevented
     firstSelectedChunk: the chunk to focus after deletions
   }
 */
function deleteSelectedChunks(chunks: Chunk[], index: number): {
  chunks: Chunk[],
  shouldPreventDefault: boolean,
  shouldChangeFocus: boolean,
  firstSelectedChunk: false | number,
} {
  let shouldPreventDefault = false;
  let firstSelectedChunk: false | number = false;

  const updatedChunks = chunks.reduce(
    (newChunks: Chunk[], chunk, i) => {
      const {
        selection,
      } = chunk;

      if (isEmptySelection(selection)) {
        newChunks.push(chunk);
        return newChunks;
      }

      if (firstSelectedChunk === false) {
        firstSelectedChunk = i;
      }

      if (i === index) {
        shouldPreventDefault = true;
      }

      const newChunk = removeSelectedText(chunk);

      if (newChunk.text === '') {
        return newChunks;
      }

      newChunks.push(newChunk);

      return newChunks;
    },
    [],
  );

  const shouldChangeFocus = updatedChunks.length !== chunks.length;

  if (updatedChunks.length === 0) {
    updatedChunks.push(emptyChunk({ errorState: lintSuccessState }));
  }

  for (let i = 0; i < updatedChunks.length; i += 1) {
    updatedChunks[i].startLine = getStartLineForIndex(updatedChunks, i);
  }

  return {
    chunks: updatedChunks,
    shouldChangeFocus,
    shouldPreventDefault,
    firstSelectedChunk,
  };
}

class DefChunk extends React.Component<DefChunkProps, any> {
  /* Used to autofocus this component when necessary */
  private input: React.RefObject<any>;

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
      && n.chunks[n.index].text === o.chunks[o.index].text
      && n.focusedChunk !== n.index) {
      return false;
    }

    if (n.focusedChunk === o.focusedChunk
        && n.chunks[n.index].text === o.chunks[o.index].text
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
      selection,
    } = chunks[index];

    if (editor !== false) {
      const doc = editor.getDoc();

      const cmSelectedText = doc.getSelection();
      const selectedText = getChunkSelectedText(chunks[index]);

      if (cmSelectedText !== selectedText) {
        if (isEmptySelection(selection)) {
          doc.setSelection(emptySelection.anchor, emptySelection.head);
        } else {
          doc.setSelection(selection.anchor, selection.head);
        }
      }
    }

    if (editor && errorState.status === 'succeeded') {
      const marks = editor.getDoc().getAllMarks();
      marks.forEach((m) => m.clear());
    } else if (editor && errorState.status === 'failed') {
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

    const { focusedChunk } = this.props;
    if (index === focusedChunk && this.input.current !== null) {
      this.input.current.editor.focus();
    }
  }

  /* Called in response to an edit event, where `value` is the chunk's text
     after the edit. Marks updated chunks as not linted so that the running
     infastructure knows to lint them before compiling. */
  scheduleUpdate(value: string) {
    const {
      chunks,
      index,
      setChunks,
      rhs,
      setRHS,
    } = this.props;

    const { editor } = chunks[index];

    if (editor !== false) {
      const marks = editor.getDoc().getAllMarks();
      marks.forEach((m) => m.clear());
    }

    const newChunks = [...chunks];
    newChunks[index] = {
      ...newChunks[index],
      text: value,
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
      setFocusedChunk,
      setShouldAdvanceCursor,
    } = this.props;
    const pos = (editor as any).getCursor();
    if (pos.line === 0 && index > 0) {
      setFocusedChunk(index - 1);
      setShouldAdvanceCursor(false);
      event.preventDefault();
    }
  }

  /* Called in response to an arrow down event. Checks if the cursor is on the
     bottom line of a chunk and, if so, focuses the subsequent chunk. */
  handleArrowDown(editor: any, event: Event) {
    const {
      index,
      setFocusedChunk,
      chunks,
      setShouldAdvanceCursor,
    } = this.props;
    const pos = (editor as any).getCursor();
    if (pos.line === chunks[index].text.split('\n').length - 1 && index < chunks.length - 1) {
      setFocusedChunk(index + 1);
      setShouldAdvanceCursor(false);
      event.preventDefault();
    }
  }

  /* Called in response to an Enter key event. Uses the Pyret mode for
     CodeMirror to determine if the cursor is at the end of a complete statement
     and, if so, instructs the linting infastructure to create a new chunk upon
     lint success. If shift+enter is pressed, no new chunk will be made. In
     either case, a run is triggered by saving the file. */
  handleEnter(editor: any, event: Event) {
    const {
      enqueueEffect,
      setShouldAdvanceCursor,
    } = this.props;
    const pos = (editor as any).getCursor();
    const token = editor.getTokenAt(pos);
    if ((event as any).shiftKey) {
      setShouldAdvanceCursor(false);
      enqueueEffect({ effectKey: 'initCmd', cmd: BackendCmd.Run });
      event.preventDefault();
    } else if (token.state.lineState.tokens.length === 0) {
      setShouldAdvanceCursor(true);
      enqueueEffect({ effectKey: 'initCmd', cmd: BackendCmd.None });
      event.preventDefault();
    }
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
      setChunks,
      setFocusedChunk,
      focusedChunk,
      enqueueEffect,
      editorResponseLoop,
    } = this.props;
    if (index === 0 && chunks.length > 1 && chunks[0].text.trim() === '') {
      /* Cursor is in the first chunk, the text of this chunk is empty, and
         there is more than one chunk. Delete this chunk, move every chunk up by
         one, and keep the focus where it is. */
      const newChunks = [...chunks.slice(1, chunks.length)];
      for (let i = 0; i < newChunks.length; i += 1) {
        newChunks[i] = {
          ...newChunks[i],
          startLine: getStartLineForIndex(newChunks, i),
        };
      }
      setChunks({
        chunks: newChunks,
        modifiesText: true,
      });
      setFocusedChunk(0);
      event.preventDefault();
    } else if (index > 0 && index < chunks.length - 1 && chunks[index].text.trim() === '') {
      /* Cursor is not in the first chunk nor in the last chunk, and the text of
         this chunk is empty. Delete this chunk, move every chunk below it up by
         one, and keep the focus where it is. */
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
      event.preventDefault();
    } else {
      /* Try deleting any selected chunks. */
      const result = deleteSelectedChunks(chunks, index);
      setChunks({
        chunks: result.chunks,
        modifiesText: true,
      });

      const {
        shouldPreventDefault,
        shouldChangeFocus,
        firstSelectedChunk,
      } = result;

      if (shouldChangeFocus && firstSelectedChunk !== false) {
        const newFocusedChunk = Math.min(result.chunks.length - 1, firstSelectedChunk + 1);

        if (newFocusedChunk !== focusedChunk) {
          setFocusedChunk(newFocusedChunk);
        } else {
          enqueueEffect({ effectKey: 'initCmd', cmd: backendCmdFromState(editorResponseLoop) });
        }
      }
      if (shouldPreventDefault) {
        event.preventDefault();
      }
    }
  }

  /* Called in response to a Backspace key event. Deletes chunks in different ways
     depending on where the cursor is and which chunks (if any) are selected.

     This is a whole lot like handleDelete, but with slightly different
     functionality to mimic the differences between deleting something with Delete
     versus deleting something with Backspace */
  handleBackspace(event: Event) {
    const {
      chunks, index, setChunks, setFocusedChunk,
    } = this.props;
    if (index === 0 && chunks.length > 1 && chunks[0].text.trim() === '') {
      /* Cursor is in the first chunk, the text of this chunk is empty, and
         there is more than one chunk. Delete this chunk, move every chunk up by
         one, and keep the focus where it is. */
      const newChunks = [...chunks.slice(1, chunks.length)];
      for (let i = 0; i < newChunks.length; i += 1) {
        newChunks[i] = {
          ...newChunks[i],
          startLine: getStartLineForIndex(newChunks, i),
        };
      }
      setChunks({
        chunks: newChunks,
        modifiesText: true,
      });
      setFocusedChunk(0);
      event.preventDefault();
    } else if (index > 0 && chunks[index].text.trim() === '') {
      /* Cursor is not in the first chunk and the text of this chunk is empty.
         Delete this chunk, move every chunk below it up by one, and focus the
         previous chunk. */
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
      setFocusedChunk(index - 1);
      event.preventDefault();
    } else {
      /* Try deleting any selected chunks. */
      const result = deleteSelectedChunks(chunks, index);
      setChunks({
        chunks: result.chunks,
        modifiesText: true,
      });

      const {
        shouldPreventDefault,
        shouldChangeFocus,
        firstSelectedChunk,
      } = result;

      if (shouldChangeFocus && firstSelectedChunk !== false) {
        setFocusedChunk(Math.max(0, firstSelectedChunk - 1));
      }
      if (shouldPreventDefault) {
        event.preventDefault();
      }
    }
  }

  /* Called in response to a mouse down key event. Sets this chunk as the
     focused chunk and removes all chunk selections. */
  handleMouseDown(event: any) {
    const {
      index,
      chunks,
      setFocusedChunk,
      setShouldAdvanceCursor,
      setFirstSelectedChunkIndex,
      setChunks,
    } = this.props;

    /* Instruct Redux to not move the cursor into the next chunk upon lint
       success. This only matters if a lint is happening at the time of clicking.
       Without this, focus would first jump to this chunk (indented), but then jump
       away to the chunk past the linted chunk (unintended). */
    setShouldAdvanceCursor(false);

    setFocusedChunk(index);

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

  /* Called in response to a mouse enter key event. Selects the correct chunks
     if a drag is happening. */
  handleMouseEnter(e: any) {
    const {
      chunks,
      index,
      firstSelectedChunkIndex,
      setFirstSelectedChunkIndex,
      setChunks,
    } = this.props;

    /* Do not proceed if a click-and-drag (selection) is not happening */
    if (e.buttons !== 1) {
      return;
    }

    if (firstSelectedChunkIndex === false) {
      setFirstSelectedChunkIndex(index);
      setChunks({
        chunk: selectAll(chunks[index]),
        modifiesText: false,
      });
    } else if (index <= firstSelectedChunkIndex) {
      // selecting from bottom to the top
      setChunks({
        chunks: chunks.map((chunk, i) => {
          if (i < index || i > firstSelectedChunkIndex) {
            return removeSelection(chunk);
          }

          return selectAll(chunk);
        }),
        modifiesText: false,
      });
    } else if (index > firstSelectedChunkIndex) {
      // selecting from top to bottom
      setChunks({
        chunks: chunks.map((chunk, i) => {
          if (i > index || i < firstSelectedChunkIndex) {
            return removeSelection(chunk);
          }

          return selectAll(chunk);
        }),
        modifiesText: false,
      });
    }
  }

  /* Called in response to the user selecting text inside of the underlying
     CodeMirror object. We use this to intercept these selection events and
     handle them ourselves in Redux instead of letting CodeMirror do it itself.
     This is necessary to efficiently detect and properly update chunks (see
     componentNeedsUpdate). */
  handleOnSelection({ ranges, origin }: { ranges: Selection[], origin?: string }) {
    const {
      chunks,
      index,
      setChunks,
    } = this.props;

    if (origin !== '*mouse') {
      // This happens when we manually call setSelection, as opposed to the user
      // selecting text with their mouse.
      return;
    }

    if (ranges.length < 1) {
      return;
    }

    const cmp = compareLineAndCh(chunks[index].text, ranges[0].anchor, ranges[0].head);

    if (cmp <= 0) {
      setChunks({
        chunk: {
          ...chunks[index],
          selection: { anchor: ranges[0].anchor, head: ranges[0].head },
        },
        modifiesText: false,
      });
    } else {
      setChunks({
        chunk: {
          ...chunks[index],
          selection: { anchor: ranges[0].head, head: ranges[0].anchor },
        },
        modifiesText: false,
      });
    }
  }

  render() {
    const {
      chunks, index, focusedChunk,
    } = this.props;
    const { text, startLine } = chunks[index];

    return (
      <div
        style={{
          width: '100%',
          display: 'flex',
        }}
      >
        <div
          style={{
            position: 'relative',
            width: 0,
            height: '100%',
          }}
        >
          {(() => {
            const chunk = chunks[index];

            if (chunk.errorState.status === 'failed'
          && focusedChunk === index) {
              return (
                <div style={{
                  alignSelf: 'center',
                  background: '#FFF2F2',
                  position: 'absolute',
                  top: '100%',
                  width: '40em',
                  zIndex: 500001,
                  fontFamily: 'sans-serif',
                  borderRadius: '3px',
                  border: '0.3em solid hsl(204, 100%, 74%)',
                  padding: '0.2em',
                  marginRight: '1em',
                  boxShadow: '0 0 1em',
                }}
                >
                  {chunk.errorState.failures}
                </div>
              );
            }

            return false;
          })()}
        </div>
        <div
          style={{
            width: '100%',
            display: 'flex',
            flexDirection: 'column',
          }}
          onMouseEnter={(event: any) => {
            this.handleMouseEnter(event);
          }}
        >
          <CodeMirror
            ref={this.input}
            onMouseDown={(editor: any, e: any) => {
              this.handleMouseDown(e);
            }}
            editorDidMount={(editor) => {
              const {
                setChunks,
              } = this.props;

              const marks = editor.getDoc().getAllMarks();
              marks.forEach((m) => m.clear());
              editor.setSize(null, 'auto');

              setChunks({
                chunk: {
                  ...chunks[index],
                  editor,
                },
                modifiesText: false,
              });
            }}
            value={text}
            options={{
              mode: 'pyret',
              theme: 'default',
              lineNumbers: true,
              lineWrapping: true,
              lineNumberFormatter: (l) => String(l + startLine),
              autofocus: index === focusedChunk,
            }}
            onBeforeChange={(editor, data, value) => {
              this.scheduleUpdate(value);
            }}
            onSelection={(editor, data) => {
              this.handleOnSelection(data);
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
                default:
              }
            }}
            autoCursor
          />
          <div>
            {(() => {
              const {
                thisChunkRHSObjects,
                displayResultsInline,
              } = this.props;

              if (displayResultsInline && thisChunkRHSObjects.length > 0) {
                const isSelected = index === focusedChunk;

                return (
                  <pre
                    style={{
                      margin: 0,
                      background: isSelected ? '#d7d4f0' : 'rgba(0, 0, 0, 0)',
                      borderTop: isSelected ? '2px solid #c8c8c8' : '2px solid rgba(0, 0, 0, 0)',
                      borderBottom: isSelected ? '2px solid #c8c8c8' : '2px solid rgba(0, 0, 0, 0)',
                      width: '100%',
                      display: 'flex',
                      flexDirection: 'row',
                      justifyContent: 'flex-end',
                    }}
                  >
                    {thisChunkRHSObjects.map((val) => {
                      if (!isRHSCheck(val)) {
                        return (
                          <RHSObjectComponent
                            key={getRow(val)}
                            rhsObject={val}
                            isSelected={false}
                          />
                        );
                      }
                      return false;
                    })}
                  </pre>
                );
              }

              return false;
            })()}
          </div>
        </div>
      </div>
    );
  }
}

export default connector(DefChunk);
