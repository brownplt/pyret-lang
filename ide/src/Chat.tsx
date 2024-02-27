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
import { DomEvent, UnControlled as ReactCM } from 'react-codemirror2';
import { State } from './state';
import {
  CMEditor, enterShouldSend, isWrapFirst, isWrapLast,
} from './utils';

import {
  Chunk,
  isInitializedEditor,
  UninitializedEditor,
} from './chunk';

import {
  Action,
  ChunksUpdate,
} from './action';
import ChatResult from './ChatResult';

import {
  LogIn,
  Plus,
  Trash2
} from 'react-feather';

type StateProps = {
  chunks: Chunk[],
  enterNewline: boolean,
  technicallyOutdated: boolean,
  fontSize: number,
  focusedChunk: number | false
};

function mapStateToProps(state: State, ownProps: any): StateProps {
  const {
    chunks,
    enterNewline,
    firstOutdatedChunk,
    fontSize,
    focusedChunk
  } = state;

  const {
    index,
  } = ownProps;

  const technicallyOutdated = index >= firstOutdatedChunk;

  return {
    chunks,
    enterNewline,
    technicallyOutdated,
    fontSize,
    focusedChunk
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
  mergeChunks: (top: number, bottom: number) => void,
  insertChunk: (index: number, text?: string) => void,
  focusChunk: (index: number | false) => void,
};

function mapDispatchToProps(dispatch: (action: Action) => any): DispatchProps {
  return {
    run() {
      dispatch({ type: 'run' });
    },
    setChunks(chunks: ChunksUpdate) {
      dispatch({ type: 'update', key: 'chunks', value: chunks });
    },
    deleteChunk(index: number) {
      dispatch({ type: 'chunk', key: 'delete', index });
    },
    mergeChunks(top: number, bottom: number) {
      dispatch({ type: 'chunk', key: 'merge', top, bottom });
    },
    insertChunk(index: number, text?: string) {
      dispatch({
        type: 'chunk', key: 'insert', index, grabFocus: true, text
      });
    },
    focusChunk(index : number | false) {
      dispatch({
        type: 'update', key: 'updater', value: (state : State) => ({ ...state, focusedChunk: index })
      });  
    }
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type ChatProps = PropsFromRedux & DispatchProps & StateProps & PropsFromReact;

class Chat extends React.Component<ChatProps, {}> {
  /* A React component updates every time its props change. Since each chunk
     receives, as props, all other chunks, this would cause a lot of redundant
     re-rendering. This function attempts to determine when such prop updates
     can be ignored. It will probably need to be changed when new props are
     added or removed from this component. */
  shouldComponentUpdate(newProps: ChatProps) {
    const n = newProps;
    const o = this.props;
    if (n.enterNewline !== o.enterNewline) { return true; }
    if (n.technicallyOutdated !== o.technicallyOutdated) { return true; }
    if (n.fontSize !== o.fontSize) { return true; }
    if (n.focusedChunk !== o.focusedChunk) { return true; }
    const nChunk = n.chunks[n.index];
    const oChunk = o.chunks[o.index];
    if (nChunk !== oChunk) {
      return true;
    }
    return false;
  }

  componentDidUpdate(prevProps: Readonly<ChatProps>): void {
    const { editor } = this.props.chunks[this.props.index];
    if (this.props.fontSize !== prevProps.fontSize && isInitializedEditor(editor)) {
      editor.refresh();
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

    setChunks({
      chunk: {
        ...chunks[index],
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
      if (isInitializedEditor(newEditor)) {
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
        if (isInitializedEditor(newEditor)) {
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
    const { enterNewline, run } = this.props;
    if (enterShouldSend(editor, enterNewline, event)) {
      editor.getInputField().blur();
      run();
      event.preventDefault();
    }
  }

  handleFocus() {
    this.props.focusChunk(this.props.index);
  }

  insertAbove() {
    const { insertChunk: insert, index } = this.props;
    insert(index);
  }

  merge() {
    const { mergeChunks, index } = this.props;
    mergeChunks(index - 1, index);
  }

  /* Delete this chunk and move every chunk below it up by one */
  deleteChunk(index: number) {
    const { deleteChunk, run } = this.props;
    deleteChunk(index);
    run();
  }

  /* Called in response to a Backspace or Delete key event. Deletes empty chunks */
  handleBackspace(event: Event) {
    const {
      chunks, index,
    } = this.props;
    const ed = chunks[index].editor;
    if (ed.getValue().trim() === '') {
      if (!isInitializedEditor(ed)) {
        throw new Error('backspace on uninitialized editor');
      }
      ed.getInputField().blur();
      event.preventDefault();
    }
  }

  handleMount(editor: CMEditor, initialEditor: CMEditor | UninitializedEditor) {
    const {
      setChunks,
      chunks,
      index,
    } = this.props;

    editor.setSize(null, 'auto');

    editor.setOption('matchKeywords', true);
    editor.setOption('matchBrackets', true);

    // Use value of ghost UninitializedEditor in real editor
    editor.setValue(initialEditor.getValue());
    const grabs = 'grabFocus' in initialEditor && initialEditor.grabFocus;
    const isCurrent = this.props.focusedChunk === this.props.index;
    if (grabs || isCurrent) {
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

    const chunk = chunks[index];
    const {
      editor: chunkEditor, results, outdated, id,
    } = chunk;

    let chunkResultsPart = <ChatResult editor={chunkEditor} results={results} id={id} technicallyOutdated={technicallyOutdated}/>;

    const isError = results.status === 'failed' && isInitializedEditor(chunkEditor);

    const chunkEditorPart = (
      <div className="chat-editor-wrapper">
        <ReactCM
          editorDidMount={(editor) => this.handleMount(editor as CMEditor, chunkEditor)}
          options={{
            mode: 'pyret',
            theme: 'default',
            lineWrapping: true,
            extraKeys: { Tab: 'indentAuto' },
          }}
          onChange={() => {
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
              case 'Delete':
                this.handleBackspace(event);
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
          onFocus={((editor: CMEditor) => this.handleFocus()) as any}
          className="chat"
        />
      </div>
    );

    const addButtonTitle = 'Insert new message here';

    const outdatedClass = outdated ? 'outdated' : '';
    const pendingRerunClass = technicallyOutdated ? 'partially-outdated' : '';
    const isErrorClass = isError ? 'chatitor-error' : '';
    const focusedClass = this.props.focusedChunk === this.props.index ? 'focused-chunk' : '';

    const merge = this.props.index === 0 ? <></> :
      <button title='Merge with previous' className="text-button chunk-menu-icon" onClick={() => this.merge()} type="button">
        <LogIn style={{ transform: "rotate(270deg)" }} className="icon"/>
      </button>;
    return (
      <>
        <div className={`chat-and-result ${outdatedClass} ${pendingRerunClass} ${isErrorClass} ${focusedClass}`}>
          <div className='chunk-menu'>
            <button title={addButtonTitle} className="text-button chunk-menu-icon" onClick={() => this.insertAbove()} type="button">
              <Plus className="icon"/>
            </button>
            <button title='Delete' className="text-button chunk-menu-icon" onClick={() => this.deleteChunk(this.props.index)} type="button">
              <Trash2 className="icon"/>
            </button>
            {merge}
          </div>
          { chunkEditorPart }
          { chunkResultsPart }
        </div>
      </>
    );
  }
}

export default connector(Chat);
