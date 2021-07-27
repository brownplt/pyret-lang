import React from 'react';
import CM from 'codemirror';

type Editor = CM.Editor & CM.Doc;

type Props = {
  parent: CM.Doc,
  start: number,
  end: number,
  // react-codemirror2 equivalents
  onMouseDown?: (editor: Editor, e: InputEvent) => void,
  editorDidMount?: (editor: Editor) => void,
  // value={text}
  options?: CM.EditorConfiguration,
  onBeforeChange?: (editor: Editor, data: CM.EditorChange, value: string) => void,
  onSelection?: (editor: Editor, data: any) => void,
  onKeyDown?: (editor: Editor, event: KeyboardEvent) => void,
  // autoCursor: boolean,
  onBlur?: (editor: Editor) => void,
  className?: string,
};
type State = {
  editor?: Editor,
};

export default class LinkedCM extends React.PureComponent<Props, State> {
  private tag: React.RefObject<HTMLDivElement>;

  constructor(props: Props) {
    super(props);
    this.tag = React.createRef();
    this.state = { editor: undefined };
  }

  setupEditor() {
    // TODO(luna): Fix the refs
    if (this.tag.current === null) {
      throw new Error('ref didn\'t update');
    }
    const {
      options, parent, start, end, editorDidMount,
    } = this.props;
    console.log('parent doc has text:', parent.getValue());
    const doc = parent.linkedDoc({
      sharedHist: true,
      from: start,
      to: end,
      // This might inherit, or it might be different. Could just specify 'pyret'
      mode: null,
    });
    console.log('child doc has text:', doc.getValue());
    const editor = CM(this.tag.current, {
      ...options,
      value: doc,
      mode: 'pyret',
    }) as Editor;
    console.log('editor has text:', editor.getValue());
    this.setState({ editor });
    this.setupHandlers(editor);
    editorDidMount?.(editor);
  }

  setupHandlers(editor: Editor) {
    const {
      onMouseDown, onBeforeChange, onSelection, onKeyDown, onBlur,
    } = this.props;
    if (onMouseDown) { editor.on('mousedown', onMouseDown as any); }
    if (onBeforeChange) { editor.on('beforeChange', onBeforeChange as any); }
    if (onSelection) { editor.on('selection', onSelection as any); }
    if (onKeyDown) { editor.on('keydown', onKeyDown as any); }
    if (onBlur) { editor.on('blur', onBlur as any); }
  }

  render() {
    const {
      className, start,
    } = this.props;
    // cannot destructure null
    // eslint-disable-next-line
    if (this.state.editor !== undefined) {
      console.log('handlers');
      const { editor } = this.state;
      // Re-setup handlers to avoid stale closures
      // this.setupHandlers(editor);
      if (start !== editor.firstLine()) {
        console.log('respawn');
        this.tag = React.createRef();
        this.setState({
          editor: undefined,
        });
      }
    } else if (this.tag.current) {
      this.setupEditor();
    }
    return (
      <div ref={this.tag} className={className} />
    );
  }
}
