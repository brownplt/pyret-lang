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
  options?: CM.EditorConfiguration & { matchBrackets?: boolean, matchKeywords?: boolean },
  onBeforeChange?: (editor: Editor, data: CM.EditorChange, value: string) => void,
  onSelection?: (editor: Editor, data: any) => void,
  onKeyDown?: (editor: Editor, event: InputEvent) => void,
  // autoCursor: boolean,
  onBlur?: (editor: Editor) => void,
  className?: string,
};
type State = {};

export default class LinkedCM extends React.Component<Props, State> {
  private tag: React.RefObject<HTMLDivElement>;

  constructor(props: Props) {
    super(props);
    this.tag = React.createRef();
  }

  componentDidMount() {
    // TODO(luna): Fix the refs
    if (this.tag.current === null) {
      throw new Error('ref didn\'t update');
    }
    const {
      parent, start, end, editorDidMount, onMouseDown, options, onBeforeChange,
      onSelection, onKeyDown, onBlur,
    } = this.props;
    const doc = parent.linkedDoc({
      sharedHist: true,
      from: start,
      to: end,
      // This might inherit, or it might be different. Could just specify 'pyret'
      mode: null,
    });
    const editor = CM(this.tag.current, {
      ...options,
      value: doc,
      mode: 'pyret',
    });
    editor.on('mousedown', onMouseDown as any);
    editor.on('beforeChange', onBeforeChange as any);
    editor.on('selection', onSelection as any);
    editor.on('keydown', onKeyDown as any);
    editor.on('blur', onBlur as any);
    editorDidMount?.(editor as Editor);
  }

  render() {
    const { className } = this.props;
    return (
      <div ref={this.tag} className={className} />
    );
  }
}