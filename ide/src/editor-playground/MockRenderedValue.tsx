import React from 'react';
import CM from 'codemirror';
import ReactDOM from 'react-dom';

interface RVState {
  portal: HTMLElement,
  widget?: CM.LineWidget,
}

type RVProps = {
  chunkLength: number, programLength: number, editor: CM.Editor & CM.Doc
  line: CM.LineHandle,
};

export default class MockRV extends React.PureComponent<RVProps, RVState> {
  constructor(props: RVProps) {
    super(props);
    // TODO(luna): Check if constructor is right place for this. Does this
    // happen on every render? Is that bad?
    const portal = document.createElement('div');
    portal.append('sthoeunth long string');
    console.log('constructor of MockRV. This shouldnt happen often', props.line);
    const { editor, line } = this.props;
    this.state = {
      portal,
      widget: editor.addLineWidget(line, portal),
    };
  }

  componentDidMount() {
    const { widget } = this.state;
    widget?.changed();
  }

  componentDidUpdate(oldProps: RVProps) {
    const { editor, line } = this.props;
    const { portal, widget } = this.state;
    if (oldProps.line !== line) {
      editor.removeLineWidget(widget as CM.LineWidget);
      editor.addLineWidget(line, portal);
    }
    // this.state.widget?.changed();
  }

  componentWillUnmount() {
    const { editor } = this.props;
    const { widget } = this.state;
    editor.removeLineWidget(widget as CM.LineWidget);
    console.log('removing a line widget (componentWilLUnMount)');
  }

  render() {
    const { chunkLength, programLength } = this.props;
    const { portal } = this.state;
    // this.state.widget?.changed();
    return ReactDOM.createPortal(
      <span>
        Chunk length:
        {chunkLength}
        ; Program length:
        {programLength}
      </span>,
      portal,
    );
  }
}
