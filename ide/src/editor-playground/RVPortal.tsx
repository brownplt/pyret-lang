import React from 'react';
import CM from 'codemirror';
import ReactDOM from 'react-dom';
import ResizeObserver from 'react-resize-detector';
import { RHSObject } from '../rhsObject';
import RHSObjectComponent from '../RHSObjectComponent';

const lwOptions = { coverGutter: true };

interface RVState {
  portal: HTMLElement,
  widget?: CM.LineWidget,
}

type RVProps = {
  rhs: RHSObject[],
  editor: CM.Editor & CM.Doc,
  line: CM.LineHandle,
};

export default class RVPortal extends React.PureComponent<RVProps, RVState> {
  constructor(props: RVProps) {
    super(props);
    // TODO(luna): Check if constructor is right place for this. Does this
    // happen on every render? Is that bad?
    const portal = document.createElement('div');
    const { editor, line } = this.props;
    this.state = {
      portal,
      widget: editor.addLineWidget(line, portal, lwOptions),
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
      // eslint-disable-next-line
      this.setState({
        widget: editor.addLineWidget(line, portal, lwOptions),
      });
    }
    widget?.changed();
  }

  componentWillUnmount() {
    const { editor } = this.props;
    const { widget } = this.state;
    editor.removeLineWidget(widget as CM.LineWidget);
  }

  // We expect the following warning, presented as an error:
  // Warning: unstable_flushDiscreteUpdates: Cannot flush updates when React is
  // already rendering.
  // We haven't yet seen any issues due to this
  render() {
    const { rhs, editor } = this.props;
    const { portal, widget } = this.state;
    const rvs = rhs.map((rv, i) => (
      <ResizeObserver
        handleWidth={false}
        onResize={() => {
          widget?.changed();
          editor.focus();
        }}
        key={
          /* eslint-disable-next-line */
          i
        }
        skipOnMount
      >
        <RHSObjectComponent rhsObject={rv} isSelected={false} className="chunks-rhs" />
      </ResizeObserver>
    ));
    const content = rvs.length === 0 ? <hr /> : rvs;
    return ReactDOM.createPortal(
      <>{content}</>,
      portal,
    );
  }
}
