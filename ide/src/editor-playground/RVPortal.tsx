import React from 'react';
import CM from 'codemirror';
import ReactDOM from 'react-dom';
import ResizeObserver from 'react-resize-detector';
import { isTrace, RHSObject } from '../rhsObject';
import RenderedValue from '../reps/RenderedValue';

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
    const traces = rhs.filter(isTrace);
    const values = traces.map((oneRHS) => oneRHS.value);
    const rvs = values.map((rv, i) => (
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
        {/* A rendered value can be a DOM string ("5"), but ResizeObserver
        expects a DOM *Element* */}
        <div>
          <RenderedValue value={rv} />
        </div>
      </ResizeObserver>
    ));
    return ReactDOM.createPortal(
      <>{rvs}</>,
      portal,
    );
  }
}
