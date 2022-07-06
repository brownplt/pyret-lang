import React from 'react';
import type { ValueSkeleton } from '../../../src/runtime/runtime';
import ExactNumWidget from './ExactNum';
import { ArrayWidget } from './List';
import TupleWidget from './TupleWidget';

type VSWidgetProps = { value: ValueSkeleton, depth?: number };
type VSWidgetState = any;

const MAX_DEPTH = 3;

export default class ValueSkeletonWidget extends React.Component<VSWidgetProps, VSWidgetState> {
  render() {
    const { value } = this.props;
    const depth = this.props.depth ?? 0;
    const recrender = (props : VSWidgetProps) => {
      const newDepth = props.depth ?? depth + 1;
      return <ValueSkeletonWidget value={props.value} depth={newDepth} />;
    };
    switch (value.$name) {
      case 'vs-bool': return String(value.v);
      case 'vs-num': {
        if (typeof value.v === 'number') {
          return String(value.v);
        }
        return <ExactNumWidget num={value.v.n} den={value.v.d} />;
      }
      case 'vs-literal-str': return value.s;
      case 'vs-str': return `"${value.s}"`; // TODO: replaceUnprintableStringChars
      case 'vs-function': return `<function: ${value.v.name}>`;
      case 'vs-method': return '<method>';
      case 'vs-nothing': return 'nothing';
      case 'vs-record': {
        if (depth >= MAX_DEPTH) {
          return '{ ⋯ }';
        }
        const zipped = value['field-names'].map((v, i) : [string, ValueSkeleton] => [v, value.vals[i]]);
        return (
          <ArrayWidget
            tag="keyvals"
            keyvals={zipped}
            begin="{"
            end="}"
            sep=","
            expandable={depth === 0}
            RenderedValue={recrender}
          />
        );
      }
      case 'vs-collection': {
        if (depth >= MAX_DEPTH) {
          return `[${value.name}: ⋯]`;
        }
        return (
          <ArrayWidget
            tag="sequence"
            value={value.items}
            begin={`[${value.name}: `}
            end="]"
            sep=", "
            expandable={depth === 0}
            RenderedValue={recrender}
          />
        );
      }
      case 'vs-tuple': {
        if (depth >= MAX_DEPTH) {
          return '{ ⋯ }';
        }
        return <TupleWidget vals={value.vals} expandable={depth === 0} render={recrender} />;
      }
      default: {
        console.log('Unhandled valueskeleton render: ', value);
        return `unhandled valueskeleon render: ${value.$name}`;
      }
    }
  }
}
