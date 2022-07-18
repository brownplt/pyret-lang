import React from 'react';
import type { ValueSkeleton } from '../../../src/runtime/runtime';
import NumWidget from './ExactNum';
import ImageWidget from './Image';
import { ArrayWidget } from './List';
import TableWidget from './Table';
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
      case 'vs-num': return <NumWidget v={value.v} />;
      case 'vs-literal-str': return value.s;
      case 'vs-str': return `"${value.s}"`; // TODO: replaceUnprintableStringChars
      case 'vs-function': return `<function: ${value.v.name}>`;
      case 'vs-method': return '<method>';
      case 'vs-nothing': return 'nothing';
      case 'vs-constr': {
        if (depth >= MAX_DEPTH) {
          return `${value.name}( ⋯ )`;
        }
        const zipped = value['field-names'].map((v, i) : [string, ValueSkeleton] => [v, value.args[i]]);
        return (
          <ArrayWidget
            tag="keyvals"
            keyvals={zipped}
            begin={`${value.name}(`}
            end=")"
            sep=","
            expandable={depth === 0}
            RenderedValue={recrender}
          />
        );
      }
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
      case 'vs-cyclic': {
        return `<cyclic value ${value.label}>`;
      }
      case 'vs-table': {
        if (depth >= MAX_DEPTH) {
          return '<table>';
        }
        return (
          <TableWidget
            headers={value.headers}
            rows={value.rows}
            RenderedValue={ValueSkeletonWidget}
          />
        );
      }
      case 'vs-other': {
        if (value.v.$brand === 'image') {
          return <ImageWidget image={value.v} />;
        } else {
          console.log('Unhandled valueskeleton render: ', value);
          return `unhandled valueskeleon render: ${value.$name}`;
        }
      }
      default: {
        console.log('Unhandled valueskeleton render: ', value);
        return `unhandled valueskeleon render: ${value.$name}`;
      }
    }
  }
}
