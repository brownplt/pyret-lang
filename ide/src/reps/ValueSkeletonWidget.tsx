import React from 'react';
import type { ValueSkeleton } from '../../../src/runtime/runtime';
import ExactNumWidget from './ExactNum';

type VSWidgetProps = { value: ValueSkeleton };
type VSWidgetState = any;

export default class ValueSkeletonWidget extends React.Component<VSWidgetProps, VSWidgetState> {
  render() {
    const { value } = this.props;
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
      case 'vs-tuple': {
        return 'tuplewidget';
      }
      default: {
        return `unhandled vs render: ${value.$name}`;
      }
    }
  }
}
