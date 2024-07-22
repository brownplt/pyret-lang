/* In inline array summaries and similar places, we render extremely abbreviated
 * values */

import React from 'react';

import { getRenderKind } from './RenderKind';
import { NeverError } from '../utils';

import ImageWidget from './Image';
import NumWidget from './ExactNum';

declare global {
  interface window { theKey: any; }
}

type ValueSummaryProps = {
  value: any;
};

type ValueSummaryState = {};

export default class SummaryValue extends React.Component<ValueSummaryProps, ValueSummaryState> {
  render() {
    const { value } = this.props;
    const kind = getRenderKind(value);
    switch (kind) {
      case 'undefined':
      case 'number':
      case 'boolean':
        return String(value);
      case 'string':
        return `"${value}"`;
      case 'exactnum':
        return <NumWidget v={value} />;
      // In general we want to be "annoyingly short" for kinds that
      // - are recursive (list)
      // - are annoyingly big (table)
      // - have no reasonable representation at all (function)
      case 'function':
      case 'table':
      case 'chart':
      case 'template':
      case 'list':
      case 'array':
      case 'data-value':
      case 'object':
      case 'spy-value':
      case 'spy-message':
      case 'reactor':
      case 'string-dict':
        return `<${kind}>`;
      case 'image':
        return (
          <ImageWidget image={value} />
        );
      case 'range': {
        const style = { backgroundColor: '#ccc', padding: '0.1em 0.4em' };
        return (
          <span style={style}>
            {'... '}
            {value.size()}
            {' more'}
          </span>
        );
      }
      case 'key-value':
        return (
          <>
            {/* only string keys are allowed */}
            {value.key}
            {': '}
            <SummaryValue value={value.value} />
          </>
        );
      default:
        throw new NeverError(kind);
    }
  }
}
