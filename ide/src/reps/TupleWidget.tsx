import React from 'react';
import type { ValueSkeleton } from '../../../src/runtime/runtime';
import { ArrayWidget } from './List';

type TWProps = { vals: ValueSkeleton[], expandable?: boolean, render: React.ElementType };
type TWState = {};

export default class TupleWidget extends React.Component<TWProps, TWState> {
  render() {
    return <ArrayWidget tag="sequence" value={this.props.vals} begin="{" end="}" sep=";" expandable={this.props.expandable} RenderedValue={this.props.render} />;
  }
}
