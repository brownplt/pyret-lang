/* Render a Range with more Ranges as pagination if necessary. */

import React from 'react';
import { ContainerRange } from './Range';
import { splitIndexRange } from './split-index-range';

type RangeWidgetProps<T> = {
  value: ContainerRange<T>;
  RenderedValue: React.ReactType
};

export function RangeWidget<T>({ value, RenderedValue }: RangeWidgetProps<T>) {
  // eslint doesn't like me breaking this up any prettier way
  const [expanded, setExpanded]:
  [boolean, (to: boolean) => void] = React.useState(false as boolean);
  let afterArrow;
  if (expanded) {
    afterArrow = <RangeBoxesWidget value={value} RenderedValue={RenderedValue} />;
  } else {
    const style = { backgroundColor: '#eee', padding: '0.1em 0.4em' };
    afterArrow = (
      <span style={style}>
        {value.min}
        -
        {value.max}
        ...
      </span>
    );
  }
  return (
    <div className="list-container">
      {/* down arrow, right arrow */}
      <button type="button" onClick={() => setExpanded(!expanded)}>
        {expanded ? '\u25BC' : '\u25B6'}
      </button>
      {afterArrow}
    </div>
  );
}

export function RangeBoxesWidget<T>({ value, RenderedValue }:
{ value: ContainerRange<T>, RenderedValue: React.ReactType }) {
  const boxes = splitIndexRange(value);
  let ranges: any[] = boxes;
  if (boxes.length === 1) {
    ranges = boxes[0].slice();
  }
  const style = { padding: '0.3em 1.6em' };
  const pipe = ranges.map((v, i) => (
    // eslint-disable-next-line
    <div style={style} key={i}>
      <RenderedValue value={v} />
    </div>
  ));
  return <>{pipe}</>;
}
