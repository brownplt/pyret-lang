/* Render a Range with more Ranges as pagination if necessary. */

import React from 'react';
import ExpandButton from './ExpandButton';
import { ContainerRange } from './Range';
import splitIndexRange from './split-index-range';

type RangeWidgetProps<T> = {
  value: ContainerRange<T>;
  expanded: boolean;
  expandable?: boolean;
  RenderedValue: React.ReactType;
};

export function RangeWidget<T>({ value, RenderedValue, expanded, expandable }: RangeWidgetProps<T>) {
  const [curExpanded, setExpanded]: [boolean, (to: boolean) => void] = (
    React.useState(expanded as boolean)
  );
  let afterArrow;
  if (curExpanded) {
    afterArrow = <RangeBoxesWidget tag="sequence" value={value} RenderedValue={RenderedValue} />;
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
  let className = 'list-container';
  if (curExpanded) { className += ' expanded'; }
  if (expandable) { className += ' expandable'; }
  return (
    <div className={className}>
      <ExpandButton expanded={curExpanded} setExpanded={setExpanded} />
      {afterArrow}
    </div>
  );
}

export function RangeBoxesWidget<T>({ tag, value, RenderedValue }:
{ tag: 'sequence' | 'keyvals', value: ContainerRange<T>, RenderedValue: React.ReactType }) {
  const boxes = splitIndexRange(value);
  let ranges: any[] = boxes;
  if (boxes.length === 1) {
    ranges = boxes[0].slice();
  }
  const style = { padding: '0.3em 1.6em', display: 'block' };
  const pipe = ranges.map((v, i) => {
    if (v instanceof ContainerRange) {
      // eslint-disable-next-line
      return <div style={style} key={i}><RangeWidget expandable expanded={false} value={v} RenderedValue={RenderedValue} /></div>;
    } else if (tag === 'sequence') {
      return (
        // eslint-disable-next-line
        <div style={style} key={i}>
          <RenderedValue value={v} depth={0} />
        </div>
      );
    } else {
      return (
        <div style={style} key={v[0]}>
          <div style={{ display: 'table-row', verticalAlign: 'top' }}>
            <span style={{ display: 'table-cell' }}>
              {v[0]}
              :
            </span>
            <div style={{ display: 'table-cell' }}><RenderedValue value={v[1]} depth={0} /></div>
          </div>
        </div>
      );
    }
  });
  return <>{pipe}</>;
}
