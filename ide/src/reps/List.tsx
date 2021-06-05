/* A widget for displaying lists.
   https://www.pyret.org/docs/latest/lists.html.

   Lists are converted to JS arrays, which are then displayed using an array
   widget. Arrays also exist in Pyret:
   https://www.pyret.org/docs/latest/arrays.html

   Objects also use arrays to conveniently render themselves. Arrays are
   primarily rendered inside a Range for pagination and more */

import React from 'react';
import { intersperse, List } from '../utils';
import ExpandButton from './ExpandButton';
import { abbreviated, ContainerRange } from './Range';
import { RangeBoxesWidget } from './RangeWidget';
import SummaryValue from './SummaryValue';

type ListWidgetProps = {
  value: List<any>;
  RenderedValue: React.ReactType;
};

function toJSArray<T>(inList: List<T>): Array<T> {
  const res = [];
  let rest = inList;
  while (rest.$tag !== 0) {
    res.push(rest.first);
    rest = rest.rest;
  }
  return res;
}

export default function ListWidget({ value, RenderedValue }: ListWidgetProps) {
  const asArray = toJSArray(value);
  return <ArrayWidget value={asArray} begin="[list:" end="]" RenderedValue={RenderedValue} />;
}

type ArrayWidgetProps = {
  value: Array<any>;
  begin: string;
  end: string;
  RenderedValue: React.ReactType;
};

export function ArrayWidget({
  value, begin, end, RenderedValue,
}: ArrayWidgetProps) {
  const [expanded, setExpanded]: [boolean, (to: boolean) => void] = (
    React.useState(false as boolean)
  );
  let pipe = value;
  if (expanded) {
    const range = new ContainerRange(value, 0, value.length - 1);
    pipe = [<RangeBoxesWidget value={range} key="one" RenderedValue={RenderedValue} />];
  } else {
    pipe = abbreviated(value);
    pipe = pipe.map((v) => <SummaryValue value={v} />);
    pipe = intersperse(pipe, <span>, </span>);
    const style = { display: 'inline-block' };
    pipe = pipe.map((v, i) => (
      // Rendering an array is a good reason to use indices as keys - they have
      // no other unique metadata
      // eslint-disable-next-line
      <div style={style} key={i}>{v}</div>));
  }
  return (
    <div className="list-container">
      <ExpandButton expanded={expanded} setExpanded={setExpanded} />
      {' '}
      {begin}
      {' '}
      {pipe}
      {end}
    </div>
  );
}
