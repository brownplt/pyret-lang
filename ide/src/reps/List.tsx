/* A widget for displaying lists.
   https://www.pyret.org/docs/latest/lists.html.

   Lists are converted to JS arrays, which are then displayed using an array
   widget. Arrays also exist in Pyret:
   https://www.pyret.org/docs/latest/arrays.html

   Objects also use arrays to conveniently render themselves. Arrays are
   primarily rendered inside a Range for pagination and more */

import React from 'react';
import { abbreviated, ContainerRange } from './Range';
import { RangeBoxesWidget } from './RangeWidget';
import SummaryValue from './SummaryValue';

type ListWidgetProps = {
  value: List<any>;
  RenderedValue: React.ReactType;
};

type List<T> = { $tag: 0 } | { $tag: 1, first: T, rest: List<T> };

function toJSArray<T>(inList: List<T>): Array<T> {
  const res = [];
  let rest = inList;
  while (rest.$tag !== 0) {
    res.push(rest.first);
    rest = rest.rest;
  }
  return res;
}

function intersperse(array: Array<JSX.Element>, btwn: JSX.Element): Array<JSX.Element> {
  return array.slice(1).reduce((acc, comp) => acc.concat([btwn, comp]), [array[0]]);
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
  // eslint doesn't like me breaking this up any prettier way
  const [expanded, setExpanded]:
  [boolean, (to: boolean) => void] = React.useState(false as boolean);
  let pipe = value;
  if (expanded) {
    const range = new ContainerRange(value, 0, value.length - 1);
    pipe = [<RangeBoxesWidget value={range} key="one" RenderedValue={RenderedValue} />];
  } else {
    pipe = abbreviated(value);
    pipe = pipe.map((v) => <SummaryValue value={v} />);
    pipe = intersperse(pipe, <span>, </span>);
    // We need unique keys or react will complain
    // Why don't you set the keys before? Because what would i set for comma?
    // Why don't you use cloneElement? Because JSX.Element doesn't have it and
    // there is NO DOCUMENTATION that i could find for that type
    // Why isn't there documentation for that? i don't know and i hate it
    // Why disabling eslint? Because the values are not unique in any way, so to be
    // unique they all need an arbitrary unique key. That's what indices are for
    // Why am i not allowed to do that? i don't know and i hate it
    // - luna
    const style = { display: 'inline-block' };
    pipe = pipe.map((v, i) => (
      // eslint-disable-next-line
      <div style={style} key={i}>{v}</div>));
  }
  return (
    <div className="list-container">
      {/* down arrow, right arrow */}
      <button type="button" onClick={() => setExpanded(!expanded)}>
        {expanded ? '\u25BC' : '\u25B6'}
      </button>
      {' '}
      {begin}
      {' '}
      {pipe}
      {end}
    </div>
  );
}
