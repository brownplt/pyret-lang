/* A widget for displaying lists.
   https://www.pyret.org/docs/latest/lists.html.

   Lists are converted to JS arrays, which are then displayed using an array
   widget. Arrays also exist in Pyret:
   https://www.pyret.org/docs/latest/arrays.html

   Objects also use arrays to conveniently render themselves. Arrays are
   primarily rendered inside a Range for pagination and more */

import React from 'react';
import type { ValueSkeleton } from '../../../src/runtime/runtime';
import { intersperse, List } from '../utils';
import ExpandButton from './ExpandButton';
import { abbreviated, ContainerRange } from './Range';
import { RangeBoxesWidget, RangeWidget } from './RangeWidget';

type ListWidgetProps = {
  value: List<any>;
  RenderedValue: React.ReactType;
};

function toJSArray<T>(inList: List<T>): Array<T> {
  const res = [];
  let rest = inList;
  while (rest.$name !== 'empty') {
    res.push(rest.first);
    rest = rest.rest;
  }
  return res;
}

export default function ListWidget({ value, RenderedValue }: ListWidgetProps) {
  const asArray = toJSArray(value);
  return <ArrayWidget tag="sequence" value={asArray} begin="[list:" end="]" sep="," RenderedValue={RenderedValue} />;
}

type ArrayWidgetProps = {
  begin: string;
  end: string;
  sep: string;
  expandable?: boolean;
  RenderedValue: React.ReactType;
} & (
  { tag: 'sequence', value: Array<ValueSkeleton> }
  | { tag: 'keyvals', keyvals: Array<[string, ValueSkeleton]>}
);

export function ArrayWidget(props : ArrayWidgetProps) {
  const [expanded, setExpanded]: [boolean, (to: boolean) => void] = (
    React.useState(false as boolean)
  );
  const { tag, begin, end, sep, expandable, RenderedValue } = props;
  let pipe : any[] = props.tag === 'sequence' ? props.value : props.keyvals;
  if (expanded) {
    const range = new ContainerRange(pipe, 0, pipe.length - 1);
    pipe = [<RangeBoxesWidget tag={tag} value={range} key="one" RenderedValue={RenderedValue} />];
  } else {
    const style = { display: 'inline-block' };
    pipe = abbreviated(pipe);
    pipe = pipe.map((v, i) => {
      if (typeof v === 'object' && v instanceof ContainerRange) {
        // eslint-disable-next-line
        return <div style={style} key={i}><RangeWidget value={v} expanded={false} RenderedValue={RenderedValue} /></div>;
      } else if (tag === 'sequence') {
        // eslint-disable-next-line
        return <div style={style} key={i}><RenderedValue value={v} inlineOrExpanded={false} /></div>;
      } else {
        return (
          <span key={v[0]}>
            {v[0]}
            :
            <div style={style}><RenderedValue value={v[1]} inlineOrExpanded={false} /></div>
          </span>
        );
      }
    });
    pipe = intersperse(pipe,
      <span>
        {sep}
      </span>);
  }
  let className = 'list-container';
  if (expanded) { className += ' expanded'; }
  if (expandable) { className += ' expandable'; }
  return (
    <div className={className}>
      <ExpandButton expanded={expanded} setExpanded={setExpanded} />
      {begin}
      {pipe}
      {end}
    </div>
  );
}
