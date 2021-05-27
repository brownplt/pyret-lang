/* A widget for displaying tables. Table documentation:
   https://www.pyret.org/docs/latest/tables.html.

   Tables are created in RenderedValue.tsx */

/* eslint-disable */

import React from 'react';
import { ValueRendererUnconnected } from './reps/components/value-renderer';
// import ValueSummary from './reps/components/value-summary';
// import { serializeForValueSummary } from './reps/serialization/value-summary-serializer';
import { makeValueRendererWithRepRequest } from './reps/components/rep-info-requestor';
import { repInfoRequestResponse } from './reps/serialization/rep-info-request-response';
import { serializeForValueSummary } from './reps/serialization/value-summary-serializer';
import ExpandableRep from './reps/components/rep-tree';
import { getChildSummary, getObjAtPath } from './reps/serialization/get-child-summaries';
import ValueSummary from './reps/components/value-summary';

type ListWidgetProps = {
  htmlify: (x: any) => any;
  value: List<any>;
};

type List<T> = { $tag: 0 } | { $tag: 1, first: T, rest: List<T> };

function toJSArray<T>(inList: List<T>): Array<T> {
  // This is a hack to rename the "summary" in the serializer
  class list extends Array {
    constructor() { super() }
  }
  const res: any = new list();
  let rest = inList;
  while (rest.$tag !== 0) {
    res.push(rest.first);
    rest = rest.rest;
  }
  return res;
}

declare global {
  interface window { theKey: any; }
}

export default function ListWidget({ htmlify, value }: ListWidgetProps) {
  const asArray = toJSArray(value);
  if (typeof (window as any).theKey === 'undefined') {
  (window as any).theKey = {};
  }
  let actualKey = (value as any)['$id'];
  if (typeof actualKey === 'undefined') {
    const newKey = Math.random().toString();
    (value as any)['$id'] = newKey;
    actualKey = newKey;
  }
  (window as any).theKey[actualKey] = asArray;
  const serializedValueSummary = serializeForValueSummary(
    (window as any).theKey[actualKey]
  );
  return (
    <ExpandableRep
      pathToEntity={[actualKey]}
      valueSummary={serializedValueSummary}
      getChildSummaries={(path: any) => getChildSummary("theKey", path)}
      renderChild={htmlify}
      rootObjName="theKey"
    />
  ); 

//  const mockUserRenderManager = {
//    getUserRepIfAvailable: htmlify,
//  };
// //
//  function repInfoRequestResponseWithMockUserReps(payload: any) {
//    console.log(payload);
//    return repInfoRequestResponse(payload, {
//      userRepManager: mockUserRenderManager,
//    });
//  }
// //
//  const ValueRenderer = makeValueRendererWithRepRequest(
//    ValueRendererUnconnected,
//    repInfoRequestResponseWithMockUserReps,
//    'window',
//  );
// //
//  const asArray = toJSArray(value);
//  /* eslint-disable */
//  (window as any)['theKey'] = asArray;
//  return <ValueRenderer valockUserRenderManager = {
//    getUserRepIfAvailable: htmlify,
//  };
// //
//  function repInfoRequestResponseWithMockUserReps(payload: any) {
//    console.log(payload);
//    return repInfoRequestResponse(payload, {
//      userRepManager: mockUserRenderManager,
//    });
//  }
// //
//  const ValueRenderer = makeValueRendererWithRepRequest(
//    ValueRendererUnconnected,
//    repInfoRequestResponseWithMockUserReps,
//    'window',
//  );
// //
//  const asArray = toJSArray(value);
//  /* eslint-disable */
//  (window as any)['theKey'] = asArray;
//  return <ValueueKey="theKey">{asArray}</ValueRenderer>;


//  const asArray = toJSArray(value);
//   const serialized = serializeForValueSummary(asArray);
//   /* eslint-disable */
//   const topLevel = <ValueSummary {...serialized} />;
//   function getChildSummaries(path: string) {
//     console.log(path);
//     return {
//       childItems: asArray.map((v, i) => (
//         <div key={/* eslint-disable */ i}>{htmlify(v)}</div>)
//       )
//     };
//   }
//   return (
//     <div>
//       {topLevel}
//       <ValueRendererUnconnected
//         topLevelRepSummary={topLevel}
//         pathToEntity={["ROOT"]}
//         requestRepInfo={getChildSummaries}
//       >
//         {asArray}
//       </ValueRendererUnconnected>
//     </div>
//   );
  // remove this if what's about to happen works
//  const NUM_INLINE_ITEMS = 20;
//  // eslint doesn't like me breaking this up any prettier way
//  const [expanded, setExpanded]:
//  [boolean, (to: boolean) => void] = React.useState(false as boolean);
//  const components = asArray.map(htmlify);
//  const style = { display: 'inline-block' };
//  const inline = components.map((v) => (
//    <div style={style}>
//      {v}
//    </div>
//  ));
//  const abbrev = inline.slice(0, NUM_INLINE_ITEMS);
//  if (abbrev.length !== inline.length) {
//    abbrev.push(<span>...</span>);
//  }
//  const withCommas = intersperse(abbrev, <span>, </span>);
//  // We need unique keys or react will complain
//  // Why don't you set the keys before? Because what would i set for comma?
//  // Why don't you use cloneElement? Because JSX.Element doesn't have it and
//  // there is NO DOCUMENTATION that i could find for that type
//  // Why isn't there documentation for that? i don't know and i hate it
//  // Why eslint-disable? Because the values are not unique in any way, so to be
//  // unique they all need an arbitrary unique key. That's what indices are for
//  // Why am i not allowed to do that? i don't know and i hate it
//  // - luna
//  const withKeys = withCommas.map((v, i) => (
//    <div style={style} key={/* eslint-disable */ i}>{v}</div>)
//  );
//  const verticalPadding = { padding: '0.3em 1.6em' };
//  const vertical = components.map((v) => <div style={verticalPadding}>{v}</div>);
//  return (
//    <div className="list-container">
//      {/* down arrow, right arrow */}
//      <button onClick={() => setExpanded(!expanded)}>{expanded ? "\u25BC" : "\u25B6"}</button>
//      <span> [list:</span> {withKeys}]
//      {expanded ? vertical : ""}
//    </div>
//  );
}
