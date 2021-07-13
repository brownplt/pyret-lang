/* A widget for displaying objects. Object documentation:
   https://www.pyret.org/docs/latest/_global_.html#%28part._~3cglobal~3e_.Object%29. */

import React from 'react';
import { DataValue, isDataValue } from '../utils';
import { ArrayWidget } from './List';

export default function ObjectWidget({ value, RenderedValue }:
{ value: DataValue | object, RenderedValue: React.ReactType }) {
  let toRender;
  if (isDataValue(value)) {
    // We will grab our properties and render those, which avoids $properties
    // and any other metadata that may exist.
    const keys = value.$brand.names;
    toRender = keys.map((key: string) => ({ renderKind: 'key-value', key, value: value[key] }));
  } else {
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/entries
    const asArray = Object.entries(value).filter(([key]) => !key.startsWith('$')).sort((a, b) => b[0].localeCompare(a[0]));
    toRender = asArray.map(([key, v]) => ({ renderKind: 'key-value', key, value: v }));
  }
  return <ArrayWidget value={toRender} begin="{" end=" }" RenderedValue={RenderedValue} />;
}
