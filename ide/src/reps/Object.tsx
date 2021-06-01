/* A widget for displaying objects. Object documentation:
   https://www.pyret.org/docs/latest/_global_.html#%28part._~3cglobal~3e_.Object%29. */

import React from 'react';
import { ArrayWidget } from './List';

export default function ObjectWidget({ value, RenderedValue }:
{ value: object, RenderedValue: React.ReactType }) {
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/entries
  const asArray = Object.entries(value).sort((a, b) => b[0].localeCompare(a[0]));
  const tagged = asArray.map(([key, v]) => ({ renderKind: 'key-value', key, value: v }));
  return <ArrayWidget value={tagged} begin="{" end=" }" RenderedValue={RenderedValue} />;
}
