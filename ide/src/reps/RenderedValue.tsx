/* Renders Pyret values into HTML. */

import React from 'react';

import { getRenderKind } from './RenderKind';

/* eslint-disable-next-line */
import TableWidget from './Table';
import ListWidget, { ArrayWidget } from './List';
import ImageWidget from './Image';
import ChartWidget from './Chart';
import ReactorWidget from './Reactor';
import { RawRTMessage } from '../rtMessages';
import { RangeWidget } from './RangeWidget';
import NumWidget from './ExactNum';
import { NeverError } from '../utils';

import RenderedValueWithOutput from './RenderedValueWithOutput';

const USE_VALUESKELETON = true;

declare global {
  interface window { theKey: any; }
}

type RenderedValueProps = {
  value: any;
};

type RenderedValueState = {};

/* eslint-disable */
export default class RenderedValue extends React.Component<RenderedValueProps, RenderedValueState> {
  render() {
    const { value } = this.props;
    const kind = getRenderKind(value);
    if (kind === 'reactor') {
      return (
        <ReactorWidget reactor={value} RenderedValue={RenderedValue} />
      );
    }
    if(USE_VALUESKELETON) { return <RenderedValueWithOutput value={this.props.value} />; }
    switch (kind) {
      case 'undefined':
      case 'number':
      case 'boolean':
        return String(value);
      case 'string':
        return `"${value}"`;
      case 'function':
        // TODO(michael) can we display more info than just <function> ?
        return '<function>';
      case 'exactnum':
        return <NumWidget v={value} />;
      case 'table':
        return (
          <TableWidget
            headers={value._headers}
            rows={value._rows}
            RenderedValue={RenderedValue}
          />
        );
      case 'image':
        return (
          <ImageWidget image={value} />
        );
      case 'chart':
        return (
          <ChartWidget
            headers={value._headers}
            rows={value._rows}
            chartType={value.chartType}
          />
        );
      case 'string-dict':
        const keys = value.$underlyingMap.keys();
        const toRender = keys.flatMap((key: string) => [key, value.$underlyingMap.get(key)]);
        const constructorName = value.$brand === 'mutable-string-dict' ? 'mutable-string-dict' : 'string-dict';
        return <ArrayWidget tag="sequence" value={toRender} begin={`[${constructorName}:`} end="]" sep="," RenderedValue={RenderedValue} />
      case 'template':
        return (
          <div>
            an expression containing a template
          </div>
        );
      case 'list':
        return (
          <ListWidget value={value} RenderedValue={RenderedValue} />
        );
      case 'array':
        <ArrayWidget tag="sequence" value={value} begin="[array:" end="]" sep="," RenderedValue={RenderedValue} />
      case 'spy-value': {
        const messageData: RawRTMessage = value.data;
        return (
          <div>
            {messageData.value.key} =
            <RenderedValue value={messageData.value.value} />
            ({messageData.loc})
          </div>
        );
      }
      case 'spy-message': {
        const messageData: RawRTMessage = value.data;
        return messageData.value ? (
          <div>
            Spying "<RenderedValue value={messageData.value} />" at:
            {messageData.loc}
          </div>
        ) : `Spying at ${messageData.loc}`;
      }
      case 'data-value': {
        // We will grab our properties and render those, which avoids $properties
        // and any other metadata that may exist.
        const keys = value.$brand.names;
        const toRender = keys.map((key: string) => ({ renderKind: 'key-value', key, value: value[key] }));
        return <ArrayWidget tag="sequence" value={toRender as any} begin="{" end=" }" sep="," RenderedValue={RenderedValue} />;
      }
      case 'object': {
        // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/entries
        const asArray = Object.entries(value).filter(([key]) => !key.startsWith('$')).sort((a, b) => b[0].localeCompare(a[0]));
        const toRender = asArray.map(([key, v]) => ({ renderKind: 'key-value', key, value: v }));
        return <ArrayWidget tag="sequence" value={toRender as any} begin="{" end=" }" sep="," RenderedValue={RenderedValue} />;
      }
      case 'range':
        return <RangeWidget expanded={false} value={value} RenderedValue={RenderedValue} />;
      case 'key-value':
        return (
          <React.Fragment>
            {/* only string keys are allowed */}
            {value.key}: <RenderedValue value={value.value} />
          </React.Fragment>
        );
      default:
        throw new NeverError(kind);
    }
  }
}
