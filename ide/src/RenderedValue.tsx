/* Renders Pyret values into HTML. */

import React from 'react';

import TableWidget from './Table';
import ImageWidget from './Image';
import ChartWidget from './Chart';
import ReactorWidget from './Reactor';

import { isRHSCheck } from './rhsObject';

import {
  isSpyMessage,
  isSpyValue,
  isRTMessage,
  RawRTMessage,
} from './rtMessages';

type RenderedValueProps = {
  value: any;
};

type RenderedValueState = {};

function convert(value: any): any {
  if (value === undefined) {
    return 'undefined';
  }

  if (typeof value === 'number') {
    // console.log('convert', value);
    return value.toString();
  }

  if (typeof value === 'string') {
    return `"${value}"`;
  }

  if (typeof value === 'boolean') {
    return value.toString();
  }

  if (typeof value === 'function') {
    // TODO(michael) can we display more info than just <function> ?
    return '<function>';
  }

  if (value.$brand === '$table') {
    return (
      <TableWidget
        headers={value._headers}
        rows={value._rows}
        htmlify={(v) => convert(v)}
      />
    );
  }

  if (value.$brand === 'image') {
    return (
      <ImageWidget image={value} />
    );
  }

  if (value.$brand === 'chart') {
    return (
      <ChartWidget
        headers={value._headers}
        rows={value._rows}
        chartType={value.chartType}
      />
    );
  }

  if (value.$brand === 'reactor') {
    return (
      <ReactorWidget reactor={value} convert={convert} />
    );
  }

  if (value['$template-not-finished'] !== undefined) {
    return (
      <div>
        an expression containing a template
      </div>
    );
  }

  if (typeof value === 'object') {
    if (Array.isArray(value) && value.length > 100) {
      const message = `${value.length - 100} elements hidden`;
      return JSON.stringify(value.slice(0, 100).concat([`... ${message}`]));
    }

    if (isRTMessage(value)) {
      const messageData: RawRTMessage = value.data;
      if (isSpyValue(messageData)) {
        return `${messageData.value.key} = ${convert(messageData.value.value)} (${messageData.loc})`;
      }

      if (isSpyMessage(value.data)) {
        return messageData.value ? `Spying "${messageData.value}" at: ${messageData.loc}` : `Spying at ${messageData.loc}`;
      }
    }

    if (isRHSCheck(value)) {
      return (
        <div>
          Test
          {' '}
          {value.success ? 'succeeded' : 'failed'}
          {' '}
          at
          {' '}
          {value.loc}
          {value.success === false && (
            <div style={{
              paddingLeft: '1em',
              display: 'flex',
              flexDirection: 'column',
            }}
            >
              <div style={{
                display: 'flex',
                alignItems: 'center',
              }}
              >
                {'The left side was: '}
                {value.lhs.exception === true ? (
                  <RenderedValue value={value.lhs.exception_val} />
                ) : (
                  <RenderedValue value={value.lhs.value} />
                )}
              </div>
              <div style={{
                display: 'flex',
                alignItems: 'center',
              }}
              >
                {'The right side was: '}
                {value.rhs.exception === true ? (
                  <RenderedValue value={value.rhs.exception_val} />
                ) : (
                  <RenderedValue value={value.rhs.value} />
                )}
              </div>
            </div>
          )}
        </div>
      );
    }

    // TODO(michael) palceholder for better object display
    console.log('idk: ', value);
    return `${value}`;
  }
  return 'error: data is not string-convertible';
}

export default class RenderedValue extends React.Component<RenderedValueProps, RenderedValueState> {
  render() {
    const { value } = this.props;
    return convert(value);
  }
}
