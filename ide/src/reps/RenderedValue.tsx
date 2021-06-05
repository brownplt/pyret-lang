/* Renders Pyret values into HTML. */

import React from 'react';

import { getRenderKind, NeverError } from './RenderKind';

/* eslint-disable-next-line */
import TableWidget from './Table';
import ListWidget, { ArrayWidget } from './List';
import ObjectWidget from './Object';
import ImageWidget from './Image';
import ChartWidget from './Chart';
import ReactorWidget from './Reactor';
import { RawRTMessage } from '../rtMessages';
import { RangeWidget } from './RangeWidget';
import ExactNumWidget from './ExactNum';

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
        return <ExactNumWidget num={value.n} den={value.d} />;
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
      case 'reactor':
        return (
          <ReactorWidget reactor={value} RenderedValue={RenderedValue} />
        );
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
        <ArrayWidget value={value} begin="[array:" end="]" RenderedValue={RenderedValue} />
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
      case 'check':
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
      case 'object':
        return <ObjectWidget value={value} RenderedValue={RenderedValue} />;
      case 'range':
        return <RangeWidget value={value} RenderedValue={RenderedValue} />;
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
