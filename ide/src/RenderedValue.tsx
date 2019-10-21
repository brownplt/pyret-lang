import React from 'react';

import { TableWidget } from './Table';
import { ImageWidget } from './Image';

type RenderedValueProps = {
  value: any;
};

type RenderedValueState = {};

function convert(value: any) {
  if (value === undefined) {
    return "undefined";
  } else if (typeof value === 'number') {
    return value.toString();
  } else if (typeof value === 'string') {
    return `"${value}"`;
  } else if (typeof value === 'boolean') {
    return value.toString();
  } else if (typeof value === 'function') {
    // TODO(michael) can we display more info than just <function> ?
    return "<function>";
  } else if (value.$brand === '$table') {
    return (
      <TableWidget headers={value._headers}
        rows={value._rows}
        htmlify={(v) => convert(v)}>
      </TableWidget>
    );
  } else if (value.$brand === 'image') {
    return (
      <ImageWidget image={value}>
      </ImageWidget>
    );
  } else if (typeof value === 'object') {
    if(Array.isArray(value) && value.length > 100) {
      const message = (value.length - 100) + " elements hidden"
      return JSON.stringify(value.slice(0, 100).concat(["... " + message]));
    }
    // TODO(michael) palceholder for better object display
    return JSON.stringify(value);
  }
};


export class RenderedValue extends React.Component<RenderedValueProps, RenderedValueState> {
  render() {
    return convert(this.props.value);
  }
}
