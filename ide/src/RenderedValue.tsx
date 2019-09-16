import React from 'react';

import { TableWidget } from './Table';
import { ImageWidget } from './Image';

type RenderedValueProps = {
  value: any;
  setMessage: (newMessage: string) => void;
};

type RenderedValueState = {

};
function convert(value: any, setMessage : (newMessage: string) => void) {
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
        htmlify={(v) => convert(v, setMessage)}
        setMessage={setMessage}>
      </TableWidget>
    );
  } else if (value.$brand === 'image') {
    return (
      <ImageWidget image={value}>
      </ImageWidget>
    );
  } else if (typeof value === 'object') {
    // TODO(michael) palceholder for better object display
    return JSON.stringify(value);
  }
};


export class RenderedValue extends React.Component<RenderedValueProps, RenderedValueState> {
  render() {
    return convert(this.props.value, this.props.setMessage);
  }
}
