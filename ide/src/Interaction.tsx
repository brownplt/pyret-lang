import React from 'react';
import {TableWidget} from './Table';
import {ImageWidget} from './Image';

type InteractionProps = {
    name: string,
    value: any
};

type InteractionState = {};

export class Interaction extends React.Component<InteractionProps, InteractionState> {
    convert = (value: any) => {
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
                             htmlify={this.convert}>
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

    render() {
        return (
            <div className="interaction">
                <pre>
                    {this.props.name} =&nbsp;
                </pre>
                {this.convert(this.props.value)}
            </div>
        )
    };
}
