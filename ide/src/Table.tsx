import React from 'react';

type TableWidgetProps = {
    htmlify: (x: any) => any;
    headers: string[];
    rows: any[][];
};
type TableWidgetState = {};

export class TableWidget extends React.Component<TableWidgetProps, TableWidgetState> {
    render() {
        return (
            <table>
                <thead className="table-widget-header">
                    <tr>
                        {
                            this.props.headers.map((header) => {
                                return (
                                    <th key={header}>{this.props.htmlify(header)}</th>
                                );
                            })
                        }
                    </tr>
                </thead>
                <tbody className="table-widget-body">
                    {
                        this.props.rows.map((row, i) => {
                            return (
                                <tr key={i}>
                                    {
                                        row.map((x, j) => {
                                            return (
                                                <td key={`${i}:${j}`}>{this.props.htmlify(x)}</td>
                                            );
                                        })
                                    }
                                </tr>
                            );
                        })
                    }
                </tbody>
            </table>
        )
    }
}
