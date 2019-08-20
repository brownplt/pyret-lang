import React from 'react';

type TableWidgetProps = {
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
                                    <th key={header}>{header}</th>
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
                                        row.map((x) => {
                                            return (
                                                <td key={x}>{x}</td>
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
