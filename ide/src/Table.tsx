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
            <div className="table-container">
                <table className="table-widget">
                    <thead className="table-widget-header">
                        <tr>
                            {
                                this.props.headers.map((header) => {
                                    return (
                                        <th key={header}
                                            className="table-header-item">
                                            {header}
                                        </th>
                                    );
                                })
                            }
                        </tr>
                    </thead>
                    <tbody>
                        {
                            this.props.rows.map((row, i) => {
                                return (
                                    <tr key={i}>
                                        {
                                            (() => {
                                                let colorClass: string;
                                                if (i % 2 === 0) {
                                                    colorClass = "table-body-item";
                                                } else {
                                                    colorClass = "table-light-body-item";
                                                }
                                                return row.map((x, j) => {
                                                    return (
                                                        <td key={`${i}:${j}`}
                                                            className={`${colorClass} table-body-item`}>
                                                            {this.props.htmlify(x)}
                                                        </td>
                                                    );
                                                });
                                            })()
                                        }
                                    </tr>
                                );
                            })
                        }
                    </tbody>
                </table>
            </div>
        )
    }
}
