import React from 'react';
import ReactTable from 'react-table';
import 'react-table/react-table.css';

type TableWidgetProps = {
    htmlify: (x: any) => any;
    headers: string[];
    rows: any[][];
};
type TableWidgetState = {};

export class TableWidget extends React.Component<TableWidgetProps, TableWidgetState> {
    render() {
        const data = this.props.rows;
        const columns = this.props.headers.map(
            (header: string, index: number) => {
                return {
                    id: header,
                    Header: header,
                    accessor: (row: any) => {
                        return this.props.htmlify(row[index]);
                    }
                };
            });
        return (
            <ReactTable data={data} columns={columns}/>
        );
    }
}
