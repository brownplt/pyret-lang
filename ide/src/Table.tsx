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
        const maxRowsPerPage = 5;
        const showOptions = this.props.rows.length > maxRowsPerPage;
        const defaultPageSize = showOptions ? maxRowsPerPage : this.props.rows.length;
        return (
            <ReactTable data={data}
                        columns={columns}
                        showPagination={showOptions}
                        pageSize={defaultPageSize}
                        showPageSizeOptions={false}
                        filterable={showOptions}/>
        );
    }
}
