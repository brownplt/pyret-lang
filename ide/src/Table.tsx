import React from 'react';
import ReactTable from 'react-table';
import 'react-table/react-table.css';
import CopyToClipboard from 'react-copy-to-clipboard';

type TableWidgetProps = {
  htmlify: (x: any) => any;
  headers: string[];
  rows: any[][];
};

type TableWidgetState = {};

export default class TableWidget extends React.Component<TableWidgetProps, TableWidgetState> {
  render() {
    const { rows } = this.props;
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
      <div className="table-container">
        <CopyToClipboard text={rows.map((d) => d.join("\t")).join("\n")}>
          <div className="table-copy">
      &#128203;
          </div>
        </CopyToClipboard>
        <ReactTable data={rows}
          columns={columns}
          showPagination={showOptions}
          pageSize={defaultPageSize}
          showPageSizeOptions={false}
          filterable={showOptions}/>
      </div>
    );
  }
}
