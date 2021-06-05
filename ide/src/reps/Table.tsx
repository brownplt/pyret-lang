/* A widget for displaying tables. Table documentation:
   https://www.pyret.org/docs/latest/tables.html.

   Tables are created in RenderedValue.tsx */

import React from 'react';
import ReactTable from 'react-table';
import 'react-table/react-table.css';
import CopyToClipboard from 'react-copy-to-clipboard';

type TableWidgetProps = {
  headers: string[];
  rows: any[][];
  RenderedValue: React.ElementType;
};

export default function TableWidget({ headers, rows, RenderedValue }: TableWidgetProps) {
  const columns = headers.map(
    (header: string, index: number) => ({
      id: header,
      Header: header,
      accessor: (row: any) => <RenderedValue value={row[index]} />,
    }),
  );
  const maxRowsPerPage = 5;
  const showOptions = rows.length > maxRowsPerPage;
  const defaultPageSize = showOptions ? maxRowsPerPage : rows.length;
  return (
    <div className="table-container">
      <CopyToClipboard text={rows.map((d) => d.join('\t')).join('\n')}>
        <div className="table-copy">
          &#128203;
        </div>
      </CopyToClipboard>
      <ReactTable
        data={rows}
        columns={columns}
        showPagination={showOptions}
        pageSize={defaultPageSize}
        showPageSizeOptions={false}
        filterable={showOptions}
      />
    </div>
  );
}
