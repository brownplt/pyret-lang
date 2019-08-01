// @ts-ignore
const PyretOption = require("./option.arr.js");
const List = require("./list.arr.js");

interface Table {
  'all-columns': () => any[][],
  'all-rows': () => Row[],
  'column-names': () => string[],
  'column-n': (index: number) => any[],
  'row-n': (index: number) => Row,
  'add-row': (row: Row) => Table,
  'add-column': (columnName: string, newVals: any[]) => Table,
  'build-column': (columName: string,
                   computeNewVal: (row: Row) => any) => Table,
  '_headers': string[],
  '_rows': any[],
  'length': () => number,
  'row': (...columns: any[]) => Row,
  '$brand': string
}

interface Row {
  '_headers': string[],
  '_elements': any[],
  'get-column-names': () => string[],
  'get-value': (columnName: string) => any,
  'get': (columnName: string) => any;
}

function getColumnNames(row: Row): string[] {
  return List.list.make(row._headers);
}

function getValue(row: Row, columnName: string): any {
  const columnIndex: number = row._headers.indexOf(columnName);
  if (columnIndex === -1) {
    throw "get-value: column does not exist";
  }
  return row._elements[columnIndex];
}

function rowGet(row: Row, columnName: string): any {
  const columnIndex: number = row._headers.indexOf(columnName);

  if (columnIndex === -1) {
    // @ts-ignore
    return PyretOption.none;
  } else {
    // @ts-ignore
    return PyretOption.some(row._elements[columnIndex]);
  }
}

function rawRow(elements: [string, any][]): Row {
  const headers: string[] = [];
  const rowElements: any[] = [];
  for (let i = 0; i < elements.length; i++) {
    const [h, e] = elements[i];
    headers.push(h);
    rowElements.push(e);
  }

  const result = {
    '_headers': headers,
    '_elements': rowElements,
    'get-column-names': () => getColumnNames(result),
    'get-value': (columnName: string) => getValue(result, columnName),
    'get': (columnName: string) => rowGet(result, columnName)
  };

  return result;
}

function zipWith<X, Y, Z>(f: (arg0: X, arg1: Y) => Z, xs: X[], ys: Y[]): Z[] {
  if (xs.length !== ys.length) {
    throw new Error("can't zipWith arrays of different lengths");
  }

  const result: Z[] = [];

  for (let i = 0; i < xs.length; i++) {
    result.push(f(xs[i], ys[i]));
  }

  return result;
}

function zip<X, Y>(xs: X[], ys: Y[]): [X, Y][] {
  return zipWith((x, y) => [x, y], xs, ys);
}

function _row(table: any, ...columns: any[]): Row {
  const elements: [string, any[]][] = zip(table._headers, columns);
  return rawRow(elements);
}

function _buildColumn(table: any,
                      columnName: string,
                      computeNewVal: (arg0: Row) => any): Table {
  const headers = _deepCopy(table._headers);
  const newHeaders = headers.slice();
  newHeaders.push(columnName);
  const rows = _deepCopy(table._rows);

  rows.forEach((row: any[]) => {
    row.push(computeNewVal(rawRow(zip(headers, row))));
  });

  return _makeTable(newHeaders, rows);
}

function _addColumn(table: any, columnName: string, newVals: any[]): Table {
  const headers = _deepCopy(table._headers);

  for (let i = 0; i < headers.length; i++) {
    if (headers[i] === columnName) {
      throw new Error("duplicate column name: " + columnName);
    }
  }

  const rows = _deepCopy(table._rows);

  if (rows.length !== newVals.length) {
    throw new Error("length of new column is different than the length of the table");
  }

  for (let i = 0; i < rows.length; i++) {
    rows[i].push(newVals[i]);
  }

  headers.push(columnName);

  return _makeTable(headers, rows);
}

function _addRow(table: any, row: Row): Table {
  const tableHeaders = _deepCopy(table._headers);
  const rowHeaders = row._headers;

  if (!_arraysEqual(tableHeaders, rowHeaders)) {
    throw new Error("table does not have the same column names as the new row");
  }

  const tableRows = _deepCopy(table._rows);
  tableRows.push(row._elements);

  return _makeTable(tableHeaders, tableRows);
}

function _rowN(table: any, index: number): Row {
  if (index >= table._rows.length) {
    throw new Error("index " + index + " out of bounds in table rows");
  }

  return rawRow(zip(table._headers, table._rows[index]));
}

function _columnN(table: any, index: number): any[] {
  if (index >= table._headers.length) {
    throw new Error("index " + index + " out of bounds in table columns");
  }

  return List.list.make(table._rows.map((row) => row[index]));
}

function _columnNames(table: any): string[] {
  return List.list.make(table._headers);
}

function _allRows(table: any): Row[] {
  return List.list.make(table._rows
                        .map((row) =>
                             rawRow(zip(table._headers, row))));
}

function _allColumns(table: any): any[][] {
  const rows = table._rows;
  const headers = table._headers;
  const columns = headers.map((_) => []);

  for (let i = 0; i < columns.length; i++) {
    for (let j = 0; j < rows.length; j++) {
      columns[i].push(rows[j][i]);
    }
  }

  return columns;
}

function _makeTable(headers: string[], rows: any[][]): Table {
  var headerIndex = {};

  for (var i = 0; i < headers.length; i++) {
    headerIndex["column:" + headers[i]] = i;
  }

  const table = {
    'all-columns': () => _allColumns(table),
    'all-rows': () => _allRows(table),
    'column-names': () => _columnNames(table),
    'column-n': (index) => _columnN(table, index),
    'row-n': (index) => _rowN(table, index),
    'add-row': (row) => _addRow(table, row),
    'add-column': (columnName, newVals) => _addColumn(table, columnName, newVals),
    'build-column': (columName, computeNewVal) => _buildColumn(table, columName, computeNewVal),
    '_headers': headers,
    '_rows': rows,
    'length': function() { return rows.length; },
    '_headerIndex': headerIndex,
    'row': (...columns) => _row(table, ...columns),
    '$brand': '$table'
  };

  return table;
}

// Changes the elements of a table in the specified column using the given function
function _transformColumnMutable(table: Table,
                                 colname: string,
                                 func: (element: any) => any): void {
  if(!hasColumn(table, colname)) {
    throw "transformColumnMutable: tried changing the column " + colname + " but it doesn't exist";
  }

  // index of the column to change
  var i = table["_headerIndex"]['column:' + colname];

  table._rows.forEach((row) =>
    row[i] = func(row[i])
  );
}

// Creates a new table and mutates the specified columns with the given functions
function _tableTransform(table: Table,
                         colnames: string[],
                         updates: ((element: any) => any)[]) {
  var newHeaders = _deepCopy(table._headers);
  var newRows = _deepCopy(table._rows);
  var newTable = _makeTable(newHeaders, newRows);

  for (let i = 0; i < colnames.length; i++) {
    _transformColumnMutable(newTable, colnames[i], updates[i]);
  }

  return newTable;
}

// transformColumn :: (Table, String, Function) -> Table
// Creates a new table that mutates the specified column for the given function
function transformColumn(table: Table, colname: string, update: (element: any) => any) {
  var newHeaders = _deepCopy(table._headers);
  var newRows = _deepCopy(table._rows);
  var newTable = _makeTable(newHeaders, newRows);
  _transformColumnMutable(newTable, colname, update);
  return newTable;
}

// returns a deep copy of (nested) arrays
function _deepCopy(arr: any): any {
  var i, copy;

  if ( Array.isArray(arr) ) {
    copy = arr.slice(0);
    for ( i = 0; i < copy.length; i++ ) {
      copy[i] = _deepCopy(copy[i]);
    }
    return copy;
  } else {
    return arr;
  }
}

// _tableFilter :: Table -> (Array -> Boolean) -> Table
// Creates a new Table which contains the rows from table that satisfy predicate.
function _tableFilter(table: Table, predicate: (row: any[]) => boolean): Table {
  return _makeTable(table._headers, table._rows.filter(predicate));
}

// _tableGetColumnIndex :: Table -> String -> Integer
// Returns the index of column_name, or throws an error if column_name is not a
// column in table.
function _tableGetColumnIndex(table: Table, column_name: string): number {
  const headers = table._headers;

  for (let index = 0; index < headers.length; index++) {
    if (headers[index] === column_name) {
      return index;
    }
  }

  throw "not a valid column";
}

interface Ordering {
  'column': string,
  'direction': 'ascending' | 'descending'
}

// Creates a Table that is like table, except that its rows are sorted according
// to columnOrders. An element in columnOrders specifies a column name and an
// ordering, either ascending or descending.
function _tableOrder(table: Table, columnOrders: Ordering[]): Table {
  const headers = table._headers;
  const rows = table._rows;

  function ordering(a: any, b: any): number {
    for (let i = 0; i < columnOrders.length; i++) {
      const columnOrder = columnOrders[i];
      const name = columnOrder["column"];
      const order = columnOrder["direction"];
      const index = _tableGetColumnIndex(table, name);
      const elemA = a[index];
      const elemB = b[index];

      if (order === "ascending") {
        if (elemA < elemB) {
          return -1;
        } else if (elemA > elemB) {
          return 1;
        }
      } else if (order === "descending") {
        if (elemA < elemB) {
          return 1;
        } else if (elemA > elemB) {
          return -1;
        }
      }
    }

    return 0;
  }

  // Array.prototype.sort() mutates the array it sorts, so we need to create a
  // copy with Array.prototype.slice() first.
  let sortedRows = rows.slice().sort(ordering);

  return _makeTable(headers, sortedRows);
}

// _selectColumns :: (Table, Array<String>) -> Table
function _selectColumns(table: Table, colnames: string[]): Table {
  //var colnamesList = ffi.toArray(colnames);
  // This line of code below relies on anchor built-in lists being js arrays
  var colnamesList = colnames;
  if(colnamesList.length === -1) {
    throw "zero-columns";
  }
  for(var i = 0; i < colnamesList.length; i += 1) {
    if(!hasColumn(table, colnamesList[i])) {
      throw "no-such-column";
   }
  }
  var newRows = [];
  for(var i = 0; i < table['_rows'].length; i += 1) {
    console.log(i);
    newRows[i] = [];
    for(var j = 0; j < colnamesList.length; j += 1) {
      var colIndex = _tableGetColumnIndex(table, colnamesList[j]);
      newRows[i].push(table['_rows'][i][colIndex]);
    }
  }
  return _makeTable(colnamesList, newRows);
}

function _tableExtractColumn(table: Table, columnName: string): any[] {
  const index = _tableGetColumnIndex(table, columnName);
  const rows = table._rows;
  const extracted = List["empty-list"]();

  for (let i = 0; i < rows.length; i++) {
    const element = rows[i][index];
    List.push(extracted, element);
  }

  return extracted;
}

// TableExtensions are generated by the `extend` syntax.
type TableExtension = MappingExtension<any> | ReducingExtension<any, any, any>;

// An extension of the form `new-column-name: expression`, i.e.,
//
// extend my-table using a:
//   b: a / 2 # a mapping extension
// end
interface MappingExtension<OutVal> {
  type: 'map',
  reduce: (row: number) => OutVal,
  extending: string
}

// An extension of the form `new-column-name: reducer-expr of old-column-name`, i.e.,
//
// extend my-table using a:
//   b : running-sum of a
// end
interface ReducingExtension<Acc, InVal, OutVal> {
  type: 'reduce',
  one: (element: InVal) => [Acc, OutVal],
  reduce: (acc: Acc, element: InVal) => [Acc, OutVal],
  using: string,
  extending: string
}

const runningSum = {
  one: (element) => [element, element],
  reduce: (acc, element) => {
    const sum = acc + element;
    return [sum, sum];
  },
};

function isReducingExtension(x: TableExtension): x is ReducingExtension<any, any, any> {
  return (x as ReducingExtension<any, any, any>).type === 'reduce';
}

function isMappingExtension(x: TableExtension): x is MappingExtension<any> {
  return (x as MappingExtension<any>).type === 'map';
}

// Creates a new Table that is like table, except that it has one or more new columns,
// as specified by the supplied TableExtensions.
function _tableReduce(table: Table, extensions: Array<TableExtension>): Table {
  const headers = table['_headers'];
  const rows = table['_rows'];
  const extendedColumns =
    extensions.map((extension: TableExtension) => extension.extending);
  const newHeaders = headers.concat(extendedColumns);
  const newRows = rows.slice();
  const newTable = _makeTable(newHeaders, newRows);

  if (rows.length === 0) {
    return newTable;
  }

  let accumulators = [];

  for (let i = 0; i < extensions.length; i++) {
    const extension: TableExtension = extensions[i];
    if (isMappingExtension(extension)) {
      const mapping: MappingExtension<any> = extension;
      const row = newRows[0];
      const extending = mapping.extending;
      const extendingIndex = _tableGetColumnIndex(newTable, extending);
      row[extendingIndex] = mapping.reduce(0);
    } else if (isReducingExtension(extension)) {
      const reducing: ReducingExtension<any, any, any> = extension;
      const row = newRows[0];
      const one = reducing.one;
      const using = reducing.using;
      const extending = reducing.extending;
      const usingIndex = _tableGetColumnIndex(newTable, using);
      const extendingIndex = _tableGetColumnIndex(newTable, extending);
      const [acc, outVal] = one(row[usingIndex]);
      accumulators[i] = acc;
      row[extendingIndex] = outVal;
    } else {
      throw "_tableReduce: extension is not a TableExtension"
    }
  }

  for (let i = 1; i < newRows.length; i++) {
    for (let j = 0; j < extensions.length; j++) {
      const extension: TableExtension = extensions[j];
      if (isMappingExtension(extension)) {
        const mapping: MappingExtension<any> = extension;
        const row = newRows[i];
        const extending = mapping.extending;
        const extendingIndex = _tableGetColumnIndex(newTable, extending);
        row[extendingIndex] = mapping.reduce(i);
      } else if (isReducingExtension(extension)) {
        const reducing: ReducingExtension<any, any, any> = extension;
        const row = newRows[i];
        const reduce = reducing.reduce;
        const using = reducing.using;
        const extending = reducing.extending;
        const usingIndex = _tableGetColumnIndex(newTable, using);
        const extendingIndex = _tableGetColumnIndex(newTable, extending);
        const [acc, outVal] = reduce(accumulators[j], row[usingIndex]);
        accumulators[j] = acc;
        row[extendingIndex] = outVal;
      } else {
        throw "_tableReduce: extension is not a TableExtension"
      }
    }
  }

  return _makeTable(newHeaders, newRows);
}

function hasColumn(table: Table, column_name: string): boolean {
  return table._headers.includes(column_name);
}

function getColumn(table: Table, column_name: string): any[] {
  // Raise error if table lacks column
  if ( !hasColumn(table, column_name) ) {
    throw "no such column";
  }

  var column_index;
  Object.keys(table._headers).forEach(function(i) {
    if(table._headers[i] == column_name) { column_index = i; }
  });
  return table._rows.map(function(row){return row[column_index];});
}

function _length(table: Table): number {
  return table._rows.length;
}

// creates a new table with a column renamed
function renameColumn(table: Table, old_name: string, new_name: string): Table {
  // check if old_name exists
  if ( !hasColumn(table, old_name) ) {
    throw "no such column";
  }
  var newHeaders = _deepCopy(table._headers);
  var newRows = _deepCopy(table._rows);
  var colIndex = _tableGetColumnIndex(table, old_name);
  newHeaders[colIndex] = new_name;
  var newTable = _makeTable(newHeaders, newRows);
  return newTable;
}

// orders column in ascending order
function increasingBy(table: Table, colname: string): Table {
  // check if colname exists
  if ( !hasColumn(table, colname) ) {
    throw new Error("no such column");
  }

  var newHeaders = _deepCopy(table._headers);
  var newRows = _deepCopy(table._rows);
  var colIndex = _tableGetColumnIndex(table, colname);

  function ordering(a: any, b: any): number {
    const elemA = a[colIndex];
    const elemB = b[colIndex];
    if (elemA < elemB) {
      return -1;
    } else if (elemA > elemB) {
      return 1;
    }
    return 0;
  }

  var sortedRows = newRows.slice().sort(ordering);

  var newTable = _makeTable(newHeaders, sortedRows);
  return newTable;
}

// orders column in descending order
function decreasingBy(table: Table, colname: string): Table {
  // check if colname exists
  if ( !hasColumn(table, colname) ) {
    throw new Error("no such column");
  }

  var newHeaders = _deepCopy(table._headers);
  var newRows = _deepCopy(table._rows);
  var colIndex = _tableGetColumnIndex(table, colname);

  function ordering(a: any, b: any): number {
    const elemA = a[colIndex];
    const elemB = b[colIndex];
    if (elemA < elemB) {
      return 1;
    } else if (elemA > elemB) {
      return -1;
    }
    return 0;
  }

  var sortedRows = newRows.slice().sort(ordering);

  var newTable = _makeTable(newHeaders, sortedRows);
  return newTable;
}

// orders a column ascending or descending depending on the boolean
function orderBy(table: Table, colname: string, asc: boolean): Table {
  if (asc) { return increasingBy(table, colname); }
  else { return decreasingBy(table, colname); }
}

function orderByColumns(table: Table, cols: [string, boolean][]): Table {
  const headers = table._headers;
  const rows = table._rows;

  function ordering(a: any, b: any): number {
    for (let i = 0; i < cols.length; i++) {
      const columnOrder = cols[i];
      const name = columnOrder[0];
      const order = columnOrder[1];
      const index = _tableGetColumnIndex(table, name);
      const elemA = a[index];
      const elemB = b[index];

      if (order === true) {
        if (elemA < elemB) {
          return -1;
        } else if (elemA > elemB) {
          return 1;
        }
      } else if (order === false) {
        if (elemA < elemB) {
          return 1;
        } else if (elemA > elemB) {
          return -1;
        }
      }
    }

    return 0;
  }

  // Array.prototype.sort() mutates the array it sorts, so we need to create a
  // copy with Array.prototype.slice() first.
  let sortedRows = rows.slice().sort(ordering);

  return _makeTable(headers, sortedRows);
}

// returns an empty Table with the same column headers
function empty(table: Table): Table {
  var newHeaders = _deepCopy(table._headers);
  var newTable = _makeTable(newHeaders, []);
  return newTable;
}

// returns a new table without the specified column
function drop(table: Table, colname: string): Table {
  // check if colname exists
  if ( !hasColumn(table, colname) ) {
    throw "no such column";
  }
  var newHeaders = _deepCopy(table._headers);
  var newRows = _deepCopy(table._rows);
  var colIndex = _tableGetColumnIndex(table, colname);
  newHeaders.splice(colIndex, 1);
  for ( let i = 0; i < newRows.length; i++ ) {
    newRows[i].splice(colIndex, 1);
  }
  var newTable = _makeTable(newHeaders, newRows);
  return newTable;
}

// returns a new table with elements of both tables
function stack(table: Table, bot: Table): Table {
  var tableHeaders = _deepCopy(table._headers);
  var headersToSort = _deepCopy(table._headers);
  var botHeaders = _deepCopy(bot._headers);
  if ( !(_arraysEqual(headersToSort.sort(), botHeaders.sort())) ) {
    throw new Error("headers do not match");
  }

  var newRows = _deepCopy(table._rows);

  for ( let i = 0; i < bot._rows.length; i++ ) {
    newRows.push([]);
    for ( let j = 0; j < tableHeaders.length; j++ ) {
      newRows[ newRows.length - 1 ]
        .push(bot._rows[i][_tableGetColumnIndex(bot, tableHeaders[j])]);
    }
  }

  var newTable = _makeTable(tableHeaders, newRows);
  return newTable;
}

function _arraysEqual(xs: any[], ys: any[]): boolean {
  if (xs === ys) {
    return true;
  }

  if (xs.length !== ys.length) {
    return false;
  }

  for (let i = 0; i < xs.length; i++) {
    if (xs[i] !== ys[i]) {
      return false;
    }
  }

  return true;
}

function tableFromRows(rows: Row[]): Table {
  if (rows.length === 0) {
    throw "table-from-rows: expected one or more rows";
  }

  const headers: string[][] = rows.map(row => row._headers);

  for (let i = 0; i < headers.length; i++) {
    if (!_arraysEqual(headers[i], headers[0])) {
      throw "table-from-rows: row name mismatch";
    }
  }

  const elements: any[][] = rows.map(row => row._elements);

  return _makeTable(headers[0], elements);
}

type Column = [string, any[]];

function tableFromColumns(columns: Column[]): Table {
  if (columns.length === 0) {
    throw new Error("expected at least one column");
  }

  const headers = columns.map(column => column[0]);
  const sortedHeaders = headers.slice().sort();

  for (let i = 0; i < sortedHeaders.length - 1; i++) {
    if (sortedHeaders[i] === sortedHeaders[i + 1]) {
      throw new Error("duplicate header: " + sortedHeaders[i]);
    }
  }

  const rowLength = columns[0][1].length

  for (let i = 0; i < columns.length; i++) {
    if (columns[i][1].length !== rowLength) {
      throw new Error("columns must have the same number of elements");
    }
  }

  const rows = columns[0][1].map(() => []);

  for (let i = 0; i < columns.length; i++) {
    for (let j = 0; j < columns[i][1].length; j++) {
      rows[j].push(columns[i][1][j]);
    }
  }

  return _makeTable(headers, rows);
}

function tableFromColumn(columnName: string, values: any[]): Table {
  const col: Column = [columnName, values];
  return tableFromColumns([col]);
}

module.exports = {
  'table-from-column': tableFromColumn,
  'table-from-columns': {
    'make': tableFromColumns
  },
  'table-from-rows': {
    'make': tableFromRows
  },
  'raw-row': {
    'make': rawRow
  },
  '_makeTable': _makeTable,
  '_tableFilter': _tableFilter,
  '_tableGetColumnIndex': _tableGetColumnIndex,
  '_tableOrder': _tableOrder,
  '_tableExtractColumn': _tableExtractColumn,
  '_tableTransform': _tableTransform,
  '_selectColumns': _selectColumns,
  'decreasing-by': decreasingBy,
  'drop': drop,
  'empty': empty,
  'get-column': getColumn,
  'increasing-by': increasingBy,
  '_length': _length,
  'order-by': orderBy,
  'order-by-columns': orderByColumns,
  'rename-column': renameColumn,
  'select-columns': _selectColumns,
  'stack': stack,
  'transform-column': transformColumn,
  '_tableReduce': _tableReduce,
  'running-sum': runningSum
};
