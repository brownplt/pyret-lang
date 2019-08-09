// @ts-ignore
const PyretOption = require("./option.arr.js");
const List = require("./list.arr.js");
const parse = require("csv-parse/lib/sync")
const fs = require("fs");

interface Table {
  'add-column': (columnName: string, newVals: any[]) => Table,
  'add-row': (row: Row) => Table,
  'all-columns': () => any[][],
  'all-rows': () => Row[],
  'build-column': (columnName: string,
                   computeNewVal: (row: Row) => any) => Table,
  'column-n': (index: number) => any[],
  'column-names': () => string[],
  'decreasing-by': (columnName: string) => Table,
  'drop': (columnName: string) => Table,
  'empty': () => Table,
  'filter': (predicate: (row: Row) => boolean) => Table,
  'filter-by': (columnName: string, predicate: (col: any) => Boolean) => Table,
  'get-column': (columnName: string) => any[],
  'increasing-by': (columnName: string) => Table,
  'length': () => number,
  'order-by': (columnName: string, asc: boolean) => Table,
  'order-by-columns': (columns: any[]) => Table,
  'rename-column': (oldName: string, newName: string) => Table,
  'row': (...columns: any[]) => Row,
  'row-n': (index: number) => Row,
  'select-columns': (columnNames: string[]) => Table,
  'stack': (bot: Table) => Table,
  'transform-column': (columnName: string, update: (funcIn: any) => any) => Table,
  '_headerIndex': any,
  '_headers': string[],
  '_rows': any[],
  '$brand': string
}

function _primitiveEqual(a1: any, a2: any): boolean {
  if (a1 === a2) {
    return true;
  }

  if (a1 == null || a2 == null) {
    return false;
  }

  if (Array.isArray(a1) && Array.isArray(a2)) {
    return _primitiveArraysEqual(a1, a2);
  }

  if (a1.$brand === '$table' && a2.$brand === '$table') {
    return _primitiveTablesEqual(a1, a2);
  }

  if (a1.$brand === '$row' && a2.$brand === '$row') {
    return _primitiveRowsEqual(a1, a2);
  }

  return false;
}

function _primitiveRowsEqual(a1: Row, a2: Row): boolean {
  if (a1.$brand !== '$row') {
    throw new Error("expected an object with the field '$brand': '$row',"
                    + " but received " + JSON.stringify(a1)+ " instead");
  }

  if (a2.$brand !== '$row') {
    throw new Error("expected an object with the field '$brand': '$row',"
                    + " but received " + JSON.stringify(a2) + " instead");
  }

  if (!_primitiveEqual(a1._headers, a2._headers)) {
    return false;
  }

  if (!_primitiveEqual(a1._elements, a2._elements)) {
    return false;
  }

  return true;
}

// Returns true if a1 and a2 contain identical primitive values.
function _primitiveArraysEqual(a1: any, a2: any): boolean {
  if (a1 === a2) {
    return true;
  }

  if (!Array.isArray(a1)) {
    throw new Error("found non-array object: " + a1);
  }


  if (!Array.isArray(a2)) {
    throw new Error("found non-array object: " + a2);
  }

  if (a1.length !== a2.length) {
    return false;
  }

  for (let i = 0; i < a1.length; i++) {
    if (!_primitiveEqual(a1[i], a2[i])) {
      return false;
    }
  }

  return true;
}

function _primitiveTablesEqual(t1: Table, t2: Table): boolean {
  if (t1.$brand !== '$table') {
    throw new Error("expected an object with the field '$brand': '$table',"
                    + " but received " + JSON.stringify(t1) + " instead");
  }

  if (t2.$brand !== '$table') {
    throw new Error("expected an object with the field '$brand': '$table',"
                    + " but received " + JSON.stringify(t2) + " instead");
  }

  const t1_headers = t1._headers;
  const t2_headers = t2._headers;

  if (!_primitiveArraysEqual(t1_headers, t2_headers)) {
    return false;
  }

  const t1_rows = t1._rows;
  const t2_rows = t2._rows;

  if (t1_rows.length !== t2_rows.length) {
    return false;
  }

  for (let i = 0; i < t1_rows.length; i++) {
    if (!_primitiveEqual(t1_rows[i], t2_rows[i])) {
      return false;
    }
  }

  return true;
}

interface Row {
  '_headers': string[],
  '_elements': any[],
  'get-column-names': () => string[],
  'get-value': (columnName: string) => any,
  'get': (columnName: string) => any,
  '$brand': string
}

function getColumnNames(row: Row): string[] {
  return List.list.make(row._headers);
}

function getValue(row: Row, columnName: string): any {
  const columnIndex: number = row._headers.indexOf(columnName);
  if (columnIndex === -1) {
    throw new Error("get-value: column does not exist");
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
    'get': (columnName: string) => rowGet(result, columnName),
    '$brand': '$row'
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
  if (table._headers.indexOf(columnName) !== -1) {
    throw new Error("duplicate column name: " + columnName);
  }
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
  const tableHeaders = table._headers;
  const rowHeaders = row._headers;

  if (!_primitiveArraysEqual(tableHeaders, rowHeaders)) {
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
    'add-column': (columnName, newVals) => _addColumn(table, columnName, newVals),
    'add-row': (row) => _addRow(table, row),
    'all-columns': () => _allColumns(table),
    'all-rows': () => _allRows(table),
    'build-column': (columnName, computeNewVal) => _buildColumn(table, columnName, computeNewVal),
    'column-n': (index) => _columnN(table, index),
    'column-names': () => _columnNames(table),
    'decreasing-by': (columnName) => decreasingBy(table, columnName),
    'drop': (columnName) => drop(table, columnName),
    'empty': () => empty(table),
    'filter': (predicate) => filter(table, predicate),
    'filter-by': (columnName, predicate) => filterBy(table, columnName, predicate),
    'get-column': (columnName) => getColumn(table, columnName),
    'increasing-by': (columnName) => increasingBy(table, columnName),
    'length': function() { return rows.length; },
    'order-by': (columnName, asc) => orderBy(table, columnName, asc),
    'order-by-columns': (columns) => orderByColumns(table, columns),
    'rename-column': (oldName, newName) => renameColumn(table, oldName, newName),
    'row': (...columns) => _row(table, ...columns),
    'row-n': (index) => _rowN(table, index),
    'select-columns': (columnNames) => _selectColumns(table, columnNames),
    'stack': (bot) => stack(table, bot),
    'transform-column': (columnName, update) => transformColumn(table, columnName, update),
    '_headerIndex': headerIndex,
    '_headers': headers,
    '_rows': rows,
    '$brand': '$table'
  };

  return table;
}

type TableSkeleton = { headers: string[], rows: any[][] };

function _tableSkeletonChangeHeaders(
  skeleton: TableSkeleton,
  newHeaders: string[]): TableSkeleton {
  if (newHeaders.length !== skeleton.headers.length) {
    throw new Error("Expected " + skeleton.headers.length + " headers, but got "
                    + newHeaders.length + " in " + newHeaders);
  }

  return { headers: newHeaders, rows: skeleton.rows };
}

function _makeTableSkeletonFromCSVString(s: string): TableSkeleton {
  const headers = [];

  const csv = parse(s, {
    columns: (header: string[]) => {
      return header.map((column: string) => {
        headers.push(column);
        return column;
      });
    }
  })

  const rows: any[][] = csv.map((row: object) => {
    const result = [];

    for (let i = 0; i < headers.length; i++) {
      result.push(row[headers[i]]);
    }

    return result;
  });

  return { headers: headers, rows: rows };
}

function _makeTableSkeletonFromCSVFile(path: string): TableSkeleton {
  const contents = fs.readFileSync(path, { encoding: "utf-8" });
  return _makeTableSkeletonFromCSVString(contents);
}

function _makeTableFromTableSkeleton(s: TableSkeleton): Table {
  return _makeTable(s.headers, s.rows);
}

function _makeTableFromCSVString(s: string): Table {
  const skeleton = _makeTableSkeletonFromCSVString(s);
  return _makeTableFromTableSkeleton(skeleton);
}

function _makeTableFromCSVFile(path: string): Table {
  const contents = fs.readFileSync(path, { encoding: "utf-8" });
  return _makeTableFromCSVString(contents);
}

// Changes the elements of a table in the specified column using the given function
function _transformColumnMutable(table: Table,
                                 columnName: string,
                                 func: (element: any) => any): void {
  if(!hasColumn(table, columnName)) {
    throw new Error("transformColumnMutable: tried changing the column " + columnName + " but it doesn't exist");
  }

  // index of the column to change
  var i = table["_headerIndex"]['column:' + columnName];

  table._rows.forEach((row) =>
    row[i] = func(row[i])
  );
}

// Creates a new table and mutates the specified columns with the given functions
function _tableTransform(table: Table,
                         columnNames: string[],
                         updates: ((element: any) => any)[]) {
  var headers = table._headers;
  var newRows = _deepCopy(table._rows);
  var newTable = _makeTable(headers, newRows);

  for (let i = 0; i < columnNames.length; i++) {
    _transformColumnMutable(newTable, columnNames[i], updates[i]);
  }

  return newTable;
}

// transformColumn :: (Table, String, Function) -> Table
// Creates a new table that mutates the specified column for the given function
function transformColumn(table: Table, columnName: string, update: (element: any) => any) {
  var headers = table._headers;
  var newRows = _deepCopy(table._rows);
  var newTable = _makeTable(headers, newRows);
  _transformColumnMutable(newTable, columnName, update);
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
  var headers = table._headers;
  var rows = table._rows;
  return _makeTable(headers, rows.filter(predicate));
}

// filter :: (Table, (Row -> Boolean)) -> Table
// creates a new table containing only the rows for which the predicate
// returned true
function filter(table, predicate) {
  var headers = table._headers;
  var rows = table._rows;
  var newRows = [];

  for (let i = 0; i < rows.length; i++) {
    if (predicate(_rowN(table, i))) {
      newRows.push(rows[i]);
    }
  }

  return _makeTable(headers, newRows);
}

// filter-by :: (Table, String, (Col -> Boolean)) -> Table
// creates a new table containing only the rows for which the predicate
// returned true for that column
function filterBy(table, columnName, predicate) {
  var headers = table._headers;
  var newRows = [];
  var column = getColumn(table, columnName);

  for ( let i = 0; i < column.length; i++ ) {
    if ( predicate(column[i]) ) {
      newRows.push(table._rows[i]);
    }
  }

  return _makeTable(headers, newRows);
}

// _tableGetColumnIndex :: Table -> String -> Integer
// Returns the index of columnName, or throws an error if columnName is not a
// column in table.
function _tableGetColumnIndex(table: Table, columnName: string): number {
  const headers = table._headers;

  for (let index = 0; index < headers.length; index++) {
    if (headers[index] === columnName) {
      return index;
    }
  }

  throw new Error("Column " + columnName + " is not valid");
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
function _selectColumns(table: Table, columnNames: string[]): Table {
  //var colnamesList = ffi.toArray(columnNames);
  // This line of code below relies on anchor built-in lists being js arrays
  var colnamesList = columnNames;
  if(colnamesList.length === -1) {
    throw new Error("zero-columns");
  }

  for(var i = 0; i < colnamesList.length; i += 1) {
    if(!hasColumn(table, colnamesList[i])) {
      throw new Error("no-such-column");
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
  // throws an error if columnName is not in table
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

function hasColumn(table: Table, columnName: string): boolean {
  return table._headers.includes(columnName);
}

function getColumn(table: Table, columnName: string): any[] {
  // Raise error if table lacks column
  if ( !hasColumn(table, columnName) ) {
    throw new Error("no such column: " + columnName);
  }

  var columnIndex;
  Object.keys(table._headers).forEach(function(i) {
    if(table._headers[i] == columnName) { columnIndex = i; }
  });
  return table._rows.map(function(row){return row[columnIndex];});
}

function _length(table: Table): number {
  return table._rows.length;
}

// creates a new table with a column renamed
function renameColumn(table: Table, oldName: string, newName: string): Table {
  // check if oldName exists
  if ( !hasColumn(table, oldName) ) {
    throw new Error("no such column to change: " + oldName);
  }

  var newHeaders = _deepCopy(table._headers);
  var rows = table._rows;
  var colIndex = _tableGetColumnIndex(table, oldName);
  newHeaders[colIndex] = newName;
  var newTable = _makeTable(newHeaders, rows);
  return newTable;
}

// orders column in ascending order
function increasingBy(table: Table, columnName: string): Table {
  // check if columnName exists
  if ( !hasColumn(table, columnName) ) {
    throw new Error("no such column");
  }

  var headers = table._headers;
  var newRows = _deepCopy(table._rows);
  var colIndex = _tableGetColumnIndex(table, columnName);

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

  var newTable = _makeTable(headers, sortedRows);
  return newTable;
}

// orders column in descending order
function decreasingBy(table: Table, columnName: string): Table {
  // check if columnName exists
  if ( !hasColumn(table, columnName) ) {
    throw new Error("no such column");
  }

  var newHeaders = table._headers;
  var newRows = _deepCopy(table._rows);
  var colIndex = _tableGetColumnIndex(table, columnName);

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
function orderBy(table: Table, columnName: string, asc: boolean): Table {
  if (asc) { return increasingBy(table, columnName); }
  else { return decreasingBy(table, columnName); }
}

function orderByColumns(table: Table, columns: [string, boolean][]): Table {
  const headers = table._headers;
  const rows = table._rows;

  function ordering(a: any, b: any): number {
    for (let i = 0; i < columns.length; i++) {
      const columnOrder = columns[i];
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
  var headers = table._headers;
  var newTable = _makeTable(headers, []);
  return newTable;
}

// returns a new table without the specified column
function drop(table: Table, columnName: string): Table {
  // check if columnName exists
  if ( !hasColumn(table, columnName) ) {
    throw new Error("no such column: " + columnName);
  }

  var newHeaders = table._headers;
  var newRows = _deepCopy(table._rows);
  var columnIndex = _tableGetColumnIndex(table, columnName);
  newHeaders.splice(columnIndex, 1);

  for ( let i = 0; i < newRows.length; i++ ) {
    newRows[i].splice(columnIndex, 1);
  }

  var newTable = _makeTable(newHeaders, newRows);
  return newTable;
}

// returns a new table with elements of both tables
function stack(table: Table, bot: Table): Table {
  var tableHeaders = table._headers;
  var headersToSort = _deepCopy(table._headers);
  var botHeaders = _deepCopy(bot._headers);
  if ( !(_primitiveArraysEqual(headersToSort.sort(), botHeaders.sort())) ) {
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

function tableFromRows(rows: Row[]): Table {
  if (rows.length === 0) {
    throw new Error("table-from-rows: expected one or more rows");
  }

  const headers: string[][] = rows.map(row => row._headers);

  for (let i = 0; i < headers.length; i++) {
    if (!_primitiveArraysEqual(headers[i], headers[0])) {
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
  '_makeTableSkeletonFromCSVString': _makeTableSkeletonFromCSVString,
  '_makeTableFromTableSkeleton': _makeTableFromTableSkeleton,
  '_tableSkeletonChangeHeaders': _tableSkeletonChangeHeaders,
  'csv-open': _makeTableSkeletonFromCSVFile,
  '_makeTableFromCSVFile': _makeTableFromCSVFile,
  '_makeTableFromCSVString': _makeTableFromCSVString,
  '_primitiveEqual': _primitiveEqual,
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
  'filter': filter,
  'filter-by': filterBy,
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
