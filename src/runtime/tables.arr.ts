const List = require("./list.arr.js");

function _makeTable(headers, rows) {
  var headerIndex = {};

  for (var i = 0; i < headers.length; i++) {
    headerIndex["column:" + headers[i]] = i;
  }

  function getRowContentAsRecordFromHeaders(headers, raw_row) {
    /* TODO: Raise error if no row at index */
    var obj = {};
    for(var i = 0; i < headers.length; i++) {
      obj[headers[i]] = raw_row[i];
    }
    return obj;
  }

  function getRowContentAsRecord(raw_row) {
    return getRowContentAsRecordFromHeaders(headers, raw_row);
  }

  function getRowContentAsGetter(headers, raw_row) {
    var obj = getRowContentAsRecordFromHeaders(headers, raw_row);
    obj["get-value"] = function(key) {
      if(obj.hasOwnProperty(key)) {
        return obj[key];
      }
      else {
        throw "Not found: " + key;
      }
    };
    return obj;
  }

  return {
    '_headers-raw-array': headers,
    '_rows-raw-array': rows,
    'length': function(_) { return rows.length; },
    '_headerIndex': headerIndex,

    $brand: '$table'
  };
}

// _transformColumnMutable :: (Table, String, Function) -> none
// Changes the elements of a table in the specified column using the given function
function _transformColumnMutable(table, colname, func) {
  if(!hasColumn(table, colname)) {
    throw "transformColumnMutable: tried changing the column " + colname + " but it doesn't exist";
  }

  // index of the column to change
  var i = table["_headerIndex"]['column:' + colname];

  table["_rows-raw-array"].forEach((row) =>
    row[i] = func(row[i])
  );
}

// _tableTransform :: (Table, Array<String>, Array<Function>) -> Table
// Creates a new table and mutates the specified columns with the given functions
function _tableTransform(table, colnames, updates) {
  var newHeaders = _deepCopy(table["_headers-raw-array"]);
  var newRows = _deepCopy(table["_rows-raw-array"]);
  var newTable = _makeTable(newHeaders, newRows);

  for (let i = 0; i < colnames.length; i++) {
    _transformColumnMutable(newTable, colnames[i], updates[i]);
  }

  return newTable;
}

// transformColumn :: (Table, String, Function) -> Table
// Creates a new table that mutates the specified column for the given function
function transformColumn(table, colname, update) {
  var newHeaders = _deepCopy(table["_headers-raw-array"]);
  var newRows = _deepCopy(table["_rows-raw-array"]);
  var newTable = _makeTable(newHeaders, newRows);
  _transformColumnMutable(newTable, colname, update);
  return newTable;
}

// returns a deep copy of (nested) arrays
function _deepCopy(arr) {
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
function _tableFilter(table, predicate) {
  return _makeTable(table["_headers-raw-array"], table["_rows-raw-array"].filter(predicate));
}

// _tableGetColumnIndex :: Table -> String -> Integer
// Returns the index of column_name, or throws an error if column_name is not a
// column in table.
function _tableGetColumnIndex(table, column_name) {
  const headers = table["_headers-raw-array"];

  for (let index = 0; index < headers.length; index++) {
    if (headers[index] === column_name) {
      return index;
    }
  }

  throw "not a valid column";
}

// _tableOrder : Table -> List<{'column': String, 'direction': String} -> Table
//   where in `'direction': x`, x can be either "ascending" or "descending".
// Creates a Table that is like table, except that its rows are sorted according
// to columnOrders. An element in columnOrders specifies a column name and an
// ordering, either ascending or descending.
function _tableOrder(table: any, columnOrders: any): any {
  const headers = table["_headers-raw-array"];
  const rows = table["_rows-raw-array"];

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
function _selectColumns(table, colnames) {
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
  for(var i = 0; i < table['_rows-raw-array'].length; i += 1) {
    console.log(i);
    newRows[i] = [];
    for(var j = 0; j < colnamesList.length; j += 1) {
      var colIndex = table._headerIndex['column:' + colnamesList[j]];
      newRows[i].push(table['_rows-raw-array'][i][colIndex]);
    }
  }
  return _makeTable(colnamesList, newRows);
}

function _tableExtractColumn(table: any, columnName: string): any {
  const index = _tableGetColumnIndex(table, columnName);
  const rows = table["_rows-raw-array"];
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
function _tableReduce(table: any, extensions: Array<TableExtension>): any {
  const headers = table['_headers-raw-array'];
  const rows = table['_rows-raw-array'];
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

function hasColumn(table, column_name) {
  return table._headerIndex.hasOwnProperty("column:" + column_name);
}

function getColumn(table, column_name) {
  // Raise error if table lacks column
  if ( !hasColumn(table, column_name) ) {
    throw "no such column";
  }

  var column_index;
  Object.keys(table["_headers-raw-array"]).forEach(function(i) {
    if(table["_headers-raw-array"][i] == column_name) { column_index = i; }
  });
  return table["_rows-raw-array"].map(function(row){return row[column_index];});
}

function _length(table) {
  return table["_rows-raw-array"].length;
}

// renameColumn :: ( Table, String, String ) -> Table
// creates a new table with a column renamed
function renameColumn(table, old_name, new_name) {
  // check if old_name exists
  if ( !hasColumn(table, old_name) ) {
    throw "no such column"
  }
  var newHeaders = _deepCopy(table["_headers-raw-array"]);
  var newRows = _deepCopy(table["_rows-raw-array"]);
  var colIndex = table._headerIndex['column:' + old_name];
  newHeaders[colIndex] = new_name;
  var newTable = _makeTable(newHeaders, newRows);
  return newTable;
}

module.exports = {
  '_makeTable': _makeTable,
  '_tableFilter': _tableFilter,
  '_tableGetColumnIndex': _tableGetColumnIndex,
  '_tableOrder': _tableOrder,
  '_tableExtractColumn': _tableExtractColumn,
  '_tableTransform': _tableTransform,
  '_selectColumns': _selectColumns,
  '_length': _length,
  'renameColumn': renameColumn,
  'transformColumn': transformColumn,
  '_tableReduce': _tableReduce,
  'running-sum': runningSum
};
