function _makeTable(headers, rows) {
  var headerIndex = {};

  for (var i = 0; i < headers.length; i++) {
    headerIndex["column:" + headers[i]] = i;
  }

  function getColumn(column_name) {
    /* TODO: Raise error if table lacks column */
    var column_index;
    Object.keys(headers).forEach(function(i) {
      if(headers[i] == column_name) { column_index = i; }
    });
    return rows.map(function(row){return row[column_index];});
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

function hasColumn(table, column_name) {
  return table._headerIndex.hasOwnProperty("column:" + column_name);
}

module.exports = {
  '_makeTable': _makeTable,
  '_tableFilter': _tableFilter,
  '_tableGetColumnIndex': _tableGetColumnIndex,
  '_selectColumns': _selectColumns
};
