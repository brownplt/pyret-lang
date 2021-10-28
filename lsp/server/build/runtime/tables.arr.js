"use strict";
// @ts-ignore
var __spreadArray = (this && this.__spreadArray) || function (to, from) {
    for (var i = 0, il = from.length, j = to.length; i < il; i++, j++)
        to[j] = from[i];
    return to;
};
exports.__esModule = true;
// TODO(alex): Swapped from equalAlways to equalNow internally
//   to test equality for (potentially) mutable raw arrays properly.
//   Is this "correct"?
var Equality = require("./equality.js");
var Primitives = require("./primitives.js");
var PyretOption = require("./option.arr.js");
var RA = require("./raw-array.arr.js");
var List = require("./lists.arr.js");
var parse = require("csv-parse/lib/sync");
var fs = require("fs");
var jsnums = require("./js-numbers.js");
function getColumnNames(row) {
    return List.list.make(row._headers);
}
function getValue(row, columnName) {
    var columnIndex = row._headers.indexOf(columnName);
    if (columnIndex === -1) {
        throw new Error("get-value: column does not exist");
    }
    return row._elements[columnIndex];
}
function rowGet(row, columnName) {
    var columnIndex = row._headers.indexOf(columnName);
    if (columnIndex === -1) {
        // @ts-ignore
        return PyretOption.none;
    }
    else {
        // @ts-ignore
        return PyretOption.some(row._elements[columnIndex]);
    }
}
function rawRow(elements) {
    var headers = [];
    var rowElements = [];
    for (var i = 0; i < elements.length; i++) {
        var _a = elements[i], h = _a[0], e = _a[1];
        headers.push(h);
        rowElements.push(e);
    }
    var result = {
        '_headers': headers,
        '_elements': rowElements,
        'get-column-names': function () { return getColumnNames(result); },
        'get-value': function (columnName) { return getValue(result, columnName); },
        'get': function (columnName) { return rowGet(result, columnName); },
        '$brand': Primitives["$PRowBrand"]
    };
    return result;
}
function zipWith(f, xs, ys) {
    if (xs.length !== ys.length) {
        throw new Error("can't zipWith arrays of different lengths");
    }
    var result = [];
    for (var i = 0; i < xs.length; i++) {
        result.push(f(xs[i], ys[i]));
    }
    return result;
}
function zip(xs, ys) {
    return zipWith(function (x, y) { return [x, y]; }, xs, ys);
}
function _row(table) {
    var columns = [];
    for (var _i = 1; _i < arguments.length; _i++) {
        columns[_i - 1] = arguments[_i];
    }
    var elements = zip(table._headers, columns);
    return rawRow(elements);
}
function _buildColumn(table, columnName, computeNewVal) {
    if (table._headers.indexOf(columnName) !== -1) {
        throw new Error("duplicate column name: " + columnName);
    }
    var headers = _deepCopy(table._headers);
    var newHeaders = headers.slice();
    newHeaders.push(columnName);
    var rows = _deepCopy(table._rows);
    rows.forEach(function (row) {
        row.push(computeNewVal(rawRow(zip(headers, row))));
    });
    return _makeTable(newHeaders, rows);
}
function _addColumn(table, columnName, newVals) {
    var headers = _deepCopy(table._headers);
    for (var i = 0; i < headers.length; i++) {
        if (headers[i] === columnName) {
            throw new Error("duplicate column name: " + columnName);
        }
    }
    var rows = _deepCopy(table._rows);
    if (rows.length !== newVals.length) {
        throw new Error("length of new column is different than the length of the table");
    }
    for (var i = 0; i < rows.length; i++) {
        rows[i].push(newVals[i]);
    }
    headers.push(columnName);
    return _makeTable(headers, rows);
}
function _addRow(table, row) {
    var tableHeaders = table._headers;
    var rowHeaders = row._headers;
    if (!Equality.equalNow(tableHeaders, rowHeaders)) {
        throw new Error("table does not have the same column names as the new row");
    }
    var tableRows = _deepCopy(table._rows);
    tableRows.push(row._elements);
    return _makeTable(tableHeaders, tableRows);
}
function _rowN(table, index) {
    if (index >= table._rows.length) {
        throw new Error("index " + index + " out of bounds in table rows");
    }
    return rawRow(zip(table._headers, table._rows[index]));
}
function _columnN(table, index) {
    if (index >= table._headers.length) {
        throw new Error("index " + index + " out of bounds in table columns");
    }
    return List.list.make(table._rows.map(function (row) { return row[index]; }));
}
// TODO(alex): Pyret list def
// Returns PyretList<string>
function _columnNames(table) {
    return List.list.make(table._headers);
}
// TODO(alex): common list definition
// Actually returns PyretList<Row>
function _allRows(table) {
    return List.list.make(table._rows
        .map(function (row) {
        return rawRow(zip(table._headers, row));
    }));
}
// TODO(alex): common list definition
function _allColumns(table) {
    var rows = table._rows;
    var headers = table._headers;
    var columns = headers.map(function (_) { return []; });
    for (var i = 0; i < columns.length; i++) {
        for (var j = 0; j < rows.length; j++) {
            columns[i].push(rows[j][i]);
        }
    }
    var columnsList = List.list.make(columns.map(function (c) { return List.list.make(c); }));
    return columnsList;
}
function _makeTable(headers, rows) {
    var headerIndex = {};
    for (var i = 0; i < headers.length; i++) {
        headerIndex["column:" + headers[i]] = i;
    }
    var table = {
        'add-column': function (columnName, newVals) { return _addColumn(table, columnName, newVals); },
        'add-row': function (row) { return _addRow(table, row); },
        'all-columns': function () { return _allColumns(table); },
        'all-rows': function () { return _allRows(table); },
        'build-column': function (columnName, computeNewVal) { return _buildColumn(table, columnName, computeNewVal); },
        'column-n': function (index) { return _columnN(table, index); },
        'column-names': function () { return _columnNames(table); },
        'decreasing-by': function (columnName) { return decreasingBy(table, columnName); },
        'drop': function (columnName) { return drop(table, columnName); },
        'empty': function () { return empty(table); },
        'filter': function (predicate) { return filter(table, predicate); },
        'filter-by': function (columnName, predicate) { return filterBy(table, columnName, predicate); },
        'get-column': function (columnName) { return getColumn(table, columnName); },
        'increasing-by': function (columnName) { return increasingBy(table, columnName); },
        'length': function () { return rows.length; },
        'order-by': function (columnName, asc) { return orderBy(table, columnName, asc); },
        'order-by-columns': function (columns) { return orderByColumns(table, columns); },
        'rename-column': function (oldName, newName) { return renameColumn(table, oldName, newName); },
        'row': function () {
            var columns = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                columns[_i] = arguments[_i];
            }
            return _row.apply(void 0, __spreadArray([table], columns));
        },
        'row-n': function (index) { return _rowN(table, index); },
        'select-columns': function (columnNames) { return _selectColumns(table, columnNames); },
        'stack': function (bot) { return stack(table, bot); },
        'transform-column': function (columnName, update) { return transformColumn(table, columnName, update); },
        '_headerIndex': headerIndex,
        '_headers': headers,
        '_rows': rows,
        '$brand': Primitives["$PTableBrand"]
    };
    return table;
}
function _tableSkeletonChangeHeaders(skeleton, newHeaders) {
    if (newHeaders.length !== skeleton.headers.length) {
        throw new Error("Expected " + skeleton.headers.length + " headers, but got "
            + newHeaders.length + " in " + newHeaders);
    }
    return { headers: newHeaders, rows: skeleton.rows };
}
var CellType;
(function (CellType) {
    CellType[CellType["CellString"] = 0] = "CellString";
    CellType[CellType["CellNumber"] = 1] = "CellNumber";
    CellType[CellType["CellBoolean"] = 2] = "CellBoolean";
})(CellType || (CellType = {}));
;
function guessType(val) {
    var maybeNumber = jsnums.fromString(val);
    if (maybeNumber !== false) {
        return CellType.CellNumber;
    }
    else if (val === "TRUE" || val === "FALSE") {
        return CellType.CellBoolean;
    }
    else {
        return CellType.CellString;
    }
}
function convertCell(c, s) {
    switch (c) {
        case CellType.CellString:
            return s;
        case CellType.CellNumber:
            return jsnums.fromString(s);
        case CellType.CellBoolean:
            return s === "TRUE";
        default:
            throw new Error("Unknown cell type: " + String(c) + " " + String(s));
    }
}
function _makeTableSkeletonFromCSVString(s) {
    var headers = [];
    var csv = parse(s, {
        columns: function (header) {
            return header.map(function (column) {
                headers.push(column);
                return column;
            });
        }
    });
    var converters = [];
    var rows = csv.map(function (row) {
        var result = [];
        if (converters.length === 0) {
            for (var i = 0; i < headers.length; i++) {
                converters.push(guessType(row[headers[i]]));
            }
        }
        for (var i = 0; i < headers.length; i++) {
            result.push(convertCell(converters[i], row[headers[i]]));
        }
        return result;
    });
    return { headers: headers, rows: rows };
}
function _makeTableSkeletonFromCSVFile(path) {
    var contents = fs.readFileSync(path, { encoding: "utf-8" });
    return _makeTableSkeletonFromCSVString(contents);
}
function _makeTableFromTableSkeleton(s) {
    return _makeTable(s.headers, s.rows);
}
function _makeTableFromCSVString(s) {
    var skeleton = _makeTableSkeletonFromCSVString(s);
    return _makeTableFromTableSkeleton(skeleton);
}
function _makeTableFromCSVFile(path) {
    var contents = fs.readFileSync(path, { encoding: "utf-8" });
    return _makeTableFromCSVString(contents);
}
// Changes the elements of a table in the specified column using the given function
function _transformColumnMutable(table, columnName, func) {
    if (!hasColumn(table, columnName)) {
        throw new Error("transformColumnMutable: tried changing the column " + columnName + " but it doesn't exist");
    }
    // index of the column to change
    var i = table["_headerIndex"]['column:' + columnName];
    table._rows.forEach(function (row) {
        return row[i] = func(row[i]);
    });
}
// Creates a new table and mutates the specified columns with the given functions
function _tableTransform(table, columnNames, updates) {
    var headers = table._headers;
    var newRows = _deepCopy(table._rows);
    var newTable = _makeTable(headers, newRows);
    for (var i = 0; i < columnNames.length; i++) {
        _transformColumnMutable(newTable, columnNames[i], updates[i]);
    }
    return newTable;
}
// transformColumn :: (Table, String, Function) -> Table
// Creates a new table that mutates the specified column for the given function
function transformColumn(table, columnName, update) {
    var headers = table._headers;
    var newRows = _deepCopy(table._rows);
    var newTable = _makeTable(headers, newRows);
    _transformColumnMutable(newTable, columnName, update);
    return newTable;
}
// returns a deep copy of (nested) arrays
function _deepCopy(arr) {
    var i, copy;
    if (Array.isArray(arr)) {
        copy = arr.slice(0);
        for (i = 0; i < copy.length; i++) {
            copy[i] = _deepCopy(copy[i]);
        }
        return copy;
    }
    else {
        return arr;
    }
}
// _tableFilter :: Table -> (Array -> Boolean) -> Table
// Creates a new Table which contains the rows from table that satisfy predicate.
function _tableFilter(table, predicate) {
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
    for (var i = 0; i < rows.length; i++) {
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
    var column = _getRawColumn(table, columnName);
    console.log(column.length);
    for (var i = 0; i < column.length; i++) {
        var predicateResult = predicate(column[i]);
        console.log("Result = " + predicateResult + ". Column " + i + ": " + column[i]);
        if (predicateResult) {
            newRows.push(table._rows[i]);
        }
    }
    return _makeTable(headers, newRows);
}
// _tableGetColumnIndex :: Table -> String -> Integer
// Returns the index of columnName, or throws an error if columnName is not a
// column in table.
function _tableGetColumnIndex(table, columnName) {
    var headers = table._headers;
    for (var index = 0; index < headers.length; index++) {
        if (headers[index] === columnName) {
            return index;
        }
    }
    throw new Error("Column " + columnName + " is not valid");
}
// Creates a Table that is like table, except that its rows are sorted according
// to columnOrders. An element in columnOrders specifies a column name and an
// ordering, either ascending or descending.
function _tableOrder(table, columnOrders) {
    var headers = table._headers;
    var rows = table._rows;
    function ordering(a, b) {
        for (var i = 0; i < columnOrders.length; i++) {
            var columnOrder = columnOrders[i];
            var name_1 = columnOrder["column"];
            var order = columnOrder["direction"];
            var index = _tableGetColumnIndex(table, name_1);
            var elemA = a[index];
            var elemB = b[index];
            if (order === "ascending") {
                if (elemA < elemB) {
                    return -1;
                }
                else if (elemA > elemB) {
                    return 1;
                }
            }
            else if (order === "descending") {
                if (elemA < elemB) {
                    return 1;
                }
                else if (elemA > elemB) {
                    return -1;
                }
            }
        }
        return 0;
    }
    // Array.prototype.sort() mutates the array it sorts, so we need to create a
    // copy with Array.prototype.slice() first.
    var sortedRows = rows.slice().sort(ordering);
    return _makeTable(headers, sortedRows);
}
// _selectColumns :: (Table, Array<String>) -> Table
function _selectColumns(table, columnNames) {
    //var colnamesList = ffi.toArray(columnNames);
    // This line of code below relies on anchor built-in lists being js arrays
    var colnamesList = columnNames;
    if (colnamesList.length === -1) {
        throw new Error("zero-columns");
    }
    for (var i = 0; i < colnamesList.length; i += 1) {
        if (!hasColumn(table, colnamesList[i])) {
            throw new Error("no-such-column");
        }
    }
    var newRows = [];
    for (var i = 0; i < table['_rows'].length; i += 1) {
        console.log(i);
        newRows[i] = [];
        for (var j = 0; j < colnamesList.length; j += 1) {
            var colIndex = _tableGetColumnIndex(table, colnamesList[j]);
            newRows[i].push(table['_rows'][i][colIndex]);
        }
    }
    return _makeTable(colnamesList, newRows);
}
// TODO(alex): Common list definition
// Returns List<any>
function _tableExtractColumn(table, columnName) {
    // throws an error if columnName is not in table
    var index = _tableGetColumnIndex(table, columnName);
    var rows = table._rows;
    var extracted = List.empty;
    for (var i = 0; i < rows.length; i++) {
        var element = rows[i][index];
        extracted = List.append(extracted, List.link(element, List.empty));
    }
    return extracted;
}
var runningSum = {
    one: function (element) { return [element, element]; },
    reduce: function (acc, element) {
        var sum = acc + element;
        return [sum, sum];
    }
};
function isReducingExtension(x) {
    return x.type === 'reduce';
}
function isMappingExtension(x) {
    return x.type === 'map';
}
// Creates a new Table that is like table, except that it has one or more new columns,
// as specified by the supplied TableExtensions.
function _tableReduce(table, extensions) {
    var headers = table['_headers'];
    var rows = table['_rows'];
    var extendedColumns = extensions.map(function (extension) { return extension.extending; });
    var newHeaders = headers.concat(extendedColumns);
    var newRows = rows.slice();
    var newTable = _makeTable(newHeaders, newRows);
    if (rows.length === 0) {
        return newTable;
    }
    var accumulators = [];
    for (var i = 0; i < extensions.length; i++) {
        var extension = extensions[i];
        if (isMappingExtension(extension)) {
            var mapping = extension;
            var row = newRows[0];
            var extending = mapping.extending;
            var extendingIndex = _tableGetColumnIndex(newTable, extending);
            row[extendingIndex] = mapping.reduce(0);
        }
        else if (isReducingExtension(extension)) {
            var reducing = extension;
            var row = newRows[0];
            var one = reducing.one;
            var using = reducing.using;
            var extending = reducing.extending;
            var usingIndex = _tableGetColumnIndex(newTable, using);
            var extendingIndex = _tableGetColumnIndex(newTable, extending);
            var _a = one(row[usingIndex]), acc = _a[0], outVal = _a[1];
            accumulators[i] = acc;
            row[extendingIndex] = outVal;
        }
        else {
            throw "_tableReduce: extension is not a TableExtension";
        }
    }
    for (var i = 1; i < newRows.length; i++) {
        for (var j = 0; j < extensions.length; j++) {
            var extension = extensions[j];
            if (isMappingExtension(extension)) {
                var mapping = extension;
                var row = newRows[i];
                var extending = mapping.extending;
                var extendingIndex = _tableGetColumnIndex(newTable, extending);
                row[extendingIndex] = mapping.reduce(i);
            }
            else if (isReducingExtension(extension)) {
                var reducing = extension;
                var row = newRows[i];
                var reduce = reducing.reduce;
                var using = reducing.using;
                var extending = reducing.extending;
                var usingIndex = _tableGetColumnIndex(newTable, using);
                var extendingIndex = _tableGetColumnIndex(newTable, extending);
                var _b = reduce(accumulators[j], row[usingIndex]), acc = _b[0], outVal = _b[1];
                accumulators[j] = acc;
                row[extendingIndex] = outVal;
            }
            else {
                throw "_tableReduce: extension is not a TableExtension";
            }
        }
    }
    return _makeTable(newHeaders, newRows);
}
function hasColumn(table, columnName) {
    return table._headers.includes(columnName);
}
// TODO(alex): Common Pyret list definition
// Returns a PyretList<any>
function getColumn(table, columnName) {
    // Raise error if table lacks column
    if (!hasColumn(table, columnName)) {
        throw new Error("no such column: " + columnName);
    }
    var columnIndex;
    Object.keys(table._headers).forEach(function (i) {
        if (table._headers[i] == columnName) {
            columnIndex = i;
        }
    });
    return table._rows.reduceRight(function (acc, row) {
        acc = List.link(row[columnIndex], acc);
        return acc;
    }, List.empty);
}
// TODO(alex): merge duplicated code between getColumn() and _getRawColumn()
function _getRawColumn(table, columnName) {
    // Raise error if table lacks column
    if (!hasColumn(table, columnName)) {
        throw new Error("no such column: " + columnName);
    }
    var columnIndex;
    Object.keys(table._headers).forEach(function (i) {
        if (table._headers[i] == columnName) {
            columnIndex = i;
        }
    });
    return table._rows.map(function (row) { return row[columnIndex]; });
}
function _length(table) {
    return table._rows.length;
}
// creates a new table with a column renamed
function renameColumn(table, oldName, newName) {
    // check if oldName exists
    if (!hasColumn(table, oldName)) {
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
function increasingBy(table, columnName) {
    // check if columnName exists
    if (!hasColumn(table, columnName)) {
        throw new Error("no such column");
    }
    var headers = table._headers;
    var newRows = _deepCopy(table._rows);
    var colIndex = _tableGetColumnIndex(table, columnName);
    function ordering(a, b) {
        var elemA = a[colIndex];
        var elemB = b[colIndex];
        if (elemA < elemB) {
            return -1;
        }
        else if (elemA > elemB) {
            return 1;
        }
        return 0;
    }
    var sortedRows = newRows.slice().sort(ordering);
    var newTable = _makeTable(headers, sortedRows);
    return newTable;
}
// orders column in descending order
function decreasingBy(table, columnName) {
    // check if columnName exists
    if (!hasColumn(table, columnName)) {
        throw new Error("no such column");
    }
    var newHeaders = table._headers;
    var newRows = _deepCopy(table._rows);
    var colIndex = _tableGetColumnIndex(table, columnName);
    function ordering(a, b) {
        var elemA = a[colIndex];
        var elemB = b[colIndex];
        if (elemA < elemB) {
            return 1;
        }
        else if (elemA > elemB) {
            return -1;
        }
        return 0;
    }
    var sortedRows = newRows.slice().sort(ordering);
    var newTable = _makeTable(newHeaders, sortedRows);
    return newTable;
}
// orders a column ascending or descending depending on the boolean
function orderBy(table, columnName, asc) {
    if (asc) {
        return increasingBy(table, columnName);
    }
    else {
        return decreasingBy(table, columnName);
    }
}
function orderByColumns(table, columns) {
    var headers = table._headers;
    var rows = table._rows;
    columns = List["to-raw-array"](columns);
    function ordering(a, b) {
        for (var i = 0; i < columns.length; i++) {
            var columnOrder = columns[i];
            var name_2 = columnOrder[0];
            var order = columnOrder[1];
            var index = _tableGetColumnIndex(table, name_2);
            var elemA = a[index];
            var elemB = b[index];
            if (order === true) {
                if (elemA < elemB) {
                    return -1;
                }
                else if (elemA > elemB) {
                    return 1;
                }
            }
            else if (order === false) {
                if (elemA < elemB) {
                    return 1;
                }
                else if (elemA > elemB) {
                    return -1;
                }
            }
        }
        return 0;
    }
    // Array.prototype.sort() mutates the array it sorts, so we need to create a
    // copy with Array.prototype.slice() first.
    var sortedRows = rows.slice().sort(ordering);
    return _makeTable(headers, sortedRows);
}
// returns an empty Table with the same column headers
function empty(table) {
    var headers = table._headers;
    var newTable = _makeTable(headers, []);
    return newTable;
}
// returns a new table without the specified column
function drop(table, columnName) {
    // check if columnName exists
    if (!hasColumn(table, columnName)) {
        throw new Error("no such column: " + columnName);
    }
    var newHeaders = table._headers;
    var newRows = _deepCopy(table._rows);
    var columnIndex = _tableGetColumnIndex(table, columnName);
    newHeaders.splice(columnIndex, 1);
    for (var i = 0; i < newRows.length; i++) {
        newRows[i].splice(columnIndex, 1);
    }
    var newTable = _makeTable(newHeaders, newRows);
    return newTable;
}
// returns a new table with elements of both tables
function stack(table, bot) {
    var tableHeaders = table._headers;
    var headersToSort = _deepCopy(table._headers);
    var botHeaders = _deepCopy(bot._headers);
    if (!(Equality.equalNow(headersToSort.sort(), botHeaders.sort()))) {
        throw new Error("headers do not match");
    }
    var newRows = _deepCopy(table._rows);
    for (var i = 0; i < bot._rows.length; i++) {
        newRows.push([]);
        for (var j = 0; j < tableHeaders.length; j++) {
            newRows[newRows.length - 1]
                .push(bot._rows[i][_tableGetColumnIndex(bot, tableHeaders[j])]);
        }
    }
    var newTable = _makeTable(tableHeaders, newRows);
    return newTable;
}
function tableFromRows(rows) {
    if (rows.length === 0) {
        throw new Error("table-from-rows: expected one or more rows");
    }
    var headers = rows.map(function (row) { return row._headers; });
    for (var i = 0; i < headers.length; i++) {
        if (!Equality.equalNow(headers[i], headers[0])) {
            throw "table-from-rows: row name mismatch";
        }
    }
    var elements = rows.map(function (row) { return row._elements; });
    return _makeTable(headers[0], elements);
}
// TODO(alex): common List definition
// NOTE(alex): Used as a constructor function (therefore taking an array of Columns)
function tableFromColumns(columns) {
    if (columns.length === 0) {
        throw new Error("expected at least one column");
    }
    var duplicate_cache = {};
    var columnLength = columns[0][1].length();
    var duplicates = new Array();
    var columnArrays = new Array();
    var headers = new Array();
    columns.forEach(function (column) {
        var header = column[0];
        if (duplicate_cache[header] !== undefined) {
            throw new Error("duplicate header: " + header);
        }
        var rawArray = List["to-raw-array"](column[1]);
        if (columnLength !== rawArray.length) {
            throw new Error("columns must have the same number of elements");
        }
        headers.push(header);
        columnArrays.push(rawArray);
    });
    // Convert columns to rows for internal consumption
    var rows = new Array(columnLength);
    for (var i = 0; i < columnArrays.length; i++) {
        for (var j = 0; j < columnLength; j++) {
            var element = columnArrays[i][j];
            if (rows[j] === undefined) {
                rows[j] = new Array();
            }
            rows[j].push(element);
        }
    }
    return _makeTable(headers, rows);
}
// TODO(alex): common List definition
// Takes in a PyretList<any>
function tableFromColumn(columnName, values) {
    var col = [columnName, values];
    return tableFromColumns([col]);
}
module.exports = {
    '_makeTableSkeletonFromCSVString': _makeTableSkeletonFromCSVString,
    '_makeTableFromTableSkeleton': _makeTableFromTableSkeleton,
    '_tableSkeletonChangeHeaders': _tableSkeletonChangeHeaders,
    'csv-open': _makeTableSkeletonFromCSVFile,
    '_makeTableFromCSVFile': _makeTableFromCSVFile,
    '_makeTableFromCSVString': _makeTableFromCSVString,
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
