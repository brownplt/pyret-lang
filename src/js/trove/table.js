({
  requires: [
    { "import-type": "builtin", name: "valueskeleton" },
    { "import-type": "builtin", name: "ffi" }
  ],
  nativeRequires: [
    "pyret-base/js/type-util"
  ],
  provides: {},
  theModule: function(runtime, namespace, uri, VSlib, ffi, t) {
    var get = runtime.getField;

    var VS = get(VSlib, "values");
    
    var brandTable = runtime.namedBrander("table", ["table: table brander"]);
    var annTable   = runtime.makeBranderAnn(brandTable, "Table");

    function applyBrand(brand, val) {
      return get(brand, "brand").app(val);
    }
    
    function hasBrand(brand, val) {
      return get(brand, "test").app(val);
    }
    
    function isTable(val) {
      return hasBrand(brandTable,  val);
    }

    function openTable(info) {
      runtime.checkTuple(info);
      if (info.vals.length != 2) {
        runtime.ffi.throwMessageException("Expected to find {header; contents} pair, "
                                          + "but found a tuple of length "
                                          + info.vals.length);
      }
      var headers = info.vals[0];
      var contents = info.vals[1];
      runtime.checkArray(headers);
      runtime.checkArray(contents);
      var names = [];
      var sanitizers = [];
      for(var i = 0; i < headers.length; ++i) {
        runtime.checkTuple(headers[i]);
        if (headers[i].vals.length !== 2) {
          runtime.ffi.throwMessageException("Expected to find {name; sanitizer} pairs "
                                            + "in header data, but found a tuple of "
                                            + "length " + headers[i].vals.length);
        }
        var header = headers[i].vals;
        runtime.checkString(header[0]);
        runtime.checkFunction(header[1]);
        names.push(header[0]);
        sanitizers.push(header[1]);
      }
      for(var i = 0; i < contents.length; ++i) {
        runtime.checkArray(contents[i]);
        if (contents[i].length !== headers.length) {
          if (i === 0) {
            runtime.ffi.throwMessageException("Contents must match header size");
          } else {
            runtime.ffi.throwMessageException("Contents must be rectangular");
          }
        }
        for (var j = 0; j < contents[i].length; ++j) {
          runtime.checkCellContent(contents[i][j]);
        }
        contents[i] = runtime.raw_array_mapi(runtime.makeFunction(function(v, j) {
          return sanitizers[j].app(contents[i][j], names[j], runtime.makeNumber(i));
        }), contents[i]);
      }
      return makeTable(names, contents);
    }

    function makeTable(headers, rows) {
      ffi.checkArity(2, arguments, "makeTable");
      
      var headerIndex = {};
      
      for (var i = 0; i < headers.length; i++) {
        headerIndex["column:" + headers[i]] = i;
      }
      
      function getColumn(column_name) {
        /* TODO: Raise error if table lacks column */
        var column_index = headers[column_name];
        return rows.map(function(row){return rows[column_index];});
      }
      
      function hasColumn(column_name) {
        return headerIndex.hasOwnProperty("column:" + column_name);
      }
      
      function getRowAsRecord(row_index) {
        /* TODO: Raise error if no row at index */
        var obj = {};
        var row = rows[row_index];
        for(var i = 0; i < headers.length; i++) {
          obj[headers[i]] = row[i];
        }
        return obj;
      }

      return applyBrand(brandTable, runtime.makeObject({
        
        '_header-raw-array': headers,
        '_rows-raw-array': rows,
        
        'get-row': runtime.makeMethod1(function(_, row_index) {
          ffi.checkArity(2, arguments, "get-row");
          runtime.checkArrayIndex("get-row", rows, row_index);
          return runtime.makeObject(getRowAsRecord(row_index));
        }),
        
        'length': runtime.makeMethod0(function(_) {
          ffi.checkArity(1, arguments, "length");
          return runtime.makeNumber(rows.length);
        }),
        
        'get-column': runtime.makeMethod1(function(_, col_name) {
          ffi.checkArity(2, arguments, "get-column");
          if(!hasColumn(col_name)) {
            ffi.throwMessageException("The table does not have a column named `"+col_name+"`.");
          }
          return runtime.makeList(getColumn(col_name));
        }),
        
        '_column-index': runtime.makeMethod3(function(_, table_loc, col_name, col_loc) {
          ffi.checkArity(4, arguments, "_column-index");
          var col_index = headerIndex['column:'+col_name];
          if(col_index === undefined)
            ffi.throwMessageException("The table does not have a column named `"+col_name+"`.");
          return col_index;
        }),
        
        '_no-column': runtime.makeMethod3(function(_, table_loc, col_name, col_loc) {
          ffi.checkArity(4, arguments, "_column-index");
          var col_index = headerIndex['column:'+col_name];
          if(col_index != undefined)
            ffi.throwMessageException("The table already has a column named `"+col_name+"`.");
          return col_index;
        }),
        
        '_equals': runtime.makeMethod2(function(self, other, equals) {
          ffi.checkArity(3, arguments, "_equals");
          // is the other a table
          // same number of columns?
          // same number of rows?
          // columns have same names?
          // each row has the same elements
          var eq  = function() { return ffi.equal; };
          var neq = function() { return ffi.notEqual.app('', self, other); };
          if (!hasBrand(brandTable, other)) {
            return neq();
          }
          var otherHeaders = get(other, "_header-raw-array");
          var otherRows = get(other, "_rows-raw-array");
          if (headers.length !== otherHeaders.length
              || rows.length !== otherRows.length) {
            return neq();
          }
          for (var i = 0; i < headers.length; ++i) {
            if (headers[i] != otherHeaders[i]) {
              return neq();
            }
          }
          for (var i = 0; i < rows.length; ++i) {
            var selfRow = rows[i];
            var otherRow = otherRows[i];
            var colEqual = function(j) {
              return function() {
                return equals.app(selfRow[j], otherRow[j]);
              };
            };
            var liftEquals = function(r) {
              return ffi.isEqual(r);
            };
            for (var j = 0; j < headers.length; ++j) {
              if (!(runtime.safeCall(colEqual(j), liftEquals))) {
                return neq();
              }
            }
          }
          return eq();
        }),
        
        '_output': runtime.makeMethod0(function(_) {
          ffi.checkArity(1, arguments, "_output");
          var vsValue = get(VS, "vs-value").app;
          var vsString = get(VS, "vs-str").app;
          return get(VS, "vs-table").app(
            headers.map(function(hdr){return vsString(hdr);}),
            rows.map(function(row){return row.map(
              function(elm){return vsValue(elm);});}));
        })
      }));
    }
    
    return runtime.makeJSModuleReturn({
      TableAnn : annTable,
      makeTable: makeTable,
      openTable: openTable,
      isTable: isTable },
      {});
  }
})
