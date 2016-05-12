define(["js/runtime-util", "js/type-util", "js/namespace", "trove/valueskeleton"], function(util, t, Namespace, valueskeleton) {
  return util.memoModule("table", function(runtime, namespace) {
    return runtime.loadModulesNew(namespace, [valueskeleton], function(VSlib) {

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

      function makeTable(headers, rows) {
        runtime.ffi.checkArity(2, arguments, "makeTable");
      
        var headerIndex = {};
        
        for(var i=0; i<headers.length; i++) {
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
          var obj = {}
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
              runtime.ffi.checkArity(2, arguments, "get-row");
              runtime.checkArrayIndex("get-row", rows, row_index);
              return runtime.makeObject(getRowAsRecord(row_index));
            }),
          
          'length': runtime.makeMethod0(function(_) {
              runtime.ffi.checkArity(1, arguments, "length");
              return runtime.makeNumber(rows.length);
            }),
          
          'get-column': runtime.makeMethod1(function(_, col_name) {
              runtime.ffi.checkArity(2, arguments, "get-column");
              if(!hasColumn(col_name)) {
                /* Throw error */
              }
              return runtime.makeList(getColumn(col_name));
            }),
            
          '_column-index': runtime.makeMethod3(function(_, table_loc, col_name, col_loc) {
              runtime.ffi.checkArity(4, arguments, "_column-index");
              var col_index = headerIndex['column:'+col_name];
              if(col_index === undefined)
                runtime.ffi.throwMessageException("The table does not have a column named `"+col_name+"`.");
              else return col_index;
            }),
            
          '_no-column': runtime.makeMethod3(function(_, table_loc, col_name, col_loc) {
              runtime.ffi.checkArity(4, arguments, "_column-index");
              var col_index = headerIndex['column:'+col_name];
              if(col_index != undefined)
                runtime.ffi.throwMessageException("The table already has a column named `"+col_name+"`.");
              else return col_index;
            }),
          
          '_equals': runtime.makeMethod2(function(self, other, equals) {
            runtime.ffi.checkArity(3, arguments, "_equals");
            // is the other a table
            // same number of columns?
            // same number of rows?
            // columns have same names?
            // each row has the same elements
            if (hasBrand(brandTable, other)
             && headers.length == get(other, "_header-raw-array").length
             && rows.length    == get(other, "_rows-raw-array").length
             && (function(){
                  otherHeaders = get(other, "_header-raw-array")
                  for(var i=0; i < headers.length; i++) {
                    if(headers[i] != otherHeaders[i])
                      return false;
                  } return true;})()
             && (function(){
                  debugger;
                  rowLength = headers.length;
                  otherRows = get(other, "_rows-raw-array");
                  var equal = true;
                  for(var i=0; i < rows.length && equal; i++) {
                    var selfRow = rows[i];
                    var otherRow = otherRows[i];
                    for(var j=0; j < rowLength && equal; j++) {
                      equal &= runtime.safeCall(
                        function() {return equals.app(selfRow[j], otherRow[j]);},
                        function(r){return runtime.ffi.isEqual(r);});
                    }
                  }
                  return equal;})()) {
              return runtime.ffi.equal;
            } else {
              return runtime.ffi.notEqual.app('', self, other);
            }
          }),
          
          '_output': runtime.makeMethod0(function(_) {
            runtime.ffi.checkArity(1, arguments, "_output");
            var vsValue = get(VS, "vs-value").app;
            var vsString = get(VS, "vs-str").app;
            return get(VS, "vs-table").app(
              headers.map(function(hdr){return vsString(hdr);}),
              rows.map(function(row){return row.map(
                function(elm){return vsValue(elm);});}));
          })
        }));
      }
      
      return runtime.makeObject({
        "provide-plus-types": runtime.makeObject({
          types: {
            'Table': annTable
          },
          values: runtime.makeObject({
            'makeTable': makeTable,
            'isTable':   isTable,
          }),
        })});
        
    });
  });
});
