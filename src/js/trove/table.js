({
  requires: [
    { "import-type": "builtin", name: "valueskeleton" },
    { "import-type": "builtin", name: "equality" },
    { "import-type": "builtin", name: "ffi" }
  ],
  nativeRequires: [
    "pyret-base/js/type-util"
  ],
  provides: {},
  theModule: function(runtime, namespace, uri, VSlib, EQlib, ffi, t) {
    var get = runtime.getField;

    var VS = get(VSlib, "values");
    var EQ = get(EQlib, "values");

    var eq  = function() { return ffi.equal; };
    var neq = function(left, right) { return ffi.notEqual.app('', left, right); };
    
    var brandTable = runtime.namedBrander("table", ["table: table brander"]);
    var annTable   = runtime.makeBranderAnn(brandTable, "Table");

    var brandRow = runtime.namedBrander("row", ["table: row brander"]);
    var annRow   = runtime.makeBranderAnn(brandRow, "Row");

    var rowGetValue = runtime.makeMethod1(function(self, arg) {
        ffi.checkArity(2, arguments, "get-value", false);
        runtime.checkString(arg);
        var index = self.$underlyingTable.headerIndex["column:" + arg];
        if(typeof index === "number") {
          return self.$rowData[index];
        }
        else {
          return ffi.throwMessageException("No such column: " + arg);
        }
      });

    var rowEquals = runtime.makeMethod2(function(self, other, rec) {
        runtime.checkRow(self);
        runtime.checkRow(other);
        var headers1 = self.$underlyingTable.headerIndex;
        var headers2 = other.$underlyingTable.headerIndex;
        var hk1 = Object.keys(headers1);
        var hk2 = Object.keys(headers2);
        var rowData1 = self.$rowData;
        var rowData2 = other.$rowData;
        if(rowData1.length !== rowData2.length) {
          return neq(self, other);
        }
        if(hk1.length !== hk2.length) {
          return neq(self, other);
        }
        for(var i = 0; i < hk1.length; i += 1) {
          if(headers1[hk1[i]] !== headers2[hk1[i]]) {
            return neq(self, other);
          }
        }
        return runtime.raw_array_fold(runtime.makeFunction(function(ans, val1, j) {
          if (ffi.isNotEqual(ans)) { return ans; }
          return runtime.safeCall(function() {
            return rec.app(val1, rowData2[j]);
          }, function(eqAns) {
            return get(EQ, "equal-and").app(ans, eqAns);
          }, "equals:combine-cells");
        }), eq(), rowData1, 0);
      });

    function makeRow(underlyingTable, rowData) {
      var rowVal = runtime.makeObject({
        "get-value": rowGetValue,
        "_equals": rowEquals
      });
      rowVal = applyBrand(brandRow, rowVal);
      rowVal.$underlyingTable = underlyingTable;
      rowVal.$rowData = rowData;
      return rowVal;
    }

    function makeRowFromArray(rawArrayOfTuples) {
      var headerIndex = [];
      var rowData = [];

      // TODO(joe): error checking here for bogus values, or elsewhere?
      // May be good to keep this fast and do checks in Pyret-land
      for(var i = 0; i < rawArrayOfTuples.length; i += 1) {
        var colname = rawArrayOfTuples[i].vals[0];
        if(headerIndex["column:" + colname] !== undefined) {
          return runtime.ffi.throwMessageException("Duplicate column name in row: " + colname);
        }
        headerIndex["column:" + colname] = i;
        rowData[i] = rawArrayOfTuples[i].vals[1];
      }

      return makeRow({ headerIndex: headerIndex }, rowData);
    }

    function applyBrand(brand, val) {
      return get(brand, "brand").app(val);
    }
    
    function hasBrand(brand, val) {
      return get(brand, "test").app(val);
    }
    
    function isTable(val) {
      return hasBrand(brandTable,  val);
    }

    function isRow(val) {
      return hasBrand(brandRow,  val);
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
      return runtime.safeCall(function() {
        return runtime.eachLoop(runtime.makeFunction(function(i) {
          runtime.checkArray(contents[i]);
          if (contents[i].length !== headers.length) {
            if (i === 0) {
              runtime.ffi.throwMessageException("Contents must match header size");
            } else {
              runtime.ffi.throwMessageException("Contents must be rectangular");
            }
          }
          // This loop is stack safe, since it's just a brand-checker
          for (var j = 0; j < contents[i].length; ++j) {
            runtime.checkCellContent(contents[i][j]);
          }
          return runtime.safeCall(function() {
            return runtime.raw_array_mapi(runtime.makeFunction(function(v, j) {
              return sanitizers[j].app(contents[i][j], names[j], runtime.makeNumber(i));
            }), contents[i]);
          }, function(new_contents_i) {
            contents[i] = new_contents_i;
            return runtime.nothing;
          }, "openTable:assign-rows");
        }), 0, contents.length);
      }, function(_) {
        return makeTable(names, contents);
      }, "openTable");
    }

    function makeTable(headers, rows) {
      ffi.checkArity(2, arguments, "makeTable", false);
      
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
        obj["get-value"] = runtime.makeFunction(function(key) {
            if(obj.hasOwnProperty(key)) {
              return obj[key];
            }
            else {
              runtime.ffi.throwMessageException("Not found: " + key);
            }
          });
        return runtime.makeObject(obj);
      }

      function multiOrder(sourceArr, colComps, destArr) {
        // sourceArr is a raw JS array of table rows
        // colComps is an array of 2-element arrays, [true iff ascending, colName]
        // destArr is the final array in which to place the sorted rows
        // returns destArr, and mutates destArr
        var colIdxs = [];
        var comps = [];
        var LESS = "less";
        var EQ = "equal";
        var MORE = "more";
        for (var i = 0; i < colComps.length; i++) {
          comps[i] = (colComps[i][0] ? runtime.lessthan : runtime.greaterthan);
          colIdxs[i] = headerIndex["column:" + colComps[i][1]];
          for (var dupIdx = i + 1; dupIdx < colComps.length; dupIdx++) {
            if (colComps[i][1] === colComps[dupIdx][1]) {
              runtime.ffi.throwMessageException(
                "Attempted to sort on the same column multiple times: "
                  + "'" + colComps[i][1] + "' is used as sort-key " + i
                  + ", and also as sort-key " + dupIdx);
            }
          }
        }
        function helper(sourceArr) {
          var lessers = [];
          var equals = [];
          var greaters = [];
          var pivot = sourceArr[0];
          equals.push(pivot);
          return runtime.safeCall(function() {
            return runtime.eachLoop(runtime.makeFunction(function(rowIdx) {
              return runtime.safeCall(function() {
                return runtime.raw_array_fold(runtime.makeFunction(function(order, comp, colIdx) {
                  if (order !== EQ) return order;
                  else {
                    return runtime.safeCall(function() {
                      return comp(sourceArr[rowIdx][colIdxs[colIdx]], pivot[colIdxs[colIdx]]);
                    }, function(isLess) {
                      if (isLess) return LESS;
                      else return runtime.safeCall(function() {
                        return runtime.equal_always(sourceArr[rowIdx][colIdxs[colIdx]], pivot[colIdxs[colIdx]]);
                      }, function(isEqual) {
                        return (isEqual ? EQ : MORE);
                      }, "multiOrder-isGreater");
                    }, "multiOrder-isLess");
                  }
                }), EQ, comps, 0);
              }, function(order) {
                if (order === LESS) { lessers.push(sourceArr[rowIdx]); }
                else if (order === EQ) { equals.push(sourceArr[rowIdx]); }
                else { greaters.push(sourceArr[rowIdx]); }
                return runtime.nothing;
              }, "multiOrder-temparrs");
            }), 1, sourceArr.length); // start from 1, since index 0 is the pivot
          }, function(_) {
            return runtime.safeCall(function() {
              if (lessers.length === 0) { return destArr; }
              else { return helper(lessers); }
            }, function(_) {
              for (var i = 0; i < equals.length; i++)
                destArr.push(equals[i].slice()); // need to copy here
              if (greaters.length === 0) { return destArr; }
              else { return helper(greaters); }
            }, "multiOrder-finalMoves");
          });
        }
        return helper(sourceArr);
      }

      function order(direction, colname) {
        var asList = runtime.ffi.makeList(rows);
        var index = headerIndex["column:" + colname];
        var comparator = direction ? runtime.lessthan : runtime.greaterthan;
        var compare = runtime.makeFunction(function(l, r) {
          return comparator(l[index], r[index]);
        });
        var equal = runtime.makeFunction(function(l, r) {
          return runtime.equal_always(l[index], r[index]);
        });
        return runtime.safeCall(function() {
          return runtime.getField(asList, "sort-by").app(compare, equal);
        }, function(sortedList) {
          return makeTable(headers, runtime.ffi.toArray(sortedList));
        });

      }

      function makeRowFromValues(vals) {
        if(headers.length !== vals.length) {
          throw runtime.ffi.throwRowLengthMismatch(makeTable(headers, []), vals);
        }
        return makeRow({ headerIndex: headerIndex }, vals);
      }

      return applyBrand(brandTable, runtime.makeObject({

        '_header-raw-array': headers,
        '_rows-raw-array': rows,

        'increasing-by': runtime.makeMethod1(function(_, colname) {
          ffi.checkArity(2, arguments, "increasing-by", true);
          runtime.checkString(colname);
          return order(true, colname);
        }),
        'decreasing-by': runtime.makeMethod1(function(_, colname) {
          ffi.checkArity(2, arguments, "decreasing-by", true);
          runtime.checkString(colname);
          return order(false, colname);
        }),
        'order-by': runtime.makeMethod2(function(_, colname, increasing) {
          ffi.checkArity(3, arguments, "order-by", true);
          runtime.checkString(colname);
          runtime.checkBoolean(increasing);
          return order(increasing, colname);
        }),
        'order-by-columns': runtime.makeMethod1(function(_, specs) {
          ffi.checkArity(2, arguments, "order-by-columns", true);
          runtime.checkList(specs);
          var specsArray = ffi.toArray(specs);
          var asArrays = [];
          for(var i = 0; i < specsArray.length; i += 1) {
            runtime.checkTuple(specsArray[i]);
            asArrays.push([
                runtime.getTuple(specsArray[i], 1),
                runtime.getTuple(specsArray[i], 0)
              ]);
          }
          return runtime.safeCall(function() {
            return multiOrder(rows, asArrays, []);
          }, function(destArr) {
            return makeTable(headers, destArr);
          }, "order-by-columns");
        }),

        'multi-order': runtime.makeMethod1(function(_, colComps) {
          // colComps is an array of 2-element arrays, [true iff ascending, colName]
          return runtime.safeCall(function() {
            return multiOrder(rows, colComps, []);
          }, function(destArr) {
            return makeTable(headers, destArr);
          }, "multi-order");
        }),

        'add-column': runtime.makeMethod2(function(_, colname, eltList) {
          ffi.checkArity(3, arguments, "add-column", true);
          runtime.checkString(colname);
          runtime.checkList(eltList);
          var asArray = runtime.ffi.toArray(eltList);
          if(rows.length !== asArray.length) {
            throw runtime.ffi.throwMessageException("column-length-mismatch");
          }

          var newRows = [];
          for(var i = 0; i < rows.length; i += 1) {
            newRows.push(rows[i].concat([asArray[i]]));
          }
          
          return makeTable(headers.concat([colname]), newRows);
        }),

        'add-row': runtime.makeMethod1(function(_, row) {
          ffi.checkArity(2, arguments, 'add-row', true);
          runtime.checkRow(row);
          var theseKeys = Object.keys(headerIndex);
          var rowKeys = Object.keys(row.$underlyingTable.headerIndex);
          if(theseKeys.length !== rowKeys.length) {
            throw runtime.ffi.throwMessageException("add-row-length");
          }
          for(var i = 0; i < theseKeys.length; i += 1) {
            if(headerIndex["column:" + headers[i]] !== 
                row.$underlyingTable.headerIndex["column:" + headers[i]]) {
              throw runtime.ffi.throwMessageException("row-name-mismatch: " + headers[i]);
            }
          }
          // NOTE(joe): Here we do not copy all the sub-arrays with the
          // existing data, we just copy the outer array containing them.
          // This relies on the assumption that we never mutate the
          // underlying arrays in tables, and avoids significant copying.
          // Note also that add-column doesn't get to do this sharing because
          // of the row-major organization of tables.
          var newRows = rows.concat([row.$rowData]);
          return makeTable(headers, newRows);
        }),

        'row-n': runtime.makeMethod1(function(_, row) {
          ffi.checkArity(2, arguments, "row-n", true);
          runtime.checkNumNonNegative(row);
          runtime.checkNumInteger(row);
          var rowFix = runtime.num_to_fixnum(row);
          if(rowFix >= rows.length) {
            throw runtime.ffi.throwMessageException("row-n-too-large");
          }
          return makeRow({ headerIndex: headerIndex }, rows[rowFix]);
        }),

        'all-rows': runtime.makeMethod0(function(_) {
          ffi.checkArity(1, arguments, "all-rows", true);
          return runtime.ffi.makeList(
            rows.map(function(r) {
              return makeRow({ headerIndex: headerIndex }, r);
            })
          );
        }),


        'column': runtime.makeMethod1(function(_, colname) {
          ffi.checkArity(2, arguments, "column", true);
          runtime.checkString(colname);
          var lookupName = "column:" + colname;
          if(!(lookupName in headerIndex)) {
            throw runtime.ffi.throwMessageException("no-such-column");
          }
          var results = rows.map(function(r) {
            return r[headerIndex[lookupName]];
          });
          return runtime.ffi.makeList(results);
        }),

        'column-n': runtime.makeMethod1(function(_, n) {
          ffi.checkArity(2, arguments, "column-n", true);
          runtime.checkNumNonNegative(n);
          runtime.checkNumInteger(n);
          var lookupIndex = runtime.num_to_fixnum(n);
          if(lookupIndex >= headers.length) {
            throw runtime.ffi.throwMessageException("column-n-too-large");
          }
          var results = rows.map(function(r) {
            return r[lookupIndex];
          });
          return runtime.ffi.makeList(results);
        }),

        'all-columns': runtime.makeMethod0(function(_) {
          ffi.checkArity(1, arguments, "all-columns", true);
          var collists = new Array(headers.length);
          for(var i = 0; i < headers.length; i += 1) {
            collists[i] = []; 
          }
          for(var i = 0; i < rows.length; i += 1) {
            for(var j = 0; j < headers.length; j += 1) {
              collists[j].push(rows[i][j]);
            }
          }
          return runtime.ffi.makeList(collists.map(runtime.ffi.makeList));
        }),

        'column-names': runtime.makeMethod0(function(_) {
          ffi.checkArity(1, arguments, "column-names", true);
          return runtime.ffi.makeList(headers);
        }),

        'stack': runtime.makeMethod1(function(_, otherTable) {
          var otherHeaders = runtime.getField(otherTable, "_header-raw-array");
          if(otherHeaders.length !== headers.length) {
            return ffi.throwMessageException("Tables have different column sizes in stack: " + headers.length + " " + otherHeaders.length);
          }
          var headersSorted = headers.slice(0, headers.length).sort();
          var otherHeadersSorted = otherHeaders.slice(0, headers.length).sort();
          headersSorted.forEach(function(h, i) {
            if(h !== otherHeadersSorted[i]) {
              return ffi.throwMessageException("The table to be stacked is missing column " + h);
            }
          });

          var newRows = runtime.getField(otherTable, "_rows-raw-array");
          newRows = newRows.map(function(row) {
            var rowAsRec = getRowContentAsRecordFromHeaders(otherHeaders, row);
            console.log(headers);
            var newRow = headers.map(function(h) {
              return rowAsRec[h];
            });
            return newRow;
          });
          return makeTable(headers, rows.concat(newRows));
        }),

        'reduce': runtime.makeMethod2(function(_, colname, reducer) {
          if(rows.length === 0) { ffi.throwMessageException("Reducing an empty table (column names were " + headers.join(", ") + ")"); }
          var column = getColumn(colname);
          return runtime.safeCall(function() {
            return runtime.safeCall(function() {
              return runtime.getField(reducer, "one").app(column[0]);
            }, function(one) {
              if(rows.length === 1) {
                return one;
              }
              else {
                var reduce = runtime.getField(reducer, "reduce");
                var reducerWrapped = runtime.makeFunction(function(acc, val, ix) {
                  return reduce.app(runtime.getTuple(acc, 0, ["tables"]), val);
                });
                return runtime.raw_array_fold(reducerWrapped, one, column.slice(1), 1);
              }
            });
          }, function(answerTuple) {
            return runtime.getTuple(answerTuple, 1, ["tables"]); 
          });
        }),

        'empty': runtime.makeMethod0(function(_) {
          return makeTable(headers, []);
        }),

        'drop': runtime.makeMethod1(function(_, colname) {
          var newHeaders = headers.filter(function(h) { return h !== colname; })
          var newRows = rows.map(function(rawRow) {
            return rawRow.filter(function(h, i) {
              return i !== headerIndex['column:' + colname];
            });
          });
          return makeTable(newHeaders, newRows);
        }),


        'build-column': runtime.makeMethod1(function(_, colname, func) {
          var wrappedFunc = function(rawRow) {
            return runtime.safeCall(function() {
              return func.app(getRowContentAsGetter(headers, rawRow));
            },
            function(newVal) {
              return rawRow.concat([newVal]);
            });
          };

          return runtime.safeCall(function() {
            return runtime.raw_array_map(runtime.makeFunction(wrappedFunc, "func"), rows);
          }, function(newRows) {
            return makeTable(headers.concat([colname]), newRows);
          });
        }),

        'filter-by': runtime.makeMethod2(function(_, colname, pred) {
          ffi.checkArity(3, arguments, "filter-by", true);
          runtime.checkString(colname);
          runtime.checkFunction(pred);
          if(!(("column:" + colname) in headerIndex)) {
            throw runtime.ffi.throwMessageException("no-such-column");
          }
          var wrappedPred = function(rawRow) {
            return pred.app(getRowContentAsRecord(rawRow)[colname]);
          }
          return runtime.safeCall(function() {
            return runtime.raw_array_filter(runtime.makeFunction(wrappedPred, "pred"), rows);
          }, function(filteredRows) {
            return makeTable(headers, filteredRows);
          });
        }),


        'filter': runtime.makeMethod1(function(_, pred) {
          ffi.checkArity(2, arguments, "filter", true);
          runtime.checkFunction(pred);
          var wrappedPred = function(rawRow) {
            return pred.app(makeRow({ headerIndex: headerIndex }, rawRow));
          }
          return runtime.safeCall(function() {
            return runtime.raw_array_filter(runtime.makeFunction(wrappedPred, "pred"), rows);
          }, function(filteredRows) {
            return makeTable(headers, filteredRows);
          });
        }),

        'get-row': runtime.makeMethod1(function(_, row_index) {
          ffi.checkArity(2, arguments, "get-row", true);
          runtime.checkArrayIndex("get-row", rows, row_index);
          return getRowContentAsGetter(headers, rows[row_index]);
        }),
        
        'length': runtime.makeMethod0(function(_) {
          ffi.checkArity(1, arguments, "length", true);
          return runtime.makeNumber(rows.length);
        }),
        
        'get-column': runtime.makeMethod1(function(_, col_name) {
          ffi.checkArity(2, arguments, "get-column", true);
          runtime.checkString(col_name);
          if(!hasColumn(col_name)) {
            ffi.throwMessageException("The table does not have a column named `"+col_name+"`.");
          }
          return runtime.ffi.makeList(getColumn(col_name));
        }),

        'select-columns': runtime.makeMethod1(function(_, colnames) {
          ffi.checkArity(2, arguments, "get-column", true);
          runtime.checkList(colnames);

          var colnamesList = ffi.toArray(colnames);
          if(colnamesList.length === 0) {
            throw ffi.throwMessageException("zero-columns");
          }
          for(var i = 0; i < colnamesList.length; i += 1) {
            runtime.checkString(colnamesList[i]);
            if(!hasColumn(colnamesList[i])) {
              throw ffi.throwMessageException("no-such-column");
            }
          }

          var newRows = new Array(colnamesList.length);
          for(var i = 0; i < rows.length; i += 1) {
            newRows[i] = [];
            for(var j = 0; j < colnamesList.length; j += 1) {
              var colIndex = headerIndex['column:' + colnamesList[j]];
              collists[i].push(rows[i][colIndex]);
            }
          }
          return makeTable(colnamesList, collists);
        }),

        
        '_column-index': runtime.makeMethod3(function(_, operation_loc, table_loc, col_name, col_loc) {
          ffi.checkArity(5, arguments, "_column-index", true);
          var col_index = headerIndex['column:'+col_name];
          if(col_index === undefined) {
            ffi.throwColumnNotFound(operation_loc, col_name, col_loc,
              runtime.ffi.makeList(Object.keys(headerIndex).map(function(k) { return k.slice(7); })));
          }
          return col_index;
        }),
        
        '_no-column': runtime.makeMethod3(function(_, operation_loc, table_loc, col_name, col_loc) {
          ffi.checkArity(5, arguments, "_no-column", true);
          var col_index = headerIndex['column:'+col_name];
          if(col_index != undefined)
            ffi.throwDuplicateColumn(operation_loc, col_name, col_loc);
          return col_index;
        }),
        
        '_equals': runtime.makeMethod2(function(self, other, equals) {
          ffi.checkArity(3, arguments, "_equals", true);
          // is the other a table
          // same number of columns?
          // same number of rows?
          // columns have same names?
          // each row has the same elements
          if (!hasBrand(brandTable, other)) {
            return neq(self, other);
          }
          var otherHeaders = get(other, "_header-raw-array");
          var otherRows = get(other, "_rows-raw-array");
          if (headers.length !== otherHeaders.length
              || rows.length !== otherRows.length) {
            return neq(self, other);
          }
          for (var i = 0; i < headers.length; ++i) {
            if (headers[i] != otherHeaders[i]) {
              return neq(self, other);
            }
          }
          return runtime.raw_array_fold(runtime.makeFunction(function(ans, selfRow, i) {
            if (ffi.isNotEqual(ans)) { return ans; }
            var otherRow = otherRows[i];
            return runtime.raw_array_fold(runtime.makeFunction(function(ans, selfRowJ, j) {
              if (ffi.isNotEqual(ans)) { return ans; }
              return runtime.safeCall(function() {
                return equals.app(selfRowJ, otherRow[j]);
              }, function(eqAns) {
                return get(EQ, "equal-and").app(ans, eqAns);
              }, "equals:combine-cells");
            }), ans, selfRow, 0);
          }), eq(), rows, 0);
        }),
        
        '_output': runtime.makeMethod0(function(_) {
          ffi.checkArity(1, arguments, "_output", true);
          var vsValue = get(VS, "vs-value").app;
          var vsString = get(VS, "vs-str").app;
          return get(VS, "vs-table").app(
            headers.map(function(hdr){return vsString(hdr);}),
            rows.map(function(row){return row.map(
              function(elm){return vsValue(elm);});}));
        }),

        'row': runtime.makeMethodN(function(self, ...args) {
          if(headers.length !== args.length) {
            throw runtime.ffi.throwRowLengthMismatch(makeTable(headers, []), args);
          }
          return makeRow({ headerIndex: headerIndex }, args);
        }),

        'new-row': runtime.makeObject({
          make: runtime.makeFunction(function(ar) { return makeRowFromValues(ar); }),
          make0: runtime.makeFunction(function( ) { return makeRowFromValues([]); }),
          make1: runtime.makeFunction(function(v) { return makeRowFromValues([v]); }),
          make2: runtime.makeFunction(function(v1, v2) { return makeRowFromValues([v1, v2]); }),
          make3: runtime.makeFunction(function(v1, v2, v3) { return makeRowFromValues([v1, v2, v3]); }),
          make4: runtime.makeFunction(function(v1, v2, v3, v4) { return makeRowFromValues([v1, v2, v3, v4]); }),
          make5: runtime.makeFunction(function(v1, v2, v3, v4, v5) { return makeRowFromValues([v1, v2, v3, v4, v5]); })
        })
      }));
    }
    
    return runtime.makeJSModuleReturn({
        TableAnn : annTable,
        RowAnn : annRow,
        makeTable: makeTable,
        makeRow: makeRow,
        makeRowFromArray: makeRowFromArray,
        openTable: openTable,
        isTable: isTable,
        isRow: isRow
      },
      {});
  }
})
