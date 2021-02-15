/*
TODO:
In order of priority
Figure out mat-dim type issue
replace duplicate function,method for +,-,*
*/
({
    requires:
        [
            {"import-type": "builtin", name: "valueskeleton"},
            {"import-type": "builtin", name: "lists"},
            {"import-type": "builtin", name: "arrays"}
        ],
    nativeRequires: ["pyret-base/js/js-numbers"],
    provides: {
        shorthands: {
            "Matrix": ["tyapp", ["local", "Matrix"], []],
            "Vector": ["tyapp", ["local", "Vector"], []],
            "Equality": {
                tag: "name",
                origin: {"import-type": "uri", uri: "builtin://equality"},
                name: "EqualityResult"
            },
            "VS": {
                tag: "name",
                origin: {"import-type": "uri", uri: "builtin://valueskeleton"},
                name: "ValueSkeleton"
            },
            "List": {
                tag: "name",
                origin: {"import-type": "uri", uri: "builtin://list"},
                name: "List"
            },
            "Array": {
                tag: "name",
                origin: {"import-type": "uri", uri: "builtin://array"},
                name: "Array"
            },

            "tva": ["tid", "a"],
            "tvb": ["tid", "b"]
        },
        values: {
            "mat": ["arrow", ["Number", "Number"], ["Maker", "Any", ["local", "Matrix"]]],
            "vector": ["arrow", [], ["Maker", "Any", ["local", "Vector"]]],
            "add-mat": ["arrow", ["Matrix", "Matrix"], "Matrix"],
            "sub-mat": ["arrow", ["Matrix", "Matrix"], "Matrix"],
            "mult-mat": ["arrow", ["Matrix", "Matrix"], "Matrix"],
            "get-elem": ["arrow", ["Matrix", "Number", "Number"], "Number"],
            "transpose": ["arrow", ["Matrix"], "Matrix"],
            "stack-mat": ["arrow", ["Matrix", "Matrix"], "Matrix"],
            "scale": ["arrow", ["Matrix", "Number"], "Number"],
            "set-elem": ["arrow", ["Matrix", "Number", "Number", "Number"], "Matrix"],
            "reshape": ["arrow", ["Matrix", "Number", "Number"], "Matrix"],
            "matrix-map": ["arrow", ["Matrix", ["arrow", ["Number", "Number", "Number"], "Number"]], "Matrix"],
            "mat-within": ["arrow", ["Number", "Number", "Number", "Number"], "Matrix"],
            "mat-of": ["arrow", ["Number", "Number", "Number"], "Matrix"],
            "row-map": ["arrow", [["arrow", ["Vector"], "Vector"], "Matrix"], "Matrix"],
            "col-map": ["arrow", [["arrow", ["Vector"], "Vector"], "Matrix"], "Matrix"],
            /* "mat-dims" : ["arrow" ,[["Matrix"] , ["Lis t", "Number"]], "tva"]   */






            "get-row": ["arrow", ["Matrix", "Number"], "Vector"],
            "get-col": ["arrow", ["Matrix", "Number"], "Vector"], /*
      "determinant" : ["arrow", ["Matrix"] , "Number"] , 
      "frobenius-norm" : ["arrow", ["Matrix"] , "Number"] , 
      "norm" : ["arrow", ["Matrix" , "Number"] , "Number" ]   ,
      "inverse" : ["arrow", ["Matrix"] , "Matrix"] , 
      "exponent": ["arrow" ,["Matrix" , "Number"] , "Number" ]  ,*/
            "dot-product": ["arrow", ["Vector", "Vector"], "Number"],

            "vector-to-list": ["arrow", ["Vector"], "List"],
            "vector-to-array": ["arrow", ["Vector"], "Array"],

        },
        aliases: {
            "Matrix": {
                tag: "name",
                origin: {"import-type": "$ELF"},
                name: "Matrix"
            },
            "Vector": {
                tag: "name",
                origin: {"import-type": "$ELF"},
                name: "Vector"
            }
        },
        datatypes: {
            "Matrix": ["data", "Matrix", [], [], {
                "_output": ["arrow", [["arrow", ["Any"], "VS"]], "VS"],

                "_equals": ["arrow", ["Matrix", ["arrow", ["Any", "Any"], "Equality"]], "Equality"],

                "_plus": ["arrow", ["Matrix"], "Matrix"],
                "_minus": ["arrow", ["Matrix"], "Matrix"],
                "_times": ["arrow", ["Matrix"], "Matrix"]
            }],
            "Vector": ["data", "Vector", [], [], {
                "_output": ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
                "_equals": ["arrow", ["Vector", ["arrow", ["Any", "Any"], "Equality"]], "Equality"],
                "_plus": ["arrow", ["Vector"], "Vector"],
                "_minus": ["arrow", ["Vector"], "Vector"],
            }],
        }
    },
    theModule: function (runtime, namespace, uri, VSlib, LSlib, ARRLib, jsnum) {
        var O = runtime.makeObject;
        var F = runtime.makeFunction;
        var arity = runtime.checkArity;
        var get = runtime.getField;

        var VS = get(VSlib, "values");
        var LS = get(LSlib, "values");
        var ARR = get(ARRLib, "values");

        var brandMatrix = runtime.namedBrander("matrix", ["matrix: matrix brander"]);
        var brandVector = runtime.namedBrander("vector", ["vector: vector brander"]);

        var annMatrix = runtime.makeBranderAnn(brandMatrix, "Matrix");
        var annVector = runtime.makeBranderAnn(brandVector, "Vector");

        var checkMtrx = function (v) {
            runtime._checkAnn(['matrix'], annMatrix, v)
        };
        var checkVec = function (v) {
            runtime._checkAnn(['vector'], annVector, v)
        };

        function applyBrand(brand, val) {
            return get(brand, "brand").app(val);
        }

        function hasBrand(brand, val) {
            return get(brand, "test").app(val);
        }

        function internal_isMtrx(obj) {
            return hasBrand(brandMatrix, obj);
        }

        function internal_isVec(obj) {
            return hasBrand(brandVector, obj);
        }

        function pyretNumConv(num) {
            if (jsnum.isRoughnum(num)) {
                return jsnum.toRoughnum(num);
            } else {
                return jsnum.fromFixnum(num, runtime.NumberErrbacks);
            }
        }

        // Checks if same and other are matrices with the same dimensions
        function sameDims(self, other) {
            (checkMtrx(self) && checkMtrx(other));
            return (self.$h == other.$h) || (self.$w == other.$w)
        }

        //return dimensions of self formatted for printing
        function printDims(self) {
            checkMtrx(self);
            return "(" + self.$h + "," + self.$w + ")";
        }

        // given a matrix and a 2d position return the corresponding element in the matrix
        function get1d(mtrx, h, w) {
            checkMtrx(mtrx);
            return mtrx.$underlyingMat[(h * mtrx.$w) + w];
        }

        // Given a 2d array position and the no of cols in a matrix return a 1D array index
        function get1dpos(h, w, c) {
            return (h * c) + w;
        }

        //Checks if h,w are valid matrix dimensions
        function posInteger(h, w) {
            if ((h >= 0) && (w >= 0) && Number.isInteger(h) && Number.isInteger(w)) {
                return true
            } else {
                runtime.ffi.throwMessageException("Dimensions need to be positive integers");
            }
        }

        // checks if h,w are in range of the matrix dimensions
        function checkRange(mtrx, h, w) {
            if (!posInteger(h, w) || (h >= mtrx.$h) || (w >= mtrx.$w)) {
                runtime.ffi.throwMessageException("Given dimensions are not valid");
            }
            return true;
        }

        /* Duplicates a section of the matrix into the given array with offset
        * mtrx : matrix to copy from
        * start : start index in matrix (1D index)
        * end : end index in* matrix (1D index)
        * arr : Array to copy into
        * offset : Offset to use while copying into arr
        */
        function duplicateArray(mtrx, start, end, arr, offset) {
            len = end - start;
            for (var i = 0; i < len; i++) {
                arr[i + offset] = mtrx.$underlyingMat[start + i];
            }
            return arr;
        }

        // Duplicates self
        function duplicateMatrix(self) {
            new_arr = new Array(self.$l);
            duplicateArray(self, 0, self.$l, new_arr, 0);
            return createMatrixFromArray(self.$h, self.$w, new_arr);

        }

        // Adds self and other
        var funcaddMatrix = function (self, other) {
            runtime.ffi.checkArity(2, arguments, "add-mat", false);
            runtime.checkArgsInternal2("matrix", "add-mat", self, annMatrix, other, annMatrix);
            if (!sameDims(self, other)) {
                return runtime.ffi.throwMessageException("Matrices have dimensions " + printDims(self) + " and " + printDims(other) + " . They cannot be added");
            } else {
                new_arr = new Array(self.$l);
                for (var i = 0; i < self.$l; i++) {
                    new_arr[i] = runtime.plus(self.$underlyingMat[i], other.$underlyingMat[i]);
                }

                return createMatrixFromArray(self.$h, self.$w, new_arr);
            }
        };

        var funcsubMatrix = runtime.makeFunction(function (self, other) {
            runtime.ffi.checkArity(2, arguments, "sub-mat", false);
            runtime.checkArgsInternal2("matrix", "sub-mat", self, annMatrix, other, annMatrix);
            if (!sameDims(self, other)) {
                return runtime.ffi.throwMessageException("Matrices have dimensions " + printDims(self) + " and " + printDims(other) + " . They cannot be added");
            } else {
                new_arr = new Array(self.$l);
                for (var i = 0; i < self.$l; i++) {
                    new_arr[i] = runtime.minus(self.$underlyingMat[i], other.$underlyingMat[i]);
                }

                return createMatrixFromArray(self.$h, self.$w, new_arr);
            }
        }, "sub-mat");

        var funcmultMatrix = runtime.makeFunction(function (self, other) {
            runtime.ffi.checkArity(2, arguments, "mult-mat", false);
            runtime.checkArgsInternal2("Matrix", "mult-mat", self, annMatrix, other, annMatrix);
            if (self.$w != other.$h) {
                return runtime.ffi.throwMessageException("The width of the first matrix and the height of the second matrix need to be equal");

            } else {
                new_arr = new Array((self.$h * other.$w));
                for (var i = 0; i < self.$h; i++) {
                    for (var j = 0; j < other.$w; j++) {
                        var elm = 0;
                        for (var k = 0; k < self.$w; k++) {
                            elm += (runtime.times(get1d(self, i, k)), get1d(other, k, j));
                        }
                        new_arr[get1dpos(i, j, other.$w)] = runtime.makeNumber(elm);
                    }
                }
                return createMatrixFromArray(self.$h, other.$w, new_arr);
            }
        }, "mult-mat");

        var outputMatrix = runtime.makeMethod0(function (self) {
            //if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['_output'], 1, $a, true); }
            var rows = [];
            var matr = self.$underlyingMat;
            var vsValue = get(VS, "vs-value");
            for (var i = 0; i < matr.length; i++) {
                rows.push(vsValue.app(matr[i]));
            }
            return get(VS, "vs-collection").app(
                runtime.makeString("mat" + printDims(self)),
                runtime.ffi.makeList(rows))
        });


        var getMatrixDims = runtime.makeFunction(function (self) {
            runtime.ffi.checkArity(1, arguments, "mat-dims", false);
            runtime.checkArgsInternal1("Matrix", "mat-dims", self, annMatrix);
            return runtime.makeTuple([self.$h, self.$w]);
        }, "mat-dims");

        var getMatrixElms = runtime.makeFunction(function (self, h, c) {
            runtime.ffi.checkArity(3, arguments, "get-elem", false);
            runtime.checkArgsInternal3("Matrix", "get-elem", self, annMatrix, h, runtime.Number, c, runtime.Number);
            if (checkRange(self, h, c)) {
                return runtime.makeNumber(get1d(self, h, c));
            }

        }, "get-elem");

        var setMatrixElms = function (self, h, w, num) {
            arity(4, arguments, "set-elem", false);
            runtime.checkArgsInternalInline("Matrix", "set-elem", self, annMatrix, h, runtime.Number, w, runtime.Number, num, runtime.Number);
            if (checkRange(self, h, w)) {
                new_mtrx = duplicateMatrix(self);
                new_mtrx.$underlyingMat[get1dpos(h, w, self.$w)] = num;
                return new_mtrx;
            }

        }

        var reshapeMatrix = function (self, h, w) {
            arity(3, arguments, "reshape", false);
            runtime.checkArgsInternalInline("Matrix", "reshape", self, annMatrix, h, runtime.Number, w, runtime.Number);
            posInteger(h, w);
            if ((h * w) != self.$l) {
                runtime.ffi.throwMessageException("Given dimensions do not match the matrix");
            } else {
                mtrx = duplicateMatrix(self);
                mtrx.$h = h;
                mtrx.$w = w;
                return mtrx;
            }

        }
        var transposeMatrix = runtime.makeFunction(function (self) {
            runtime.ffi.checkArity(1, arguments, "transpose", false);
            runtime.checkArgsInternal1("Matrix", "transpose", self, annMatrix);
            new_arr = new Array(self.$l);
            for (var i = 0; i < self.$h; i++) {
                for (var j = 0; j < self.$w; j++) {
                    new_arr[get1dpos(j, i, self.$h)] = get1d(self, i, j);
                }
            }
            return createMatrixFromArray(self.$w, self.$h, new_arr);

        }, "transpose");

        var stackMatrix = runtime.makeFunction(function (self, other) {
            runtime.ffi.checkArity(2, arguments, "stack-mat", false);
            runtime.checkArgsInternal2("Matrix", "stack-mat", self, annMatrix, other, annMatrix);
            if (self.$w != other.$w) {
                runtime.ffi.throwMessageException("Matrices need to have same width to be stacked ")
            } else {
                new_arr = new Array(self.$l + other.$l);
                duplicateArray(self, 0, self.$l, new_arr, 0);
                duplicateArray(other, 0, other.$l, new_arr, self.$l);
                return createMatrixFromArray(self.$h + other.$h, self.$w, new_arr);
            }
        }, "stack-mat");

        var scaleMatrix = runtime.makeFunction(function (self, num) {
            runtime.ffi.checkArity(2, arguments, "scale", false);
            runtime.checkArgsInternal2("Matrix", "scale", self, annMatrix, num, runtime.Number);
            new_mtrx = duplicateMatrix(self);
            for (var i = 0; i < new_mtrx.$l; i++) {
                new_mtrx.$underlyingMat[i] = runtime.times(new_mtrx.$underlyingMat[i], num);
            }
            return new_mtrx;
        }, "scale");

        var mapMatrix = function (self, f) {
            arity(2, arguments, "matrix-map", false);
            runtime.checkArgsInternalInline("Matrix", "matrix-map", self, annMatrix, f, runtime.Function);
            new_mtrx = duplicateMatrix(self);
            for (var i = 0; i < new_mtrx.$l; i++) {
                var h = Math.floor(i / new_mtrx.$h);
                var w = i % new_mtrx.$w;
                new_mtrx.$underlyingMat[i] = f.app(h, w, new_mtrx.$underlyingMat[i]);

            }
            return new_mtrx;
        }

        var matrixWithin = function (self, n1, n2, n3, n4) {
            arity(5, arguments, "mat-within", false);
            runtime.checkArgsInternalInline("Matrix", "mat-within", self, annMatrix,
                n1, runtime.Number, n2, runtime.Number, n3, runtime.Number, n4, runtime.Number);
            start_pos = get1dpos(n1, n2, self.$w)
            end_pos = get1dpos(n3, n4, self.$w);
            if (checkRange(self, n1, n2) && checkRange(self, n3, n4)) {
                if ((end_pos - start_pos) % 2 != 0) {
                    return runtime.ffi.throwMessageException("Given dimensions encompass an uneven number of elements");
                } else {

                    new_arr = new Array(end_pos - start_pos);
                    duplicateArray(self, start_pos, end_pos + 1, new_arr, 0);
                    return createMatrixFromArray((n3 - n1 + 1), (n4 - n2 + 1), new_arr);
                }
            } else {
                return runtime.ffi.throwMessageException("Invalid dimensions");
            }
        }
        var matOf = function (h, w, n) {
            arity(3, arguments, "mat-of", false);
            runtime.checkArgsInternalInline("Matrix", "mat-of", h, runtime.Number, w, runtime.Number, n, runtime.Number);
            return createMatrixFromArray(h, w, new Array(h * w).fill(n));
        }

        function makeMatrix(h, w, underlyingMat) {
            var equalMatrix = runtime.makeMethod2(function (self, other, Eq) {
                runtime.ffi.checkArity(3, arguments, "_equals", true);
                runtime.checkArgsInternal3("matrix", "_equals", self, annMatrix, other, annMatrix, Eq, runtime.Function);

                if (!hasBrand(brandMatrix, other)) {
                    return runtime.ffi.notEqual.app('', self, other);
                } else if (!sameDims(self, other)) {
                    return runtime.ffi.notEqual.app('', self, other);
                } else {
                    for (var i = 0; i < self.$l; i++) {
                        if (jsnum.isRoughnum(self.$underlyingMat[i]) || jsnum.isRoughnum(other.$underlyingMat[i])) {
                            return runtime.ffi.throwMessageException("The matrix consists of rough nums and cannot be checked for equality");
                        }
                        if (!jsnum.equals(self.$underlyingMat[i], other.$underlyingMat[i], runtime.NumberErrbacks)) {
                            return runtime.ffi.notEqual.app('', self, other);
                        }
                    }
                    return runtime.ffi.equal;

                }
            }, "equals");
            var addMatrix = runtime.makeMethod1(function (self, other) {
                runtime.ffi.checkArity(2, arguments, "_plus", true);
                runtime.checkArgsInternal2("matrix", "_plus", self, annMatrix, other, annMatrix);
                if (!sameDims(self, other)) {
                    return runtime.ffi.throwMessageException("Matrices have dimensions " + printDims(self) + " and " + printDims(other) + " . They cannot be added");
                } else {
                    new_arr = new Array(self.$l);
                    for (var i = 0; i < self.$l; i++) {
                        new_arr[i] = runtime.plus(self.$underlyingMat[i], other.$underlyingMat[i]);
                    }
                    return createMatrixFromArray(self.$h, self.$w, new_arr);
                }
            }, "plus");
            var minusMatrix = runtime.makeMethod1(function (self, other) {
                runtime.ffi.checkArity(2, arguments, "_minus", true);
                runtime.checkArgsInternal2("matrix", "_minus", self, annMatrix, other, annMatrix);
                if (!sameDims(self, other)) {
                    return runtime.ffi.throwMessageException("Matrices have dimensions " + printDims(self) + " and " + printDims(other) + " . They cannot be added");
                } else {
                    new_arr = new Array(self.$l);
                    for (var i = 0; i < self.$l; i++) {
                        new_arr[i] = runtime.minus(self.$underlyingMat[i], other.$underlyingMat[i]);
                    }

                    return createMatrixFromArray(self.$h, self.$w, new_arr);
                }
            }, "minus");
            var timesMatrix = runtime.makeMethod1(function (self, other) {
                runtime.ffi.checkArity(2, arguments, "_times", true);
                runtime.checkArgsInternal2("Matrix", "_times", self, annMatrix, other, annMatrix);
                if (self.$w != other.$h) {
                    return runtime.ffi.throwMessageException("The width of the first matrix and the height of the second matrix need to be equal");

                } else {
                    new_arr = new Array((self.$h * other.$w));
                    for (var i = 0; i < self.$h; i++) {
                        for (var j = 0; j < other.$w; j++) {
                            var elm = 0;
                            for (var k = 0; k < self.$w; k++) {
                                elm += runtime.times(get1d(self, i, k), get1d(other, k, j));
                            }
                            new_arr[get1dpos(i, j, other.$w)] = elm;
                        }
                    }
                    return createMatrixFromArray(self.$h, other.$w, new_arr);
                }
            }, "times");
            var get_height = runtime.makeMethod0(function (self) {
                return self.$h
            }, "get-height");
            var get_width = runtime.makeMethod0(function (self) {
                return self.$w
            }, "get-width");
            var get_shape = runtime.makeMethod0(function (self) {
                return runtime.makeTuple([self.$h, self.$w])
            }, "get-shape");
            var get_elem = runtime.makeMethod3(function (self, n1, n2) {
                getMatrixElms(self, n1, n2);
            })
            var obj = O({
                _output: outputMatrix,
                _plus: addMatrix,
                _minus: minusMatrix,
                _equals: equalMatrix,
                _times: timesMatrix,
                "get-height": get_height,
                "get-width": get_width,
                "get-shape": get_shape,
                "get-elem": get_elem
            });
            // Applying a brand creates a new object, so we need to add the reflective field afterward
            obj = applyBrand(brandMatrix, obj);
            obj.$underlyingMat = underlyingMat;
            obj.$h = h;
            obj.$w = w;
            obj.$l = h * w;

            return obj;
        }

        var vectorDotP = function (self, other) {
            arity(2, arguments, "dot-product", false);
            if (!(self.$l == other.$l)) {
                return runtime.ffi.throwMessageException("Vectors have dimensions " + self.$l + " and " + other.$l + " . They cannot be multiplied.");
            } else {
                new_arr = new Array(self.$l);
                for (var i = 0; i < self.$l; i++) {
                    new_arr[i] = runtime.times(self.$underlyingMat[i], other.$underlyingMat[i]);
                }
                return makeVector(new_arr);
            }
        }
        var getMatrixRow = function (matr, row) {
            arity(2, arguments, "get-row", false);
            if (row < 0 || row >= matr.$h || !(Number.isInteger(row))) {
                return runtime.ffi.throwMessageException("Invalid row number.");
            } else {
                var retRow = [];
                for (var i = 0; i < matr.$h; i++) {
                    if (i == row) {
                        for (var j = 0; j < matr.$w; j++) {
                            retRow.push(matr.$underlyingMat[(i * matr.$w) + j]);
                        }
                    }
                }
                return makeVector(runtime.makeArray(retRow));
            }
        }
        var getMatrixCol = function (matr, col) {
            arity(2, arguments, "get-col", false);
            if (col < 0 || col >= matr.$w || !(Number.isInteger(col))) {
                return runtime.ffi.throwMessageException("Invalid column number.");
            } else {
                var retCol = [];
                for (var i = 0; i < matr.$h; i++) {
                    for (var j = 0; j < matr.$w; j++) {
                        if (j == col) {
                            retCol.push(matr.$underlyingMat[(i * matr.$w) + j]);
                        }
                    }
                }
                return makeVector(runtime.makeArray(retCol));
            }
        }
        var rowMap = function (self, f) {
            arity(2, arguments, "row-map", false);
            runtime.checkArgsInternalInline("Matrix", "row-map", self, annMatrix, f, runtime.Function);
            new_mtrx = duplicateMatrix(self);
            for (var i = 0; i < new_mtrx.$h; i++) {
                var oldRow = getMatrixRow(new_mtrx, i);
                var newRow = f.app(oldRow);
                for (var j = 0; j < new_mtrx.$w; j++) {
                    new_mtrx.$underlyingMat[(i * new_mtrx.$w) + j] = newRow.$underlyingMat[j];
                }
            }
            return new_mtrx;
        }
        var colMap = function (self, f) {
            arity(2, arguments, "col-map", false);
            runtime.checkArgsInternalInline("Matrix", "col-map", self, annMatrix, f, runtime.Function);
            new_mtrx = duplicateMatrix(self);
            for (var j = 0; j < new_mtrx.$w; j++) {
                var oldCol = getMatrixCol(new_mtrx, j);
                var newCol = f.app(oldCol);
                for (var i = 0; i < new_mtrx.$h; i++) {
                    new_mtrx.$underlyingMat[(i * new_mtrx.$w) + j] = newCol.$underlyingMat[i];
                }
            }
            return new_mtrx;
        }
        var vectorToList = function (vect) {
            arity(1, arguments, "vector-to-list", false);
            return runtime.ffi.makeList(vect.$underlyingMat.map(function (item) {
                return runtime.makeNumber(item);
            }));
        }
        var vectorToArray = function (vect) {
            arity(1, arguments, "vector-to-array", false);
            return runtime.makeArray(vect.$underlyingMat.map(function (item) {
                return runtime.makeNumber(item);
            }));
        }
        var outputVector = runtime.makeMethod0(function (self) {
            arity(1, arguments, "_output", false);
            var rows = [];
            var matr = self.$underlyingMat;
            var vsValue = get(VS, "vs-value");
            for (var i = 0; i < matr.length; i++) {
                rows.push(vsValue.app(matr[i]));
            }
            return get(VS, "vs-collection").app(
                runtime.makeString("vector(" + self.$l + "):"),
                runtime.ffi.makeList(rows));
        });

        function makeVector(underlyingArr) {
            var equalVector = runtime.makeMethod2(function (self, other, recEq) {
                runtime.ffi.checkArity(3, arguments, "_equals", true);
                if (!hasBrand(brandVector, other)) {
                    return runtime.ffi.notEqual.app('', self, other);
                } else if (!(self.$l == other.$l)) {
                    return runtime.ffi.notEqual.app('', self, other);
                } else {
                    for (var i = 0; i < self.$l; i++) {
                        if (jsnum.isRoughnum(self.$underlyingMat[i]) || jsnum.isRoughnum(other.$underlyingMat[i])) {
                            return runtime.ffi.throwMessageException("Vector consists of Roughnums which cannot be checked for equality");
                        }
                        if (!jsnum.equals(self.$underlyingMat[i], other.$underlyingMat[i], runtime.NumberErrbacks)) {
                            return runtime.ffi.notEqual.app('', self, other);
                        }
                    }
                    return runtime.ffi.equal;

                }
            });
            var addVector = runtime.makeMethod1(function (self, other) {
                runtime.ffi.checkArity(2, arguments, "_plus", true);
                if (!(self.$l == other.$l)) {
                    return runtime.ffi.throwMessageException("Vectors have dimensions " + self.$l + " and " + other.$l + " . They cannot be added.");
                } else {
                    new_arr = new Array(self.$l);
                    for (var i = 0; i < self.$l; i++) {
                        new_arr[i] = runtime.plus(self.$underlyingMat[i], other.$underlyingMat[i]);
                    }

                    return makeVector(new_arr);
                }
            });
            var minusVector = runtime.makeMethod1(function (self, other) {
                runtime.ffi.checkArity(2, arguments, "_minus", true);
                if (!(self.$l == other.$l)) {
                    return runtime.ffi.throwMessageException("Vectors have dimensions " + self.$l + " and " + other.$l + " . They cannot be subtracted.");
                } else {
                    new_arr = new Array(self.$l);
                    for (var i = 0; i < self.$l; i++) {
                        new_arr[i] = runtime.minus(self.$underlyingMat[i], other.$underlyingMat[i]);
                    }

                    return makeVector(new_arr);
                }
            });
            var multVector = runtime.makeMethod1(function (self, other) {
                runtime.ffi.checkArity(2, arguments, "_times", true);
                if (!(self.$l == other.$l)) {
                    return runtime.ffi.throwMessageException("Vectors have dimensions " + self.$l + " and " + other.$l + " . They cannot be multiplied.");
                } else {
                    new_arr = new Array(self.$l);
                    for (var i = 0; i < self.$l; i++) {
                        new_arr[i] = runtime.times(self.$underlyingMat[i], other.$underlyingMat[i]);
                    }

                    return makeVector(new_arr);
                }
            });
            var obj = O({
                _output: outputVector,
                _equals: equalVector,
                _plus: addVector,
                _minus: minusVector,
                _times: multVector
            });
            obj = applyBrand(brandVector, obj);
            obj.$underlyingMat = underlyingArr;
            obj.$l = underlyingArr.length;
            return obj;
        }

        function createMatrixFromArray(h, w, array) {
            arity(3, arguments, "matrix", false);
            runtime.checkArray(array);
            var len = array.length;
            if (h * w != len) {
                runtime.ffi.throwMessageException("The number of provided elements does not match the given width and height.");
            }
            var matr = new Array(array.length)
            for (var i = 0; i < matr.length; i++) {
                if (jsnum.isPyretNumber(array[i])) {
                    matr[i] = array[i];
                } else {
                    runtime.ffi.throwMessageException("A Matrix can only consist of numbers");
                }
            }
            return makeMatrix(h, w, matr);
        }

        function createVectorFromArray(arr) {
            arity(1, arguments, "vector", false);
            runtime.checkArray(arr);
            var copyArr = new Array(arr.length());
            for (var i = 0; i < copyArr; i++) {
                if (jsnum.isPyretNumber(arr[i])) {
                    copyArr[i] = arr[i];
                } else {
                    runtime.ffi.throwMessageException("A Vector can only consist of numbers");
                }
            }
            return makeVector(copyArr);

        }

        function matrixInit(h, w) {
            if (!(Number.isInteger(h)) || !(Number.isInteger(w)) || h < 0 || w < 0) {
                runtime.ffi.throwMessageException("The provided width or height is invalid. Matrix dimensions need to be a positive non zero integer");
            }
            return O({
                make: F((arr) => {
                    return createMatrixFromArray(h, w, arr)
                }, "matrix:make"),
                make0: F(() => {
                    return createMatrixFromArray(h, w, runtime.makeArray([]))
                }, "matrix:make0"),
                make1: F((a) => {
                    return createMatrixFromArray(h, w, runtime.makeArray([a]))
                }, "matrix:make1"),
                make2: F((a, b) => {
                    return createMatrixFromArray(h, w, runtime.makeArray([a, b]))
                }, "matrix:make2"),
                make3: F((a, b, c) => {
                    return createMatrixFromArray(h, w, runtime.makeArray([a, b, c]))
                }, "matrix:make3"),
                make4: F((a, b, c, d) => {
                    return createMatrixFromArray(h, w, runtime.makeArray([a, b, c, d]))
                }, "matrix:make4"),
                make5: F((a, b, c, d, e) => {
                    return createMatrixFromArray(h, w, runtime.makeArray([a, b, c, d, e]))
                }, "matrix:make5")
            });
        }

        var jsCheckMtrx = runtime.makeCheckType(internal_isMtrx, "Matrix");
        var jsCheckVec = runtime.makeCheckType(internal_isVec, "Vector");
        var vals = {
            "mat": F(matrixInit, "mat"),
            "vector": O({
                make: F((arr) => {
                    return createVectorFromArray(arr)
                }, "vector:make"),
                make0: F(() => {
                    return createVectorFromArray(runtime.makeArray([]))
                }, "vector:make0"),
                make1: F((a) => {
                    return createVectorFromArray(runtime.makeArray([a]))
                }, "vector:make1"),
                make2: F((a, b) => {
                    return createVectorFromArray(runtime.makeArray([a, b]))
                }, "vector:make2"),
                make3: F((a, b, c) => {
                    return createVectorFromArray(runtime.makeArray([a, b, c]))
                }, "vector:make3"),
                make4: F((a, b, c, d) => {
                    return createVectorFromArray(runtime.makeArray([a, b, c, d]))
                }, "vector:make4"),
                make5: F((a, b, c, d, e) => {
                    return createVectorFromArray(runtime.makeArray([a, b, c, d, e]))
                }, "vector:make5"),
            }),
            "add-mat": F(funcaddMatrix, "add-mat"),
            "sub-mat": funcsubMatrix,
            "mult-mat": funcmultMatrix,
            "get-elem": getMatrixElms,
            "transpose": transposeMatrix,
            "stack-mat": stackMatrix,
            "scale": scaleMatrix,
            "set-elem": F(setMatrixElms, "set-elem"),
            "reshape": F(reshapeMatrix, "reshape"),
            "matrix-map": F(mapMatrix, "matrix-map"),
            "mat-within": F(matrixWithin, "mat-within"),
            "mat-of": F(matOf, "mat-of"),
            "dot-product": F(vectorDotP, "dot-product"),
            "row-map": F(rowMap, "row-map"),
            "col-map": F(colMap, "col-map"),
            "get-row": F(getMatrixRow, "get-row"),
            "get-col": F(getMatrixCol, "get-col"),
            "vector-to-list": F(vectorToList, "vector-to-list"),
            "vector-to-array": F(vectorToArray, "vector-to-array"),

            //  "mat-dims" : getMatrixDims
        }
        var types = {
            Matrix: annMatrix,
            Vector: annVector
        }
        var internal = {
            checkMtrx: jsCheckMtrx,
            checkVec: jsCheckVec
        }

        return runtime.makeModuleReturn(vals, types, internal);
    }
})