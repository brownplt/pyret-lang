/*
* Written by Anirudh Narsipur and William Sun
* For any queries contact Anirudh Narsipur
* A lightweight matrix library meant for linear algebra and matrix operations in general
* Uses a 1D JS Array as the base representation
* */
({
    requires:
        [
            { "import-type": "builtin", name: "valueskeleton" },
            { "import-type": "builtin", name: "lists" },
            { "import-type": "builtin", name: "arrays" }
        ],
    nativeRequires: ["pyret-base/js/js-numbers"],
    provides: {
        shorthands: {
            "Matrix": ["tyapp", ["local", "Matrix"], []],
            "Vector": ["tyapp", ["local", "Vector"], []],
            "Equality": {
                tag: "name",
                origin: { "import-type": "uri", uri: "builtin://equality" },
                name: "EqualityResult"
            },
            "VS": {
                tag: "name",
                origin: { "import-type": "uri", uri: "builtin://valueskeleton" },
                name: "ValueSkeleton"
            },
            "List": {
                tag: "name",
                origin: { "import-type": "uri", uri: "builtin://list" },
                name: "List"
            },
            "Array": {
                tag: "name",
                origin: { "import-type": "uri", uri: "builtin://array" },
                name: "Array"
            },
            "Tuple": {
                tag: "name",
                origin: { "import-type": "uri", uri: "builtin://tuple" },
                name: "Tuple"
            },

            "tva": ["tid", "a"],
            "tvb": ["tid", "b"]
        },
        values: {
            "mat": ["arrow", ["Number", "Number"], ["Maker", "Any", ["local", "Matrix"]]],
            "vector": ["arrow", [], ["Maker", "Any", ["local", "Vector"]]],
            "is-mat": ["arrow", ["Any"], "Boolean"],
            "is-vector": ["arrow", ["Any"], "Boolean"],
            "is-row-mat": ["arrow", ["Matrix"], "Boolean"],
            "is-col-mat": ["arrow", ["Matrix"], "Boolean"],
            "is-square-mat": ["arrow", ["Matrix"], "Boolean"],
            "identity-mat": ["arrow", ["Number"], "Matrix"],
            "add-mat": ["arrow", ["Matrix", "Matrix"], "Matrix"],
            "sub-mat": ["arrow", ["Matrix", "Matrix"], "Matrix"],
            "mult-mat": ["arrow", ["Matrix", "Matrix"], "Matrix"],
            "trace-mat": ["arrow", ["Matrix"], "Number"],
            "rref-mat": ["arrow", ["Matrix"], "Matrix"],
            "augment-mat": ["arrow", ["Matrix", "Matrix"], "Matrix"],
            "get-elem": ["arrow", ["Matrix", "Number", "Number"], "Number"],
            "transpose": ["arrow", ["Matrix"], "Matrix"],
            "stack-mat": ["arrow", ["Matrix", "Matrix"], "Matrix"],
            "scale-mat": ["arrow", ["Matrix", "Number"], "Matrix"],
            "set-elem": ["arrow", ["Matrix", "Number", "Number", "Number"], "Matrix"],
            "reshape": ["arrow", ["Matrix", "Number", "Number"], "Matrix"],
            "map-mat": ["arrow", ["Matrix", ["arrow", ["Number", "Number", "Number"], "Number"]], "Matrix"],
            "submatrix": ["arrow", ["Number", "Number", "Number", "Number"], "Matrix"],
            "fill-mat": ["arrow", ["Number", "Number", "Number"], "Matrix"],

            "get-row": ["arrow", ["Matrix", "Number"], "Vector"],
            "get-col": ["arrow", ["Matrix", "Number"], "Vector"],
            "lup-mat": ["arrow", ["Matrix"], "Tuple"],
            "determinant": ["arrow", ["Matrix"], "Number"],
            "is-invertible": ["arrow", ["Matrix"], "Boolean"],
            "inverse": ["arrow", ["Matrix"], "Matrix"],
            "solve-mat": ["arrow", ["Matrix", "Matrix"], "Matrix"],
            "least-squares": ["arrow", ["Matrix", "Matrix"], "Matrix"],
            "gram-schmidt": ["arrow", ["Matrix"], "Matrix"],
            "qr-mat": ["arrow", ["Matrix"], "Tuple"],
            "norm-mat": ["arrow", ["Matrix", "Number"], "Number"],
            "frob-norm": ["arrow", ["Matrix"], "Number"],
            "exp-mat": ["arrow", ["Matrix", "Number"], "Number"],
            "vdot": ["arrow", ["Vector", "Vector"], "Number"],
            "magnitude": ["arrow", ["Vector"], "Number"],
            "vscale": ["arrow", ["Vector", "Number"], "Vector"],
            "normalize": ["arrow", ["Vector"], "Vector"],
            "vector-to-list": ["arrow", ["Vector"], "List"],
            "vector-to-array": ["arrow", ["Vector"], "Array"],
            "dims-mat": ["arrow", ["Matrix"], "Tuple"],
            "build-mat": ["arrow", ["Number", "Number", ["arrow", ["Number", "Number"], "Number"]], "Matrix"]

        },
        aliases: {
            "Matrix": {
                tag: "name",
                origin: { "import-type": "$ELF" },
                name: "Matrix"
            },
            "Vector": {
                tag: "name",
                origin: { "import-type": "$ELF" },
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
        // Pyret internals : refer to Pyret module representation and pyret docs
        var O = runtime.makeObject;
        var F = runtime.makeFunction;
        var arity = runtime.checkArity;
        var get = runtime.getField;

        var VS = get(VSlib, "values");
        var LS = get(LSlib, "values");
        var ARR = get(ARRLib, "values");

        //branders
        var brandMatrix = runtime.namedBrander("matrix", ["matrix: matrix brander"]);
        var brandVector = runtime.namedBrander("vector", ["vector: vector brander"]);

        var annMatrix = runtime.makeBranderAnn(brandMatrix, "Matrix");
        var annVector = runtime.makeBranderAnn(brandVector, "Vector");

        //type check functions
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

        //Code from here is unique to the library


        /**
         * Checks if same and other are matrices with the same dimensions
         * @param {Matrix} self 
         * @param {Matrix} other 
         * @returns Boolean
         */
        function sameDims(self, other) {
            (checkMtrx(self) && checkMtrx(other));
            return (self.$h == other.$h) || (self.$w == other.$w)
        }


        /**
         * return dimensions of self formatted for printing
         * @param {Matrix} self 
         * @returns String
         */
        function printDims(self) {
            checkMtrx(self);
            return "(" + self.$h + "," + self.$w + ")";
        }

        /**
         * @param(Matrix) mtrx : Matrix
         * @param(Number) h : row index
         * @param(Number) w : col index
         * given a matrix and a 2d position return the corresponding element in the matrix
         * 
         */
        function get1dElem(mtrx, h, w) {
            checkMtrx(mtrx);
            return mtrx.$underlyingMat[(h * mtrx.$w) + w];
        }

        /**
         * Given a 2d array position and the no of cols in a matrix return a 1D array index
        * @param(Number) h : Integer , Row Wanted
        * @param(Number) w : Integer , Col wanted
        * @param(Number) c : Integer , No of cols in underlying matrix
         */
        function get1dpos(h, w, c) {
            return (h * c) + w;
        }
        /**
         * Set value at (i,j) to v in matrix self
         * @param {Matrix} self 
         * @param {Number} i 
         * @param {Number} j 
         * @param {Number} v 
         */
        function set1dElem(self, i, j, v) {
            self.$underlyingMat[(i * self.$w) + j] = v;
        }

        /**
         * Checks if h,w are valid matrix dimensions
         * @param {Number} h 
         * @param {Number} w 
         * @returns Boolean
         */
        function posInteger(h, w = 1) {
            if ((h >= 0) && (w >= 0) && Number.isInteger(h) && Number.isInteger(w)) {
                return true
            } else {
                runtime.ffi.throwMessageException("Dimensions need to be positive integers");
            }
        }

        /**
         * checks if h,w are in range of the matrix dimensions
         * @param {Matrix} mtrx 
         * @param {Number} h 
         * @param {Number} w 
         * @returns 
         */
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
        function duplicateArray(mtrx, start, end, arr, offset = 0) {
            len = end - start;
            for (var i = 0; i < len; i++) {
                arr[i + offset] = mtrx.$underlyingMat[start + i];
            }
            return arr;
        }

        // Duplicates self
        function duplicateMatrix(self) {
            new_arr = [...self.$underlyingMat];
            var ans = createMatrixFromArray(self.$h, self.$w, new_arr);
            return ans;

        }

        //Applies a function to the given row 
        function internalRowMap(self, row, f) {
            const startpos = get1dElem(self, row, 0);
            for (var i = startpos; i < startpos + self.$w; i++) {
                toret.$underlyingMat[startpos] = f(toret.$underlyingMat[startpos]);
            }
        }
        //Applies a function to the given col
        function internalColMap(self, col, f) {
            const startpos = get1dElem(self, 0, col);
            for (var i = 0; i < self.$h; i++) {
                toret.$underlyingMat[get1dElem(self, i, col)] = f(toret.$underlyingMat[get1dElem(self, i, col)]);
            }
        }

        // Adds self and other
        var addMatrix = function (self, other) {
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
        //subtract matrix
        var subMatrix = function (self, other) {
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
        }

        //multiply matrix
        var multMatrix = function (self, other) {
            runtime.ffi.checkArity(2, arguments, "mult-mat", false);
            runtime.checkArgsInternal2("Matrix", "mult-mat", self, annMatrix, other, annMatrix);
            if (self.$w != other.$h) {
                return runtime.ffi.throwMessageException("The width of the first matrix and the height of the second matrix need to be equal");

            } else {
                new_arr = new Array((self.$h * other.$w));
                return runtime.safeCall(
                    function () {
                        return runtime.raw_array_build(
                            F(function (idx) {
                                i = (idx - (idx % other.$w)) / other.$w;
                                j = idx % other.$w;
                                var elm = runtime.makeNumber(0);
                                for (var k = 0; k < self.$w; k++) {
                                    elm = runtime.plus(elm, runtime.times(get1dElem(self, i, k), get1dElem(other, k, j)));
                                }
                                return elm;
                            }), self.$h * other.$w)
                    }, function (result) {
                        return makeMatrix(self.$h, other.$w, result);
                    }
                );
            }
        }
        //Output given matrix
        var outputMatrix = runtime.makeMethod0(function (self) {
            arity(1, arguments, "_output", false);
            runtime.checkArgsInternalInline("Matrix", "_output", self, annMatrix);
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

        //Get matrix dimensions as a tuple 
        var getMatrixDims = function (self) {
            runtime.ffi.checkArity(1, arguments, "dims-mat", false);
            runtime.checkArgsInternal1("Matrix", "dims-mat", self, annMatrix);
            return runtime.makeTuple([self.$h, self.$w]);
        }

        //get element at (h,c)
        var getMatrixElms = function (self, h, w) {
            runtime.ffi.checkArity(3, arguments, "get-elem", false);
            runtime.checkArgsInternal3("Matrix", "get-elem", self, annMatrix, h, runtime.Number, w, runtime.Number);
            if (checkRange(self, h, w)) {
                return get1dElem(self, h, w);
            }

        }

        //returns new matrix with (h,w ) set to num
        var setMatrixElms = function (self, h, w, num) {
            arity(4, arguments, "set-elem", false);
            runtime.checkArgsInternalInline("Matrix", "set-elem", self, annMatrix, h, runtime.Number, w, runtime.Number, num, runtime.Number);
            if (checkRange(self, h, w)) {
                new_mtrx = duplicateMatrix(self);
                new_mtrx.$underlyingMat[get1dpos(h, w, self.$w)] = num;
                return new_mtrx;
            }

        }
        //reshapes matrix to (h,w) if possible
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
        //Transpose of matrix 
        var transposeMatrix = function (self) {
            runtime.ffi.checkArity(1, arguments, "transpose", false);
            runtime.checkArgsInternal1("Matrix", "transpose", self, annMatrix);
            new_arr = new Array(self.$l);
            for (var i = 0; i < self.$h; i++) {
                for (var j = 0; j < self.$w; j++) {
                    new_arr[get1dpos(j, i, self.$h)] = get1dElem(self, i, j);
                }
            }
            return createMatrixFromArray(self.$w, self.$h, new_arr);

        }
        //stacks matrices vertically
        var stackMatrix = function (self, other) {
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
        }

        //multiplies each element in matrix by num 
        var scaleMatrix = function (self, num) {
            runtime.ffi.checkArity(2, arguments, "scale-mat", false);
            runtime.checkArgsInternal2("Matrix", "scale-mat", self, annMatrix, num, runtime.Number);
            new_mtrx = duplicateMatrix(self);
            new_mtrx.$underlyingMat = new_mtrx.$underlyingMat.map(v => runtime.times(v, num));
            return new_mtrx;
        }

        //Applies f to each element in the matrix
        var mapMatrix = function (self, f) {
            arity(2, arguments, "map-mat", false);
            runtime.checkArgsInternalInline("Matrix", "map-mat", self, annMatrix, f, runtime.Function);
            new_mtrx = duplicateMatrix(self);
            return runtime.safeCall(
                function () {
                    return runtime.raw_array_mapi(
                        F(function (val, idx) {
                            return runtime.safeCall(
                                function () {
                                    return f.app((idx - (idx % self.$w)) / self.$w, idx % self.$w, val);
                                }, function (res) {
                                    if (!jsnum.isPyretNumber(res)) {
                                        return runtime.ffi.throwMessageException("function needs to return a number");
                                    }
                                    return res;
                                }
                            );
                        }), new_mtrx.$underlyingMat);
                }, function (result) {
                    new_mtrx.$underlyingMat = result;
                    return new_mtrx;
                }
            )
        }
        //Returns a 1d matrix  of elements from (left,top) to  (right,bottom) inclusive
        var getSubmatrix = function (self, left, top, right, bottom) {
            arity(5, arguments, "submatrix", false);
            runtime.checkArgsInternalInline("Matrix", "submatrix", self, annMatrix,
                left, runtime.Number, top, runtime.Number, right, runtime.Number, bottom, runtime.Number);

            if (checkRange(self, left, top) && checkRange(self, right, bottom)) {
                if ((right - left) <= 0 || (bottom - top) <= 0) {
                    runtime.ffi.throwMessageException("Invalid inputs : given dimensions need to be rectangular")
                }
                start_pos = get1dpos(left, top, self.$w)
                end_pos = get1dpos(right, bottom, self.$w);
                new_arr = new Array(end_pos - start_pos + 1);
                duplicateArray(self, start_pos, end_pos + 1, new_arr, 0);
                return createMatrixFromArray(end_pos - start_pos + 1, 1, new_arr);

            } else {
                return runtime.ffi.throwMessageException("Invalid dimensions");
            }
        }
        //Creates new matrix with n . Ex : fillMatrix(2,2,5.8) = [mat(2,2): 5.8,5.8,5.8,5.8]
        var fillMatrix = function (h, w, n) {
            arity(3, arguments, "fill-mat", false);
            runtime.checkArgsInternalInline("Matrix", "fill-mat", h, runtime.Number, w, runtime.Number, n, runtime.Number);
            posInteger(h, w);
            return createMatrixFromArray(h, w, new Array(h * w).fill(n));
        }

        /**
         * 
         * @param {Number} n 
         * @returns Identity Matrix of dimensions nxn
         */
        var IdentityMatrix = function (n) {
            arity(1, arguments, "identity-mat", false);
            runtime.checkArgsInternalInline("Matrix", "identity-mat", n, runtime.Number);
            if (n < 0 || !Number.isInteger(n)) {
                runtime.ffi.throwMessageException("Matrix dimension must be positive integer");
            } else {
                var req_mat = fillMatrix(n, n, 0);
                for (var i = 0; i < n; i++) {
                    req_mat.$underlyingMat[get1dpos(i, i, n)] = 1;
                }
                return req_mat;
            }
        }
        // Is the matrix a row matrix i.e has dimensions (n,1)
        var isRowMatrix = function (self) {
            arity(1, arguments, "is-row-mat", false);
            runtime.checkArgsInternalInline("Matrix", "is-row-mat", self, annMatrix);
            return runtime.makeBoolean(self.$h === 1);
        }
        // Is the matrix a col matrix i.e has dimensions (1,n)
        var isColMatrix = function (self) {
            arity(1, arguments, "is-col-mat", false);
            runtime.checkArgsInternalInline("Matrix", "is-col-mat", self, annMatrix);
            return runtime.makeBoolean(self.$w === 1)
        }
        //Is the matrix square
        var isSquareMatrix = function (self) {
            arity(1, arguments, "is-square-mat", false);
            runtime.checkArgsInternalInline("Matrix", "is-square-mat", self, annMatrix);
            return runtime.makeBoolean(self.$w === self.$h);
        }
        //Calculates trace of matrix
        var traceMatrix = function (self) {
            arity(1, arguments, "trace-mat", false);
            runtime.checkArgsInternalInline("Matrix", "trace-mat", self, annMatrix);
            var res = runtime.makeNumber(0);
            if (!isSquareMatrix(self)) {
                return runtime.ffi.throwMessageException("Matrix must be square");
            }
            for (var i = 0; i < self.$h; i++) {
                res = runtime.plus(res, self.$underlyingMat[get1dpos(i, i, self.$w)]);
            }
            return res;
        }

        //Returns the Reduced Row Echelon Form of the Matrix
        var reducedRowEchelonMatrix = function (self) {
            arity(1, arguments, "rref-mat", false);
            runtime.checkArgsInternalInline("Matrix", "rref-mat", self, annMatrix);
            //Copy matrix into new one 
            toret = duplicateMatrix(self);
            var lead = 0;
            for (var r = 0; r < toret.$h; r++) {
                var i = r;
                while (get1dElem(toret, i, lead) == 0) {
                    i++;
                    if (toret.$h == i) {
                        i = r;
                        lead++;
                        if (toret.$w == lead) {
                            return;
                        }
                    }
                }
                for (var k = 0; k < toret.$w; k++) {
                    var tmp = get1dElem(toret, i, k);
                    toret.$underlyingMat[get1dpos(i, k, toret.$w)] = get1dElem(toret, r, k);
                    toret.$underlyingMat[get1dpos(r, k, toret.$w)] = tmp;
                }

                var val = get1dElem(toret, r, lead);
                for (var j = 0; j < toret.$w; j++) {

                    toret.$underlyingMat[get1dpos(r, j, toret.$w)] = runtime.divide(toret.$underlyingMat[get1dpos(r, j, toret.$w)], val);

                }

                for (var i = 0; i < toret.$h; i++) {
                    if (i == r) continue;
                    val = get1dElem(toret, i, lead);

                    for (var j = 0; j < toret.$w; j++) {
                        toret.$underlyingMat[get1dpos(i, j, toret.$w)] = runtime.minus(toret.$underlyingMat[get1dpos(i, j, toret.$w)], runtime.times(val, get1dElem(toret, r, j)));
                    }
                }
                lead++;
            }

            return toret;

        }
        /**
         * 
         * @param {Matrix} self 
         * @param {Matrix} other 
         * @returns Pyret Matrix with other augmented onto self (stacking horizontally)
         */
        var AugmentMat = function (self, other) {
            arity(2, arguments, "augment-mat", false);
            runtime.checkArgsInternalInline("Matrix", "augment-mat", self, annMatrix, other, annMatrix);
            if (self.$h != other.$h) {
                return runtime.ffi.throwMessageException("Both matrices need to have same number of rows");

            }
            var new_arr = new Array(self.$h * (self.$w + other.$w));
            var c = self.$w + other.$w;
            for (var i = 0; i < self.$h; i++) {
                for (var j = 0; j < self.$w; j++) {
                    new_arr[get1dpos(i, j, c)] = self.$underlyingMat[get1dpos(i, j, self.$w)];
                }
                for (var k = self.$w, b = 0; k < c; k++, b++) {
                    new_arr[get1dpos(i, k, c)] = other.$underlyingMat[get1dpos(i, b, other.$w)]

                }
            }
            return makeMatrix(self.$h, c, new_arr);
        }

        //Internal function : Swaps rows m,n in place
        var swapRows = function (self, m, n) {
            for (i = 0; i < self.$w; i++) {
                temp = get1dElem(self, m, i);
                self.$underlyingMat[get1dpos(m, i, self.$w)] = get1dElem(self, n, i);
                self.$underlyingMat[get1dpos(n, i, self.$w)] = temp;
            }
        }
        /* Calculates LUP decomposition PA = LU where:
         P is permutation matrix
         L is lower triangular
         U is upper triangular 
         return tuple of (P,L,U,exchanges) where exchanges is the no of row swaps*/
        var LUPMat = function (self) {
            arity(1, arguments, "lup-mat", false);
            runtime.checkArgsInternalInline("Matrix", "lup-mat", self, annMatrix);
            if (!isSquareMatrix(self)) {
                return runtime.ffi.throwMessageException("LU decomposition with partial pivoting requires square matrix");
            }
            dim = self.$h;
            P = IdentityMatrix(dim);
            L = IdentityMatrix(dim);
            U = duplicateMatrix(self);
            exchanges = runtime.makeNumber(0);

            let getMaxEntry = function (cIndex) {
                let maxIndex = cIndex
                max = runtime.num_abs(get1dElem(U, cIndex, cIndex));



                for (i = cIndex + 1; i < dim; i++) {

                    let next = runtime.num_abs(get1dElem(U, i, cIndex));

                    if (next > max) {
                        max = next;
                        maxIndex = i;
                    }

                }

                return maxIndex;
            };

            let pivot = function (p, n) {

                // U
                for (i = p; i < dim; i++) {
                    temp = get1dElem(U, p, i);
                    U.$underlyingMat[get1dpos(p, i, dim)] = get1dElem(U, n, i);
                    U.$underlyingMat[get1dpos(n, i, dim)] = temp;

                }

                // L
                for (i = 0; i < p; i++) {
                    temp = get1dElem(L, p, i);
                    L.$underlyingMat[get1dpos(p, i, dim)] = get1dElem(L, n, i);
                    L.$underlyingMat[get1dpos(n, i, dim)] = temp;
                }
                swapRows(P, p, n);
                exchanges = runtime.plus(exchanges, 1);
            };

            let eliminate = function (p) {
                let i, j;

                for (i = p + 1; i < dim; i++) {
                    let l = runtime.divide(get1dElem(U, i, p), get1dElem(U, p, p));
                    L.$underlyingMat[get1dpos(i, p, dim)] = l;
                    for (j = p; j < dim; j++) {
                        U.$underlyingMat[get1dpos(i, j, dim)] = runtime.minus(get1dElem(U, i, j), runtime.times(l, get1dElem(U, p, j)));
                    }
                }

            };
            let PLU = function () {

                let i;
                for (i = 0; i < dim - 1; i++) {
                    // Find the max entry
                    let maxIndex = getMaxEntry(i);
                    // Pivoting
                    if (i != maxIndex) {
                        pivot(i, maxIndex);

                    }

                    // Eliminate
                    eliminate(i);
                }
            };

            PLU();
            return runtime.makeTuple([P, L, U, exchanges]);
        };


        /**
         * 
         * @param {Matrix} self 
         * @returns Determinant of matrix if it exists
         */
        var Determinant = function (self) {
            arity(1, arguments, "determinant", false);
            runtime.checkArgsInternalInline("Matrix", "determinant", self, annMatrix);
            if (!isSquareMatrix(self)) {
                return runtime.ffi.throwMessageException("Matrix must be square");
            } else {
                d = runtime.makeNumber(1);
                LUP = LUPMat(self);
                U = LUP.vals[2];
                for (var i = 0; i < self.$h; i++) {
                    d = runtime.times(d, get1dElem(U, i, i));
                }
                if (LUP.vals[3] % 2 == 1) {
                    d = runtime.times(d, -1);
                }

                return d;
            }
        }
        //Is the matrix invertible
        var isInvertibleMatrix = function (self) {
            arity(1, arguments, "is-invertible", false);
            runtime.checkArgsInternalInline("Matrix", "is-invertible", self, annMatrix);
            if (!isSquareMatrix(self)) {
                return runtime.ffi.throwMessageException("Matrix must be square");
            } else {
                var det = Determinant(self);
                if (det == 0) {
                    return runtime.makeBoolean(false);
                } else {
                    return runtime.makeBoolean(true);
                }
            }
        }
        //Calculate inverse of matrix
        var inverseMatrix = function (self) {
            arity(1, arguments, "inverse", false);
            runtime.checkArgsInternalInline("Matrix", "inverse", self, annMatrix);
            if (!isSquareMatrix(self)) {
                return runtime.ffi.throwMessageException("Matrix must be square");
            } else {
                augment_rref = reducedRowEchelonMatrix(AugmentMat(self, IdentityMatrix(self.$h)));
                var new_arr = new Array(self.$l);
                for (var i = 0; i < self.$h; i++) {
                    for (var j = 0; j < self.$w; j++) {
                        new_arr[get1dpos(i, j, self.$w)] = get1dElem(augment_rref, i, j + self.$w);
                    }
                }

                return makeMatrix(self.$h, self.$w, new_arr);
            }
        }
        //Given A and b of form Ax = b , returns x
        var solveMatrix = function (self, b) {
            arity(2, arguments, "solve-mat", false);
            runtime.checkArgsInternalInline("Matrix", "solve-mat", self, annMatrix, b, annMatrix);
            if (!isInvertibleMatrix(self)) {
                return runtime.ffi.throwMessageException("Cannot solve non invertible matrix. Try least squares");
            } else {
                return multMatrix(inverseMatrix(self), b);
            }
        }
        //Finds least squares solution of Ax = b 
        var leastSquares = function (self, b) {
            arity(2, arguments, "least-squares", false);
            runtime.checkArgsInternalInline("Matrix", "least-squares", self, annMatrix, b, annMatrix);
            at = transposeMatrix(self);
            ata_inv = inverseMatrix(multMatrix(at, self));
            return multMatrix(ata_inv, multMatrix(at, b));
        }
        //Applies Gram Schmidt to generate orthonormal matrix
        var Gram_Schmidt = function (self) {
            arity(1, arguments, "gram-schmidt", false);
            runtime.checkArgsInternalInline("Matrix", "gram-schmidt", self, annMatrix);
            /*For Gram Schmidt we operate on columns as vectors so we just create an array of vectors from matrix */
            var ans_vectors = new Array(self.$w);
            function colToVector(cn) {
                return makeVector(new Array(self.$h).fill(0).map((v, i) => get1dElem(self, i, cn)));
            }
            ans_vectors[0] = colToVector(0);
            var mags = new Array(self.$w);
            function magSqr(n) {
                tmp = ans_vectors[n].$underlyingMat.reduce((a, v) => runtime.plus(a, runtime.times(v, v)), 0);
                mags[n] = tmp;
            }
            magSqr(0);

            for (col = 1; col < self.$w; col++) {
                var currVector = colToVector(col);

                for (prev = 0; prev < col; prev++) {

                    sclr = runtime.divide(dotVector(currVector, ans_vectors[prev]), mags[prev]);
                    currVector = minusVector(currVector, scaleVector(ans_vectors[prev], sclr));
                }
                ans_vectors[col] = currVector
                magSqr(col);
            }
            mags = mags.map(v => runtime.num_sqrt(v));
            var new_arr = new Array(self.$h * self.$w);
            for (i = 0; i < self.$h; i++) {
                for (j = 0; j < self.$w; j++) {
                    new_arr[get1dpos(i, j, self.$w)] = runtime.divide(ans_vectors[j].$underlyingMat[i], mags[j]);
                }
            }

            return makeMatrix(self.$h, self.$w, new_arr);



        }
        //Finds QR decomposition using Gram Schmidt
        var qrMatrix = function (self) {
            arity(1, arguments, "qr-mat", false);
            runtime.checkArgsInternalInline("Matrix", "qr-mat", self, annMatrix);
            q = Gram_Schmidt(self);
            r = multMatrix(transposeMatrix(q), self);
            return runtime.makeTuple([q, r]);
        }
        //Finds pth norm of matrix
        var normMatrix = function (self, p) {
            arity(2, arguments, "norm-mat", false);
            runtime.checkArgsInternalInline("Matrix", "norm-mat", self, annMatrix, p, runtime.Number);
            sum = self.$underlyingMat.reduce((a, v) => runtime.plus(a, runtime.num_expt(v, p)), 0);
            return runtime.num_expt(sum, runtime.divide(1, p));
        }
        //Creates matrix of size (h,w) by applying f
        var buildMatrix = function (h, w, f) {
            arity(3, arguments, "build-mat", false);
            runtime.checkArgsInternalInline("Matrix", "build-mat", h, runtime.Number, w, runtime.Number, f, runtime.Function);
            posInteger(h, w);
            return runtime.safeCall(
                function () {
                    return runtime.raw_array_build(F(function (idx) {
                        return runtime.safeCall(
                            function () {
                                return f.app((idx - (idx % w)) / w, idx % w);
                            }, function (result) {
                                if (!jsnum.isPyretNumber(result)) {
                                    return runtime.ffi.throwMessageException("Function must return a number");
                                }
                                return result;
                            }
                        );

                    }), h * w);
                }, function (result) {
                    return makeMatrix(h, w, result);
                }
            );
        }
        //Calculates forbenius Matrix
        var frobMatrix = function (self) {
            arity(1, arguments, "frob-norm", false);
            runtime.checkArgsInternalInline("Matrix", "frob-norm", self, annMatrix);
            return runtime.num_sqrt(traceMatrix(multMatrix(transposeMatrix(self), self)));

        }
        //Calculates A^n 
        var expMatrix = function (self, n) {
            arity(2, arguments, "exp-mat", false);
            runtime.checkArgsInternalInline("Matrix", "exp-mat", self, annMatrix, n, runtime.Number);
            posInteger(n)
            sq = multMatrix(self, self);
            even = (runtime.num_modulo(n, 2) == runtime.makeNumber(0));
            len = (even) ? (n / 2) : (n - 1) / 2;
            bogus = new Array(len);
            return runtime.safeCall(
                function () {
                    return runtime.raw_array_fold(
                        F(function (acc, dummy, dummy2) {
                            return multMatrix(sq, sq);
                        })
                        , sq, bogus, 1);
                },
                function (result) {
                    if (even) { return result; }
                    else { return multMatrix(result, self); }
                }
            );


        }
        /**
        * 
        * @param {Number} h 
        * @param {Number} w 
        * @param {Array} underlyingMat 
        * @returns Pyret Matrix
        */

        function makeMatrix(h, w, underlyingMat) {
            var equalMatrix = runtime.makeMethod2(function (self, other, recEq) {
                runtime.ffi.checkArity(3, arguments, "_equals", true);
                runtime.checkArgsInternal3("Matrix", "_equals", self, annMatrix, other, annMatrix, recEq, runtime.Function);

                if (!hasBrand(brandMatrix, other)) {
                    return runtime.ffi.notEqual.app('', self, other);
                } else if (!sameDims(self, other)) {
                    return runtime.ffi.notEqual.app('', self, other);
                } else {
                    function equalsHelp(i) {
                        return runtime.safeCall(
                            function () { return recEq.app(self.$underlyingMat[i], other.$underlyingMat[i]) },
                            function (result) {
                                if (runtime.ffi.isNotEqual(result)) {
                                    return result;
                                } else if ((i + 1) == self.$l) {
                                    return result;
                                } else {
                                    res = equalsHelp(i + 1);
                                    if (runtime.ffi.isUnknown(result) && res == runtime.ffi.equal) {
                                        return result;
                                    } else {
                                        return res;
                                    }
                                }
                            }
                        )
                    };
                    return equalsHelp(0);

                }
            }, "equals");
            var get_height = runtime.makeMethod0(function (self) {
                return self.$h
            }, "get-height");
            var get_width = runtime.makeMethod0(function (self) {
                return self.$w
            }, "get-width");
            var get_shape = runtime.makeMethod0(function (self) {
                return runtime.makeTuple([self.$h, self.$w])
            }, "get-shape");
            var get_elem = runtime.makeMethod3(function (self, h, c) {
                if (checkRange(self, h, c)) {
                    return get1dElem(self, h, c);
                }
            })
            var obj = O({
                _output: outputMatrix,
                _plus: runtime.makeMethod1(addMatrix, "plus"),
                _minus: runtime.makeMethod1(subMatrix, "minus"),
                _equals: equalMatrix,
                _times: runtime.makeMethod1(multMatrix, "times"),
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
        //Get matrix row as vector
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
        //get matrix col as vector
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

        //dot product of self ,other
        var dotVector = function (self, other) {
            arity(2, arguments, "vdot", false);
            runtime.checkArgsInternalInline("Matrix", "vdot", self, annVector, other, annVector);
            if (!(self.$l == other.$l)) {
                return runtime.ffi.throwMessageException("Vectors have dimensions " + self.$l + " and " + other.$l + " . They cannot be multiplied.");
            } else {
                var sum = runtime.makeNumber(0);
                for (var i = 0; i < self.$l; i++) {
                    sum = runtime.plus(sum, runtime.times(self.$underlyingMat[i], other.$underlyingMat[i]));
                }
                return sum;
            }
        }
        //magnitude of self
        var magnitudeVector = function (self) {
            arity(1, arguments, "magnitude", false);
            runtime.checkArgsInternalInline("Matrix", "magnitude", self, annVector);
            if (self.$l == 0) {
                return runtime.ffi.throwMessageException("Empty vector has no magnitude");
            } else {
                var ans = runtime.num_sqrt(self.$underlyingMat.reduce((a, n) => runtime.plus(a, runtime.times(n, n)), 0));
                return ans;
            }
        }
        //normalize vector
        var normalizeVector = function (self) {
            arity(1, arguments, "normalize", false);
            runtime.checkArgsInternalInline("Matrix", "normalize", self, annVector);

            var mag = magnitudeVector(self);
            var new_arr = self.$underlyingMat.map((n) => { return runtime.divide(n, mag) });
            return makeVector(new_arr);
        }
        //scale vector by num
        var scaleVector = function (self, num) {
            arity(2, arguments, "vscale", false);
            var new_arr = self.$underlyingMat.map((n) => { return runtime.times(n, num) });
            return makeVector(new_arr);

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
                runtime.makeString("vector(" + self.$l + ")"),
                runtime.ffi.makeList(rows));
        });

        var addVector = function (self, other) {
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
        };
        var minusVector = function (self, other) {
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
        };
        function makeVector(underlyingArr) {

            var equalVector = runtime.makeMethod2(function (self, other, recEq) {
                runtime.ffi.checkArity(3, arguments, "_equals", true);
                if (!hasBrand(brandVector, other)) {
                    return runtime.ffi.notEqual.app('', self, other);
                } else if (!(self.$l == other.$l)) {
                    return runtime.ffi.notEqual.app('', self, other);
                } else {
                    function equalsHelp(i) {
                        return runtime.safeCall(
                            function () { return recEq.app(self.$underlyingMat[i], other.$underlyingMat[i]) },
                            function (result) {
                                if (runtime.ffi.isNotEqual(result)) {
                                    return result;
                                } else if ((i + 1) == self.$l) {
                                    return result;
                                } else {
                                    return equalsHelp(i + 1);
                                }
                            }
                        )
                    };
                    return equalsHelp(0);


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
            var getElm = runtime.makeMethod1(function (self, num) {
                runtime.ffi.checkArity(2, arguments, "get", true);
                runtime.checkArgsInternalInline("Vector", "get", self, annVector, num, runtime.Number);
                if (num < 0 || num >= self.$l) {
                    return runtime.ffi.throwMessageException("Invalid index");
                } else {
                    return self.$underlyingMat[num];
                }
            }, "get")
            var obj = O({
                _output: outputVector,
                _equals: equalVector,
                _plus: runtime.makeMethod1(addVector),
                _minus: runtime.makeMethod1(minusVector),
                _times: multVector,
                "get": getElm
            });
            obj = applyBrand(brandVector, obj);
            obj.$underlyingMat = underlyingArr;
            obj.$l = underlyingArr.length;
            return obj;
        }

        function createMatrixFromArray(h, w, array) {
            arity(3, arguments, "matrix", false);
            var len = array.length;
            if (h * w != len) {
                runtime.ffi.throwMessageException("The number of provided elements does not match the given width and height.");
            }
            for (var i = 0; i < len; i++) {
                if (!jsnum.isPyretNumber(array[i])) {
                    runtime.ffi.throwMessageException("A Matrix can only consist of numbers");
                }
            }
            return makeMatrix(h, w, array);
        }

        function createVectorFromArray(dim, arr) {
            arity(2, arguments, "vector", false);
            for (var i = 0; i < arr.length; i++) {
                if (!jsnum.isPyretNumber(arr[i])) {
                    runtime.ffi.throwMessageException("A Vector can only consist of numbers");
                }
            }
            return makeVector(arr);

        }


        function matrixInit(h, w) {
            if (!(Number.isInteger(h)) || !(Number.isInteger(w)) || h < 0 || w < 0) {
                runtime.ffi.throwMessageException("The provided width or height is invalid. Matrix dimensions need to be a positive integer");
            }
            return O({
                make: F((arr) => {
                    return createMatrixFromArray(h, w, arr)
                }, "matrix:make"),
                make0: F(() => {
                    return createMatrixFromArray(h, w, [])
                }, "matrix:make0"),
                make1: F((a) => {
                    return createMatrixFromArray(h, w, [a])
                }, "matrix:make1"),
                make2: F((a, b) => {
                    return createMatrixFromArray(h, w, [a, b])
                }, "matrix:make2"),
                make3: F((a, b, c) => {
                    return createMatrixFromArray(h, w, [a, b, c])
                }, "matrix:make3"),
                make4: F((a, b, c, d) => {
                    return createMatrixFromArray(h, w, [a, b, c, d])
                }, "matrix:make4"),
                make5: F((a, b, c, d, e) => {
                    return createMatrixFromArray(h, w, [a, b, c, d, e])
                }, "matrix:make5")
            });
        }

        function vectorInit(dim) {
            posInteger(dim);
            return O({
                make: F((arr) => {
                    return createVectorFromArray(arr)
                }, "vector:make"),
                make0: F(() => {
                    return createVectorFromArray(dim, [])
                }, "vector:make0"),
                make1: F((a) => {
                    return createVectorFromArray(dim, [a])
                }, "vector:make1"),
                make2: F((a, b) => {
                    return createVectorFromArray(dim, [a, b])
                }, "vector:make2"),
                make3: F((a, b, c) => {
                    return createVectorFromArray(dim, [a, b, c])
                }, "vector:make3"),
                make4: F((a, b, c, d) => {
                    return createVectorFromArray(dim, [a, b, c, d])
                }, "vector:make4"),
                make5: F((a, b, c, d, e) => {
                    return createVectorFromArray(dim, [a, b, c, d, e])
                }, "vector:make5"),
            })
        }
        function isMatrix(obj) {
            arity(1, arguments, "is-matrix", false);
            return runtime.makeBoolean(internal_isMtrx(obj));
        }
        function isVector(obj) {
            arity(1, arguments, "is-vector", false);
            return runtime.makeBoolean(internal_isVec(obj));
        }
        var jsCheckMtrx = runtime.makeCheckType(internal_isMtrx, "Matrix");
        var jsCheckVec = runtime.makeCheckType(internal_isVec, "Vector");
        var vals = {
            "mat": F(matrixInit, "mat"),
            "is-mat": F(isMatrix, "is-mat"),
            "vector": F(vectorInit, "vector"),
            "is-vector": F(isVector, "is-vector"),
            "is-row-mat": F(isRowMatrix, "is-row-mat"),
            "is-col-mat": F(isColMatrix, "is-col-mat"),
            "is-square-mat": F(isSquareMatrix, "is-square-mat"),
            "is-invertible": F(isInvertibleMatrix, "is-invertible"),
            "inverse": F(inverseMatrix, "inverse"),
            "solve-mat": F(solveMatrix, "solve-mat"),
            "trace-mat": F(traceMatrix, "trace-mat"),
            "least-squares": F(leastSquares, "least-squares"),
            "rref-mat": F(reducedRowEchelonMatrix, "rref-mat"),
            "augment-mat": F(AugmentMat, "augment-mat"),
            "add-mat": F(addMatrix, "add-mat"),
            "sub-mat": F(subMatrix, "sub-mat"),
            "mult-mat": F(multMatrix, "mult-mat"),
            "get-elem": F(getMatrixElms, "get-elem"),
            "transpose": F(transposeMatrix, "transpose"),
            "lup-mat": F(LUPMat, "lup-mat"),
            "determinant": F(Determinant, "determinant"),
            "identity-mat": F(IdentityMatrix, "identity-mat"),
            "stack-mat": F(stackMatrix, "stack-mat"),
            "scale-mat": F(scaleMatrix, "scale-mat"),
            "set-elem": F(setMatrixElms, "set-elem"),
            "reshape": F(reshapeMatrix, "reshape"),
            "map-mat": F(mapMatrix, "map-mat"),
            "submatrix": F(getSubmatrix, "submatrix"),
            "fill-mat": F(fillMatrix, "fill-mat"),
            "vdot": F(dotVector, "vdot"),
            "magnitude": F(magnitudeVector, "magnitude"),
            "vscale": F(scaleVector, "vscale"),
            "normalize": F(normalizeVector, "normalize"),
            "get-row": F(getMatrixRow, "get-row"),
            "get-col": F(getMatrixCol, "get-col"),
            "vector-to-list": F(vectorToList, "vector-to-list"),
            "vector-to-array": F(vectorToArray, "vector-to-array"),
            "gram-schmidt": F(Gram_Schmidt, "gram-schmidt"),
            "qr-mat": F(qrMatrix, "qr-mat"),
            "exp-mat": F(expMatrix, "exp-mat"),
            "frob-norm": F(frobMatrix, "frob-norm"),
            "dims-mat": F(getMatrixDims, "dims-mat"),
            "build-mat": F(buildMatrix, "build-mat"),
            "norm-mat": F(normMatrix, "norm-mat"),

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