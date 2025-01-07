({
  requires: [],
  nativeRequires: ["pyret-base/js/namespace"],
  provides: {
    values: {
      "multiple-regression": "tany"
    }
  },
  theModule: function(runtime, namespace, uri) {

    function makeColumnMatrix(y_s) {
      let size = y_s.length;
      let M = new Array(size);
      for (let r = 0; r < size; r++) {
        M[r][0] = y_s[r];
      }
      return M;
    }

    function matrixTranspose(m) {
      let numRows = m.length;
      let numCols = m[0].length;
      let mT = new Array(numCols);
      for (let r = 0; r < numCols; r++) {
        mT[r] = new Array(numRows);
        for (let c = 0; c < numRows; c++) {
          mT[r][c] = m[c][r];
        }
      }
      return mT;
    }

    function matrixDeterminant(m) {
      let size = m.length;
      if (size === 0) {
        return 0;
      }
      if (m[0].length !== size) {
        return 0;
      }
      if (size === 1) {
        return m[0][0];
      }
      if (size === 2) {
        return (m[0][0] * m[1][1] - m[1][0] * m[0][1]);
      }
      let det = 0;
      for (let c = 0; c < size; c++) {
        det += m[0][c] * matrixCofactor(m, 0, c);
      }
      return det;
    }

    function matrixCofactor(m, r, c) {
      let size = m.length;
      if (size === 0) {
        return 0;
      }
      if (m[0].length !== size) {
        return 0;
      }
      let mm = new Array(size - 1);
      for (let ri = 0; ri < size; ri++) {
        if (ri === r) { continue; }
        let rii = (ri > r ? ri - 1 : ri)
        mm[rii] = new Array(size - 1);
        for (let ci = 0; ci < size; ci++) {
          if (ci === c) { continue; }
          let cii = (ci > c ? ci - 1 : ci)
          mm[rii][cii] = m[ri][ci];
        }
      }
      let sign = (r + c) % 2;
      sign = (sign === 0) ? 1 : -1;
      return sign * matrixDeterminant(mm);
    }

    function matrixAdjoint(m) {
      let size = m.length;
      let mc = new Array(size);
      for (let r = 0; r < size; r++) {
        mc[r] = new Array(size);
        for (let c = 0; c < size; c++) {
          mc[r][c] = matrixCofactor(m, r, c);
        }
      }
      return matrixTranspose(mc);
    }

    function matrixInverse(m) {
      let det = matrixDeterminant(m);
      let mI = matrixAdjoint(m);
      let size = m.length;
      for (let r = 0; r < size; r++) {
        for (let c = 0; c < size; c++) {
          let x = mI[r][c];
          mI[r][c] = x/det;
        }
      }
      return mI;
    }

    function matrixMultiply2(m1, m2) {
      let m1numRows = m1.length;
      let m1numCols = m1[0].length; // this will be === m2numRows
      let m2numCols = m2[0].length;
      let mP = new Array(m1numRows);
      for (let r = 0; r < m1numRows; r++) {
        mP[r] = new Array(m2numCols);
        for (let c = 0; c < m2numCols; c++) {
          let x = 0;
          for (let k = 0; k < m1numCols; k++) {
            x += m1[r][k] * m2[k][c];
          }
          mP[r][c] = x;
        }
      }
      return mP;
    }

    function multipleRegression(x_s_s, y_s) {
      runtime.ffi.checkArity(2, arguments, "multiple-regression", false);
      runtime.checkList(x_s_s);
      runtime.checkList(y_s);
      let js_x_s_s = runtime.ffi.toArray(x_s_s);
      let js_y_s = runtime.ffi.toArray(y_s);
      let num_mappings = js_x_s_s.length;
      if (js_y_s.length !== num_mappings) {
        throw runtime.ffi.throwMessageException("multiple-regression: lists must have equal lengths");
        if (num_mappings < 2) {
          throw runtime.ffi.throwMessageException("multiple-regression: lists must have at least 2 elements each");
        }
      }
      let X = new Array(num_mappings);
      let Y = new Array(num_mappings);
      let x_s_len = false;
      js_x_s_s.forEach(function(x_s, r) {
        // runtime.checkTuple(x_s);
        runtime.checkList(x_s);
        // let js_x_s = x_s.vals;
        let js_x_s = runtime.ffi.toArray(x_s);
        let x_s_n = js_x_s.length;
        if (x_s_len === false) {
          x_s_len = x_s_n;
        } else if (x_s_n !== x_s_len) {
          throw runtime.ffi.throwMessageException("multiple-regression: lengths of input tuples are different");
        }
        X[r] = new Array(x_s_len + 1);
        let Xr = X[r];
        Xr[0] = 1;
        js_x_s.forEach(function(x, c) {
          runtime.checkNumber(x);
          Xr[c+1] = runtime.num_to_fixnum(x);
        });
      });
      js_y_s.forEach(function(y, r) {
        runtime.checkNumber(y);
        Y[r] = [runtime.num_to_fixnum(y)];
      });

      let XT = matrixTranspose(X);
      let B = matrixMultiply2(matrixInverse(matrixMultiply2(XT, X)), matrixMultiply2(XT, Y));
      let Bfunc = function(x_s) {
        // runtime.checkTuple(x_s);
        // let js_x_s = x_s.vals;
        runtime.checkList(x_s);
        let js_x_s = runtime.ffi.toArray(x_s);
        if (js_x_s.length !== x_s_len) {
          throw runtime.ffi.throwMessageException("predictor: received wrong number of arguments");
        }
        let result = B[0][0];
        for (let i = 0; i < x_s_len; i++) {
          let x = js_x_s[i];
          runtime.checkNumber(x);
          result += runtime.num_to_fixnum(x) * B[i+1][0]
        }
        return runtime.num_to_roughnum(result);
      }
      return runtime.makeFunction(Bfunc, "predictor");
    }

    let vals = {

      "multiple-regression": runtime.makeFunction(multipleRegression, "multiple-regression")

    };

    return runtime.makeModuleReturn(vals, {});
  }
})

