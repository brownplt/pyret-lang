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
      let n = y_s.length;
      let M = new Array(n);
      for (let i = 0; i < n; i++) {
        M[i][0] = y_s[i];
      }
      return M;
    }

    function matrixTranspose(m) {
      let numrows = m.length;
      let numcols = m[0].length;
      let mT = new Array(numcols);
      for (let i = 0; i < numcols; i++) {
        mT[i] = new Array(m);
        for (let j = 0; j < numrows; j++) {
          mT[i][j] = m[j][i];
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
      for (let i = 0; i < size; i++) {
        det += m[0][i] * matrixCofactor(m, 0, i);
      }
      return det;
    }

    function matrixCofactor(m, i, j) {
      let size = m.length;
      if (size === 0) {
        return 0;
      }
      if (m[0].length !== size) {
        return 0;
      }
      let mm = new Array(size - 1);
      for (let ii = 0; ii < size; ii++) {
        if (ii === i) { continue; }
        let iii = (ii > i ? ii - 1 : ii)
        mm[iii] = new Array(size - 1);
        for (let jj = 0; jj < size; jj++) {
          if (jj === j) { continue; }
          let jjj = (jj > j ? jj - 1 : jj)
          mm[iii][jjj] = m[ii][jj];
        }
      }
      let sign = (i + j) % 2;
      sign = (sign === 0) ? 1 : -1;
      return sign * matrixDeterminant(mm)
    }

    function matrixAdjoint(m) {
      let size = m.length;
      let mc = new Array(size);
      for (let i = 0; i < size; i++) {
        mc[i] = new Array(size);
        for (let j = 0; j < size; j++) {
          mc[i][j] = matrixCofactor(m, i, j);
        }
      }
      return matrixTranspose(mc)
    }

    function matrixInverse(m) {
      let det = matrixDeterminant(m);
      let mI = matrixAdjoint(m);
      let k = m.length;
      for (let i = 0; i < k; i++) {
        for (let j = 0; j < k; j++) {
          let x = mI[i][j]
          mI[i][j] = x/det
        }
      }
      return mI;
    }

    function matrixMultiply2(m1, m2) {
      let m1r = m1.length;
      let m1c = m1[0].length;
      let m2c = m2[0].length;
      let mP = new Array(m1r);
      for (let i = 0; i < m1r; i++) {
        mP[i] = new Array(m2c);
        for (let j = 0; j < m2c; j++) {
          let x = 0;
          for (let k = 0; k < m1c; k++) {
            x += m1[i][k] * m2[k][j];
          }
          mP[i][j] = x;
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
        throw runtime.ffi.throwMessageException("multiple-regression: number of mappings incorrect");
      }
      let X = new Array(num_mappings);
      let Y = new Array(num_mappings);
      let x_s_len = false;
      js_x_s_s.forEach(function(x_s, i) {
        runtime.checkTuple(x_s);
        let js_x_s = x_s.vals;
        let x_s_n = js_x_s.length;
        if (x_s_len === false) {
          x_s_len = x_s_n;
        } else if (x_s_n !== x_s_len) {
          throw runtime.ffi.throwMessageException("multiple-regression: bad mapping");
        }
        X[i] = new Array(x_s_len + 1)
        let Xi = X[i];
        Xi[0] = 1;
        js_x_s.forEach(function(x, j) {
          runtime.checkNumber(x);
          Xi[j+1] = runtime.num_to_fixnum(x);
        });
      });
      js_y_s.forEach(function(y, i) {
        runtime.checkNumber(y);
        Y[i] = [runtime.num_to_fixnum(y)];
      });

      let XT = matrixTranspose(X);
      let B = matrixMultiply2(matrixInverse(matrixMultiply2(XT, X)), matrixMultiply2(XT, Y));
      let Bfunc = function(x_s) {
        runtime.checkTuple(x_s);
        let js_x_s = x_s.vals;
        if (js_x_s.length !== x_s_len) {
          throw runtime.ffi.throwMessageException("predictor: wrong number of arguments");
        }
        let result = B[0][0];
        for (let i = 0; i < x_s_len; i++) {
          result += runtime.num_to_fixnum(js_x_s[i]) * B[i+1][0]
        }
        return runtime.makeNumber(result);
      }
      return runtime.makeFunction(Bfunc, "predictor");
    }

    let vals = {

      "multiple-regression": runtime.makeFunction(multipleRegression, "multiple-regression")

    };

    return runtime.makeModuleReturn(vals, {});
  }
})

