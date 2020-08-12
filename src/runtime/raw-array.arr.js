module.exports = {
  'raw-array': {
    'make': function(arr) {
      return arr;
    }
  },
  'raw-array-set': function( arr, index, value ) {
    arr[index] = value;
    return arr;
  },
  'raw-array-get': function( arr, index ) {
    return arr[index];
  },
  'raw-array-push': function( arr, elm ) {
    arr.push( elm );
    return arr;
  },
  'raw-array-fold': function( fun, val, arr ) {
    return arr.reduce( fun, val );
  },
  'raw-array-foldr': function( fun, val, arr ) {
    return arr.reduceRight( fun, val );
  },
  'raw-array-sum': function( arr ) {
    return arr.reduce( function( x, y ) { return x + y; }, 0 );
  },
  'raw-array-min': function( arr ) {
    return arr.reduce( function( x, y ) { return Math.min( x, y ); }, arr[0] );
  },
  'raw-array-max': function( arr ) {
    return arr.reduce( function( x, y ) { return Math.max( x, y ); }, arr[0] );
  },
  'raw-array-of': function(elem, n) {
    if (n <= 0) {
      throw "raw-array-of: <0 repititions";
    }

    return new Array(n).fill(elem);
  },
  'raw-array-build': function(f, n) {
    let array = new Array(n);
    for (let i = 0; i < n; i++) {
      array[i] = f(i);
    }

    return array;
  }
}
