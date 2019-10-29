module.exports = {
  'raw-array': {
    'make': function(arr) {
      return arr;
    }
  },
  'at': function( arr, index ) {
    return arr[index];
  },
  'get': function( list, index ) {
    var arr = [];
    for (i = 0; i < list.length; i++) {
      arr.push(list[i][index]);
    }
    return arr;
  },
  'push': function( arr, elm ) {
    arr.push( elm );
    return arr;
  },
  'fold': function( fun, val, arr ) {
    return arr.reduce( fun, val );
  },
  'sum': function( arr ) {
    return arr.reduce( function( x, y ) { return x + y; }, 0 );
  },
  'min': function( arr ) {
    return arr.reduce( function( x, y ) { return Math.min( x, y ); }, arr[0] );
  },
  'max': function( arr ) {
    return arr.reduce( function( x, y ) { return Math.max( x, y ); }, arr[0] );
  }
};
