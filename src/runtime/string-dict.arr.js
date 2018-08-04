module.exports = {
  'count': function( list ) {
    dict = {};

    for ( elm of list ) {
      if ( !dict.hasOwnProperty( elm ) ) {
        dict[elm] = 0;
      }
      
      dict[elm] += 1;
    }

    return dict;
  },
  'size': function( dict ) {
    return Object.keys( dict ).length;
  },
  'get': function( dict, elm ) {
    return dict[elm];
  },
  'has-key': function( dict, elm ) {
    return dict.hasOwnProperty( elm );
  },
  'keys': function( dict ) {
    return Object.keys( dict );
  },
  'values': function( dict ) {
    return Object.values( dict );
  }
};