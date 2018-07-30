const fs = require( 'fs' );

module.exports = {
  fileExists: function( path ) {
    return fs.existsSync( path );
  },

  fileTimes: function( path ) {
    stats = fs.statSync( path );
    return {mtime: stats.mtime, atime: stats.atime, ctime: stats.ctime};
  },

  fileToString: function( path ) {
    return fs.readFileSync( path, 'utf8' );
  },

  realPath: function( path ) {
    return fs.realpathSync( path );
  }
};
