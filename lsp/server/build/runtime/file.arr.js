const fs = require( 'fs' );

module.exports = {
  "file-exists": function( path ) {
    return fs.existsSync( path );
  },

  "file-imes": function( path ) {
    stats = fs.statSync( path );
    return {mtime: stats.mtime, atime: stats.atime, ctime: stats.ctime};
  },

  "file-to-string": function( path ) {
    return fs.readFileSync( path, 'utf8' );
  },

  "real-path": function( path ) {
    return fs.realpathSync( path );
  }
};
