function _makeTable(headers, rows) {
  return {
    '_header-raw-array': headers,
    '_rows-raw-array': rows,
    $brand: '$table'
  };
}

module.exports = {
  '_makeTable': _makeTable
};
