function _makeTable(headers, rows) {
  return {
    $brand: '$table'
  };
}

module.exports = {
  '_makeTable': _makeTable
};
