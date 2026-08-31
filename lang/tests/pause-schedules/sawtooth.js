var gas = 1024;

module.exports = {
  initialGas: function() {
    gas = gas <= 2 ? 1024 : (gas / 2);
    return gas;
  },
  initialRunGas: function() {
    return gas * 8;
  }
};
