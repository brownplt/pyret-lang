var phase = 0;

module.exports = {
  initialGas: function() {
    phase = phase + 1;
    return (phase % 2) === 1 ? 5 : 100000;
  },
  initialRunGas: function() {
    return (phase % 2) === 1 ? 100000 : 60;
  }
};
