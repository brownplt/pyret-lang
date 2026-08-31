function mulberry32(seed) {
  var a = seed >>> 0;
  return function() {
    a = (a + 0x6D2B79F5) >>> 0;
    var t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t = (t + Math.imul(t ^ (t >>> 7), t | 61)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

var gasRng = mulberry32(12345);
var runGasRng = mulberry32(67890);

module.exports = {
  initialGas: function() { return 2 + Math.floor(gasRng() * 2000); },
  initialRunGas: function() { return 10 + Math.floor(runGasRng() * 20000); }
};
