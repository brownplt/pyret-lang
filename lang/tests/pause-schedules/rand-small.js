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

var gasRng = mulberry32(0x9E3779B9);
var runGasRng = mulberry32(0x85EBCA6B);

module.exports = {
  initialGas: function() { return 2 + Math.floor(gasRng() * 63); },
  initialRunGas: function() { return 8 + Math.floor(runGasRng() * 505); }
};
