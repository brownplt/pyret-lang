var CONFIG = {
  // increase to be stricter about what is shown in the diff
  THRESHOLD_FACTOR: .08,
  // abosolute difference _must_ be at least this number to be shown in the diff
  MIN_THRESHOLD: 11
}


if (typeof module !== "undefined" && typeof module.exports !== "undefined") {
    module.exports = CONFIG;
}