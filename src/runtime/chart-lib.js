const jsnums = require("./js-numbers.js");
const RUNTIME = require("./runtime.js");
const IMAGE = require("./image.arr.js");

const colorDb = IMAGE.colorDb;
console.log("colorDb: ", colorDb);

function checkColor(val) {
    let aColor = IMAGE["is-image-color"](val);
    if (colorDb.get(val)) {
        aColor = colorDb.get(val);
    }
    return aColor;
}

// TODO(tiffany): rgb2hex???

var getNewWindow = /* @stopify flat */ function (xMinC, xMaxC, yMinC, yMaxC,
    numSamplesC) {
    return cases(RUNTIME.ffi.isOption, 'Option',
        RUNTIME.string_to_number(xMinC.val()), {
        none: function () {
            xMinC.addClass('error-bg');
            xMinC.removeClass('ok-bg');
            return null;
        },
        some: function (xMinVal) {
            xMinC.removeClass('error-bg');
            xMinC.addClass('ok-bg');
            return cases(RUNTIME.ffi.isOption, 'Option',
                RUNTIME.string_to_number(xMaxC.val()), {
                none: function () {
                    xMaxC.addClass('error-bg');
                    xMaxC.removeClass('ok-bg');
                    return null;
                },
                some: function (xMaxVal) {
                    xMaxC.removeClass('error-bg');
                    xMaxC.addClass('ok-bg');

                    if (jsnums.greaterThanOrEqual(xMinVal, xMaxVal,
                        RUNTIME.NumberErrbacks)) {
                        xMinC.addClass('error-bg');
                        xMaxC.addClass('error-bg');
                        xMinC.removeClass('ok-bg');
                        xMaxC.removeClass('ok-bg');
                        return null;
                    }

                    return cases(RUNTIME.ffi.isOption, 'Option',
                        RUNTIME.string_to_number(yMinC.val()), {
                        none: function () {
                            yMinC.addClass('error-bg');
                            yMinC.removeClass('ok-bg');
                            return null;
                        },
                        some: function (yMinVal) {
                            yMinC.removeClass('error-bg');
                            yMinC.addClass('ok-bg');

                            return cases(RUNTIME.ffi.isOption, 'Option',
                                RUNTIME.string_to_number(yMaxC.val()), {
                                none: function () {
                                    yMaxC.addClass('error-bg');
                                    yMaxC.removeClass('ok-bg');
                                    return null;
                                },
                                some: function (yMaxVal) {
                                    yMaxC.removeClass('error-bg');
                                    yMaxC.addClass('ok-bg');

                                    if (jsnums.greaterThanOrEqual(xMinVal, xMaxVal,
                                        RUNTIME.NumberErrbacks)) {
                                        yMinC.addClass('error-bg');
                                        yMaxC.addClass('error-bg');
                                        yMinC.removeClass('ok-bg');
                                        yMaxC.removeClass('ok-bg');
                                        return null;
                                    }

                                    return cases(RUNTIME.ffi.isOption, 'Option',
                                        RUNTIME.string_to_number(numSamplesC.val()), {
                                        none: function () {
                                            numSamplesC.addClass('error-bg');
                                            numSamplesC.removeClass('ok-bg');
                                            return null;
                                        },
                                        some: function (numSamplesVal) {
                                            numSamplesC.removeClass('error-bg');
                                            numSamplesC.addClass('ok-bg');

                                            if (!isTrue(RUNTIME.num_is_integer(numSamplesVal)) ||
                                                jsnums.lessThanOrEqual(numSamplesVal, 1,
                                                    RUNTIME.NumberErrbacks)) {
                                                numSamplesC.addClass('error-bg');
                                                numSamplesC.removeClass('ok-bg');
                                                return null;
                                            }

                                            return {
                                                'x-min': RUNTIME.ffi.makeSome(xMinVal),
                                                'x-max': RUNTIME.ffi.makeSome(xMaxVal),
                                                'y-min': RUNTIME.ffi.makeSome(yMinVal),
                                                'y-max': RUNTIME.ffi.makeSome(yMaxVal),
                                                'num-samples': numSamplesVal
                                            };
                                        }
                                    });
                                }
                            });
                        }
                    });
                }
            });
        }
    });
}

return module.exports = {
    "check-color": /* @stopify flat */ function (val) {
        return checkColor(val);
    },
    "get-new-window": /* @stopify flat */ function (valxMinC, xMaxC,
        yMinC, yMaxC, numSamplesC) {
        return new getNewWindow(xMinC, xMaxC, yMinC, yMaxC, numSamplesC);
    }
}