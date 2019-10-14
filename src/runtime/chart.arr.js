const jsnums = require("./js-numbers.js");
const IMAGE = require("./image.arr.js");

const colorDb = Image.colorDb;

function checkColor(val) {
    let aColor = IMAGE["is-image-color"](val);
    if (colorDb.get(aColor)) {
        aColor = colorDb.get(aColor);
    }
    return aColor;
}

var testPrint = /* @stopify flat */ function (x) {
    return x;
}
var addOne = /* @stopify flat */ function (x) {
    return x + 1;
}

return module.exports = {
    "test-print": /* @stopify flat */ function (num) {
        return new testPrint(jsnums.toFixnum(num));
    },
    "add-one": /* @stopify flat */ function (num) {
        return new addOne(jsnums.toFixnum(num));
    },

    "check-color": /* @stopify flat */ function (val) {
        return checkColor(val);
    }
}