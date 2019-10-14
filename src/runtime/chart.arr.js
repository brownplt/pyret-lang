const jsnums = require("./js-numbers.js");
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

return module.exports = {
    "check-color": /* @stopify flat */ function (val) {
        return checkColor(val);
    }
}