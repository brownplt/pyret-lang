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

// TODO(tiffany): rgb2hex and getNewWindow

function barChart(table) {
    return {
        "$brand": "chart",
        "chartType": "BarChart",
        "_headers": table._headers,
        "_rows": table._rows
    };
}

function pieChart(table) {
    return {
        "$brand": "chart",
        "chartType": "PieChart",
        "_headers": table._headers,
        "_rows": table._rows
    };
}

return module.exports = {
    "check-color": /* @stopify flat */ function (val) {
        return checkColor(val);
    },
    "bar-chart": /* @stopify flat */ function (table) {
        return barChart(table);
    },
    "pie-chart": /* @stopify flat */ function (table) {
        return pieChart(table);
    }
}
