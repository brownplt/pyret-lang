const jsnums = require("./js-numbers.js");
const RUNTIME = require("./runtime.js");
const IMAGE = require("./image.arr.js");

const colorDb = IMAGE.colorDb;
console.log("colorDb: ", colorDb);

/* @stopify flat */
function checkColor(val) {
    let aColor = IMAGE["is-image-color"](val);
    if (colorDb.get(val)) {
        aColor = colorDb.get(val);
    }
    return aColor;
}

// TODO(tiffany): rgb2hex and getNewWindow


/* @stopify flat */
function barChart(tableFromRawArray) {
    headers = [];
    rows = [];

    for(let i = 0; i < tableFromRawArray.length; i++) {
        headers.push(tableFromRawArray[i][0]);
        rows.push(tableFromRawArray[i][1]);
    }

    return {
        "$brand": "chart",
        "chartType": "BarChart",
        "_headers": headers,
        "_rows": rows
    };
}

/* @stopify flat */
function barChartFromTable(table) {
    return {
        "$brand": "chart",
        "chartType": "BarChart",
        "_headers": table._headers,
        "_rows": table._rows
    };
}

/* @stopify flat */
function pieChart(table) {
    return {
        "$brand": "chart",
        "chartType": "PieChart",
        "_headers": table._headers,
        "_rows": table._rows
    };
}

/* @stopify flat */
function histogramChart(table) {
    return {
        "$brand": "chart",
        "chartType": "Histogram",
        "_headers": table._headers,
        "_rows": table._rows
    };
}

/* @stopify flat */


return module.exports = {
    "check-color": /* @stopify flat */ function (val) {
        return checkColor(val);
    },
    "bar-chart": /* @stopify flat */ function (tableFromRawArray) {
        return barChart(tableFromRawArray);
    },
    "bar-chart-from-table": /* @stopify flat */ function (table) {
        return barChartFromTable(table);
    },
    "pie-chart": /* @stopify flat */ function (table) {
        return pieChart(table);
    },
    "histogram-chart": /* @stopify flat */ function (table) {
        return histogramChart(table);
    }
}
