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
    headers = ["",""];
    return {
        "$brand": "chart",
        "chartType": "BarChart",
        "_headers": headers,
        "_rows": tableFromRawArray
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
function pieChart(tableFromRawArray) {
    return {
        "$brand": "chart",
        "chartType": "PieChart",
        "_headers": ["",""],
        "_rows": tableFromRawArray
    };
}

/* @stopify flat */
function pieChartFromTable(table) {
    return {
        "$brand": "chart",
        "chartType": "PieChart",
        "_headers": table._headers,
        "_rows": table._rows
    };
}

/* @stopify flat */
function histogram(tableFromRawArray) {
    return {
        "$brand": "chart",
        "chartType": "Histogram",
        "_headers": ["",""],
        "_rows": tableFromRawArray
    };
}

/* @stopify flat */
function histogramFromTable(table) {
    return {
        "$brand": "chart",
        "chartType": "Histogram",
        "_headers": table._headers,
        "_rows": table._rows
    };
}

/* @stopify flat */
function scatterChartFromTable(table) {
    return {
        "$brand": "chart",
        "chartType": "ScatterChart",
        "_headers": table._headers,
        "_rows": table._rows
    };
}

/* @stopify flat */
function lineChartFromTable(table) {
    return {
        "$brand": "chart",
        "chartType": "LineChart",
        "_headers": table._headers,
        "_rows": table._rows
    };
}

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
    "pie-chart": /* @stopify flat */ function (tableFromRawArray) {
        return pieChart(tableFromRawArray);
    },
    "pie-chart-from-table": /* @stopify flat */ function (table) {
        return pieChartFromTable(table);
    },
    "histogram": /* @stopify flat */ function (tableFromRawArray) {
        return histogram(tableFromRawArray);
    },
    "histogram-from-table": /* @stopify flat */ function (table) {
        return histogramFromTable(table);
    },
    "scatter-chart-from-table": /* @stopify flat */ function (table) {
        return scatterChartFromTable(table);
    },
    "line-chart-from-table": /* @stopify flat */ function (table) {
        return lineChartFromTable(table);
    }
}
