/*global d3*/ //jslint directive


d3.csv('auto-report.build10.csv', function (error, data) {

  data = data.filter(function (datum) { return datum.success === 'true'; });
  var names = data.map(function (datum) { return datum.name; });

  if (names.length % 3 !== 0) {
    throw new Error('Corrupted CSV data!');
  }

  /* names are in triplicate because of parse, load, and eval */
  var i = 0;
  var names_set = [];
  while (i < names.length) {
    names_set[i / 3] = names[i];
    i = i + 3;
  }

  /* chart_data will be an array of arrays
    of the form [[parse_hz, load_hz, eval_hz], [...], ...]
    where chart_data[i] corresponds to names_set[i]
    */
  i = 0;
  var chart_data = [];
  var make_name_filter = function (i) {
    return function (datum) {
      return datum.name === names_set[i];
    };
  };
  var get_hz = function (datum) {
    return datum.hz;
  };
  while (i < names_set.length) {
    var splice = data.filter(make_name_filter(i));
    chart_data[i] = splice.map(get_hz);
    i = i + 1;
  }

  /* the performance of the empty program will
    serve as the "gold standard" against which
    other programs will be measured
    */
  var gold_name = '0_empty.arr';
  var gold_splice = data.filter(function (datum) { return datum.name === gold_name; });
  var gold_standard = gold_splice.map(function (datum) { return datum.hz; });

  d3.select('.chart-container')
    .selectAll('div')
      .data(names_set)
    .enter().append('div')
      .text(function (d) { return d; })
      .attr('class', 'chart')
      .attr('id', function (d) { return d; });

  d3.selectAll('.chart')
    .data(chart_data)
    .each(function (d) {
      /* d is of the form
        [parse_hz, load_hz, eval_hz]
        */
      d3.select(this)
        .selectAll('div')
        .data(d)
        .enter().append('div')
        .style('width',
          function (dd, ii) {
            return 500 * dd / gold_standard[ii] + 'px';
          });
    });

  var labels = ['parse', 'load', 'eval'];
  d3.selectAll('.chart')
    .each(function () {
      d3.select(this)
        .selectAll('div')
        .data(labels)
        .text(function (d) { return d; });
    });

});