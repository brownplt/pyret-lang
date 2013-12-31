function showCode(container, getCode, options) {
  if (options.run) {
    var run = drawCodeRunButton();
    run.on("click", function() {
      options.run(cm.getValue(), {}, {check: true});
    });
    container.append(run);
  }
  if (!options.run) { options.run = function() { /* intentional no-op */ }; }

  var cm = makeEditor(container, {
    cmOptions: { readOnly: options.readOnly },
    initial: "(Fetching contents...)",
    run: options.run
  });

  getCode(function(code) {
    cm.setValue(code);
  });
}

function studentCodeReview(container, options) {
  var crContainer = drawCodeReviewContainer();
  container.append(crContainer);
  showCode(
    crContainer,
    options.lookupCode,
    {
      readOnly: true,
      run: options.run
    }
  );
  writeReviews(
    crContainer,
    merge(options.reviewOptions, { noResubmit: true })
  );
}

function writeReviews(container, options) {

  var showReview = drawShowWriteReview();
  var reviewContainer = drawWriteReviewContainer();

  var statements = reviewStatements[options.type];
  var correctnessScale = drawLikert(
      statements[0],
      options.type + "-correct"
    );
  var designScale = drawLikert(
      statements[1],
      options.type + "-design"
    );
  var correctnessComments = drawLikertJustification();
  var designComments = drawLikertJustification();

  var submitReviewButton = drawSubmitReviewButton()
    .on("click", function(e) {
      var currentDesignScore = getScore(designScale);
      var currentCorrectnessScore = getScore(correctnessScale);
      if (currentDesignScore === undefined) {
        markInvalidReviewScore(designScale);
      }
      if (currentCorrectnessScore === undefined) {
        markInvalidReviewScore(correctnessScale);
      }
      if (currentDesignScore && currentCorrectnessScore) {
        markOkReviewScore(designScale);
        markOkReviewScore(correctnessScale);
        if (ct_confirm("Are you sure you want to submit this review?")) {
          options.reviews.save({
            review: {
              done: true, // NOTE(joe, 25 Jul 2013): This is a client UI hint, not binding
              correctnessComments: getReviewText(correctnessComments),
              designComments: getReviewText(designComments),
              design: currentDesignScore,
              correctness: currentCorrectnessScore
            }
          },
          function() {
            drawSavedNotification(container);
          });
        }
      } else {
        ct_log("Invalid review");
      }
    });

  var reviewText = drawReviewText(false);

  var textSubmitContainer = drawReviewTextSubmitContainer();
  textSubmitContainer
    .append(correctnessScale)
    .append(correctnessComments)
    .append(designScale)
    .append(designComments)
    .append(submitReviewButton);

  reviewContainer.append(textSubmitContainer);
  container.append(reviewContainer);
}

function repeat(n, s) {
  var str = "";
  for(var i = 0; i < n; i++) {
    str += s;
  }
  return str;
}

function createEditor(cm, uneditables, options) {
  ct_log("Creating: ", cm, uneditables, options);
  var doc = cm.getDoc();
  var end = doc.setBookmark({line: 0, ch: 0}, {insertLeft: true});
  var i = 0;
  var marks = [];
  var disabled_regions = {};
  function forLines(start, end, f) {
    for (var lineNumber = start; lineNumber < end + 1; lineNumber++) {
      f(lineNumber);
    }
  }
  function disableLines(start, end) {
    forLines(start, end, function(lineNumber) {
      cm.addLineClass(lineNumber, 'background', 'cptteach-fixed');
    });
  }
  function enableLines(start, end) {
    forLines(start, end, function(lineNumber) {
      cm.removeLineClass(lineNumber, 'background', 'cptteach-fixed');
    });
  }
  uneditables.forEach(function(u) {
    var oldEnd = end.find();
    var isFirst = (i === 0);
    var isLast = (i === uneditables.length - 1);
    doc.replaceRange(uneditables[i], end.find());
    var newEnd = end.find();
    var markEnd = { line: newEnd.line, ch: newEnd.ch };
    disableLines(oldEnd.line, markEnd.line);
    marks.push(doc.markText(
      oldEnd,
      markEnd, {
        //atomic: true,
        readOnly: true,
        inclusiveLeft: isFirst,
        inclusiveRight: isLast,
        className: 'cptteach-fixed'
      }));
    i += 1;
  });

  function getIndex(indexOrName) {
    var result = useNames ? indexDict[indexOrName] : indexOrName;
    if (result === undefined) { throw "No such index: " + indexOrName; }
    return Number(result);
  }

  function setAt(indexOrName, text) {
    var i = getIndex(indexOrName);
    doc.replaceRange(text, marks[i].find().to, marks[i + 1].find().from);
  }

  function getAt(indexOrName) {
    var i = getIndex(indexOrName);
    var start = marks[i].find().to;
    var end = marks[i + 1].find().from;
    return doc.getRange(start, end);
  }

  function cm_advance_char(doc, pos) {
    var is_eol = (doc.getRange(pos, {line: pos.line}) == "");
    if (is_eol) {
      return {line: pos.line + 1, ch: 0};
    } else {
      return {line: pos.line, ch: pos.ch + 1};
    }
  }

  function cm_retreat_char(doc, pos) {
    if (pos.ch == 0 && pos.line == 0) {
      return pos;
    }
    else if (pos.ch == 0) {
      return {line: pos.line - 1, ch: 0};
    } else {
      return {line: pos.line, ch: pos.ch - 1};
    }
  }

  function lineOf(indexOrName) {
    var i = getIndex(indexOrName);
    var start = marks[i].find().to;
    return cm_advance_char(doc, start).line;
  }
  function enableAt(indexOrName) {
    if (disabled_regions[indexOrName]) {
      var region = disabled_regions[indexOrName];
      enableLines(region.find().from.line, region.find().to.line);
      region.clear()
      delete disabled_regions[indexOrName];
    }
  }

  var readOnlyOptions = {
      readOnly: true,
      inclusiveLeft: true,
      inclusiveRight: true,
      className: 'cptteach-fixed'
    };

  function disableAt(indexOrName) {
    if (disabled_regions[indexOrName] === undefined) {
      var i = getIndex(indexOrName);
      var start = marks[i].find().to;
      var end =  marks[i + 1].find().from;
      var region = doc.markText(start, end, readOnlyOptions);
      disableLines(start.line, end.line);
      disabled_regions[indexOrName] = region;
    }
  }

  var indexDict = {};
  var useNames = options.hasOwnProperty("names");
  if (useNames) {
    if (options.names.length !== marks.length - 1) {
      throw "Wrong number of names for regions: " +
            options.names +
            ", " +
            uneditables;
    }
    var i = 0;
    options.names.forEach(function(n) {
      indexDict[n] = i;
      i += 1;
    });
  }

  var hasInitial = options.hasOwnProperty("initial");
  if (hasInitial) {
    Object.keys(options.initial).forEach(function(k) {
      setAt(k, options.initial[k]);
    });
  }

  var allRegion = false;
  function disableAll() {
    if (!allRegion) {
      var start = marks[0].find().from;
      var end = marks[marks.length - 1].find().to;
      allRegion = doc.markText(start, end, readOnlyOptions);
    }
  }
  function enableAll() {
    if (!allRegion) { return; }
    allRegion.clear();
    allRegion = false;
  }

  var lineWidgets = {};
  function addWidgetAt(indexOrName, dom, options) {
    var i = getIndex(indexOrName);
    var atTop = false;
    if (options && options.atTop) {
      atTop = options.atTop;
    }

    // NOTE(dbp 2013-08-06): `above` changes not only the immediate
    // position, but also the line where the widget is attached.
    var target;
    if (atTop) {
      target = marks[i].find().to.line;
    } else {
      target = cm_advance_char(cm.doc, marks[i + 1].find().from).line;
    }

    var lw = cm.addLineWidget(target, dom, {above: !atTop});
    if (!lineWidgets[indexOrName]) {
      lineWidgets[indexOrName] = [lw];
    } else {
      lineWidgets[indexOrName].push(lw);
    }
    return lw;
  }

  function clearWidgetAt(indexOrName, widget) {
    if (lineWidgets[indexOrName]) {
      var na = [];
      lineWidgets[indexOrName].forEach(function (lw) {
        if (lw === widget) {
          widget.clear();
        } else {
          na.push(lw);
        }
      });
      lineWidgets[indexOrName] = na;
    }
  }

  function clearAllWidgetsAt(indexOrName) {
    if (lineWidgets[indexOrName]) {
      lineWidgets[indexOrName].forEach(function (lw) {
        lw.clear();
      });
      delete lineWidgets[indexOrName];
    }
  }

  return {
    cm: cm,
    setAt: setAt,
    getAt: getAt,
    addWidgetAt: addWidgetAt,
    clearWidgetAt: clearWidgetAt,
    clearAllWidgetsAt: clearAllWidgetsAt,
    lineOf: lineOf,
    enableAt: enableAt,
    disableAt: disableAt,
    disableAll: disableAll,
    enableAll: enableAll
  };
}


function steppedEditor(container, uneditables, options) {

  var currentSectionTitle = drawCurrentStepTitle();
  container.append(currentSectionTitle);

  options.partGutterCallbacks = options.partGutterCallbacks || {};

  var gutterId = "steppedGutter";
  var partGutter = "steppedGutterPart";
  var steps = options.steps || [];
  var pos = 0;
  var cur = 0;
  var done = options.done ? true : false;

  var progress = progressBar(container, steps.length);

  var cmOptions = {};
  if (options.cmOptions) {
    cmOptions = options.cmOptions;
  }

  var cm = makeEditor(
    $(container),
    {
      initial: "",
      run: options.run,
      cmOptions:  merge({ gutters: [partGutter, gutterId]}, cmOptions)
    }
  );

  // NOTE(dbp 2013-08-05): we extract the unedible 'dom' elements, and
  // place them carefully.
  var codeUneditables = [];
  var domUneditables = {};
  var domOffset = 0;
  uneditables.forEach(function (ue, index) {
    if (ue.type === "code") {
      codeUneditables.push(ue.value);
    } else if (ue.type === "dom") {
      // NOTE(dbp 2013-08-05): We get more out of sync with
      // options.names with each dom uneditable.
      domOffset += 1;
      var i;
      if (index === 0) {
        i = 0;
      } else {
        i = index - domOffset;
      }
      push_set(domUneditables, options.names[i], ue.value);
    } else {
      ct_error("steppedEditor: got an uneditable I can't understand: ", ue);
    }
  });

  var editor = createEditor(cm, codeUneditables, {
      names: options.names,
      initial: options.initial
  });


  function switchTo(i) {
    if (i > pos) { pos = i; }
    cur = i;
    // NOTE(dbp 2013-08-16): Without this, drawing causes a jump,
    // due to something being erased and recreated.
    container.css("height", container.innerHeight() + "px");
    draw();
    container.css("height", "");
  }

  var instructionWidgets = [];
  var domUneditableWidgets = [];

  function getCurrentStepName() {
    return options.overrideSectionName || steps[cur];
  }

  var ephemeralWidgets = [];
  function draw() {
    setCurrentStepTitle(currentSectionTitle, getCurrentStepName());
    cm.clearGutter(gutterId);
    cm.clearGutter(partGutter);
    progress.set(done ? pos + 1 : pos);

    ephemeralWidgets.forEach(function(ew) {
      editor.clearWidgetAt(ew[0], ew[1]);
    });
    ephemeralWidgets = [];

    options.names.forEach(function (e) {
      if (domUneditables[e]) {
        var doms = domUneditables[e];
        doms.forEach(function (dom) {
          var widget = editor.addWidgetAt(e, dom, {atTop: true});
          ephemeralWidgets.push([e, widget]);
        });
      }
    });

    steps.forEach(function(e, i) {
      if (options.drawPartGutter) {
        options.drawPartGutter(e, function(gutterElement) {
          cm.setGutterMarker(
              editor.lineOf(e),
              partGutter,
              gutterElement
          );
        });
      }
      if (i === cur) {
        var isSubmittable = cur === pos && !done;
        var marker = drawCurrentStepGutterMarker(isSubmittable);
        if (isSubmittable) {
          var submitButton = drawSubmitStepButton(steps[pos]);
          submitButton.on("click", function () {
            if (ct_confirm("Are you sure you want to submit this part for review?")) {
              submitButton.hide();
              if (options.afterHandlers &&
                  options.afterHandlers[steps[pos]]) {
                options.afterHandlers[steps[pos]](editor, resume);
              } else {
                resume();
              }
              return false;
            }
          });
          var submitW = editor.addWidgetAt(e, submitButton[0], {atTop: false});
          ephemeralWidgets.push([e, submitW]);
        }
        cm.setGutterMarker(editor.lineOf(e),
                           gutterId,
                           marker[0]);
        editor.enableAt(e);

        if (options.instructions && options.instructions[e]) {
          var dom = drawInstructionsWidget(options.instructions[e])[0];
          var widget = editor.addWidgetAt(e, dom, {atTop: true});
          ephemeralWidgets.push([e, widget]);
        }
      } else {
        if (i <= pos) {
          var marker = drawSwitchToStepGutterMarker(i+1);
          $(marker).on("click", function (e) {
            switchTo(i);
            return false;
          });
        } else {
          var marker = drawInactiveStepGutterMarker(i+1);
        }
        cm.setGutterMarker(editor.lineOf(e),
                           gutterId,
                           marker);
        editor.disableAt(e);
      }
    });
  };
  draw();

  function resume() {
    if (pos < steps.length - 1) {
      if (cur === pos) {
        cur++;
      }
      pos++;
    }
    else {
      done = true;
    }
    switchTo(cur);
  }

  return merge(editor, {
    resumeAt: function(step) {
      switchTo(_.indexOf(steps, step));
    },
    advanceFrom: function(step) {
      var nextStep = _.indexOf(steps, step) + 1;
      if (nextStep >= steps.length) {
        done = true;
        switchTo(steps.length - 1);
      } else {
        switchTo(nextStep);
      }
    }
  });
}


function progressBar(container, numberSteps) {
  var progressContainer = drawProgressContainer();

  var percentPerStep = 80 / numberSteps;
  var steps = [];
  _.times(numberSteps, function () {
    var step = drawProgressStep(percentPerStep);
    steps.push(step);
    progressContainer.append(step);
  });

  function setCurrentStep(n) {
    for (var i = 0; i < steps.length; i++) {
      if (i < n) {
        steps[i].addClass("done");
      } else {
        steps[i].removeClass("done");
      }
    }
    if (n === steps.length) {
      progressContainer.empty();
      progressContainer.append(drawProgressDone());
    }
  }

  container.append(progressContainer);

  return {
    set: setCurrentStep
  };
}

var reviewTabs = ctC("reviewTabs", [TObject, {hasField: "type"}, TFunction],
  function (tabPanel, step, resume) {
    function setupReviews(reviewData) {
      var doneCount = 0;
      function incrementDone() {
        doneCount += 1;
        tryFinishReview();
      }
      function tryFinishReview() {
        if(doneCount === reviewData.length) {
          resume();
        }
      }
      tryFinishReview(); // If reviewData is empty, just be done

      reviewData.forEach(function(reviewDatum) {
        reviewDatum.getReview(incrementDone, function(/* notFound */) {
            var reviewsTab = drawReviewsTab();
            var editorContainer = drawReviewEditorContainer();
            reviewsTab.append(editorContainer);
            // NOTE(dbp 2013-08-16): Fix the min-height until it loads,
            // so if the tab panel is at the end of the page, we don't
            // jump.
            editorContainer.css("min-height",
                                window.innerHeight + "px");
            var reviewTabHandle =
              tabPanel.addTab("Reviews", reviewsTab, { cannotClose: true,
                                                       prioritize: true });


            reviewDatum.attachWorkToReview(editorContainer, function(reviewsInline) {
              writeReviews(reviewsInline, {
                  type: step.type,
                  reviews: {
                      save: function(val, f) {
                        reviewDatum.saveReview(val, function(feedback) {
                            var hasCanned = false;
                            function finishReview() {
                              reviewTabHandle.close();
                              incrementDone();
                              finishReview = function() {};
                            }
                            if(feedback && feedback.length > 0) {
                              feedback.forEach(function(fb) {
                                ct_log("fb, reviewD", fb, reviewDatum);
                                if (fb.canned && fb.submission_id === reviewDatum.submission_id) {
                                  hasCanned = true;
                                  drawModal(fb.message, finishReview);
                                }
                              });
                              if (!hasCanned) {
                                finishReview();
                              }
                            }
                            else {
                              finishReview();
                            }
                          },
                          function(e) {
                            // TODO(joe 31 July 2013): Just let them move on if
                            // this fails?
                            ct_error("Saving review failed:", e);
                          });
                      },
                      lookup: function(f) { f(null); }
                    }
                });
            }, function(e) {
              ct_error("Work for review not found: ", e);
            });
          });
      });
    }
    // TODO(joe Aug 1 2013): To consider: is empty reviews the right "not found" behavior?
    step.getReviewData(setupReviews, function() { setupReviews([]); });

  });


function makeHighlightingRunCode(codeRunner) {

  return function(src, uiOptions, options) {
    function highlightLineAt(cm, loc, className) {
      cm.addLineClass(
          loc.line - 1,
          'background',
          className
      );
    }
    function normalizeLoc(loc) {
      if(loc.path) { return { file: loc.path, line: loc.line, column: loc.column }; }
      if(loc.source) { return { file: loc.source, line: loc.line, column: loc.column }; }
      else if(!loc.file) {
        ct_exn("normalizeLoc: Doesn't look like a valid location", loc);
      }
      else { return loc; }
    }
    function showLink(loc) {
      return loc && loc.line > 0 && loc.file && loc.file === uiOptions.name;
    }
    function makeScrollingLocationLink(cm, loc) {
      function locToStr(loc) {
        return "In " + loc.file + ": Line " + loc.line + ", Column " + loc.column;
      }
      loc = normalizeLoc(loc);
      if(!showLink(loc)) { return $("<span>"); }
      var errorLink = $("<a>");
      errorLink.text(locToStr(loc));
      errorLink.attr("href", "#");
      errorLink.on("click", function(e) {
        clear();
        highlightLineAt(uiOptions.cm, loc, 'lineError');
        var coords = cm.charCoords({ line: loc.line - 1, ch: loc.column - 1 });
        if ((window.pageYOffset > coords.top) ||
            (window.pageYOffset < coords.top - window.innerHeight)) {
          $("body").animate({
            scrollTop: coords.top - (window.innerHeight / 2)
          });
        }
        e.preventDefault();
        return false;
      });
      return errorLink;
    }

    function clear() {
      uiOptions.cm.eachLine(function(l) {
        uiOptions.cm.removeLineClass(l, 'background', 'lineError');
        uiOptions.cm.removeLineClass(l, 'background', 'lineSuccess');
      });
    };

    uiOptions.cm.on("change", clear);

    function highlightingOnError(output) { return function(err) {
      ct_log(err);
      if (err.locs) {
        output.append($("<div>").text(err.message));
        err.locs.forEach(function(l) {
          output.append($("<div>").append(makeScrollingLocationLink(uiOptions.cm, l)));
        });
      }
      else if (err.racketError) {
        ct_log("Error is: ", err.racketError);
        var cms = err.racketError._fields[1];
        var loc = err.racketError._fields[2];
        whalesongFFI.callRacketFun(
            whalesongFFI.getPyretLib("p:mk-exn"),
            [err.racketError],
            function(pErr) {
              var err = pyretMaps.pyretToJSON(pErr);
              ct_log("I expect a pyret error as json here: ", err);
              var messageText;
              if (err.value && err.value.message) {
                messageText = err.value.message;
              }
              else if (err.value) {
                messageText = err.value;
              }
              else {
                messageText = "Runtime error";
              }
              var loc = { file: err.path, line: err.line, column: err.column };
              var errorLink = makeScrollingLocationLink(uiOptions.cm, loc);
              var errDom = drawErrorMessageWithLoc(messageText, errorLink);
              output.append(errDom);
              var traceDom = drawErrorLocations(
                err.trace.map(function (l) { return makeScrollingLocationLink(uiOptions.cm,l) }));
              output.append(traceDom);
            },
            function(e) {
              ct_error("Couldn't get trace: ", e);
              ct_log(err.racketError);
              var errDom = drawErrorMessage("Error: " + err.racketError._fields[0]);
              output.append(errDom);
            }
          );
      }
      else if (err.message) {
        output.append($("<div>").text(err.message));
      }
    };}

    function highlightingCheckReturn(output) { return function(obj) {
      var successCount = 0;
      function drawSuccess(name, message, location) {
        var link = $("<span>");
        successCount += 1;
        if(location.value) {
          highlightLineAt(uiOptions.cm, location.value, 'lineSuccess');
          link = makeScrollingLocationLink(uiOptions.cm, location.value);
        }
        return $("<div>").text(name +  ": " + message)
          .addClass("check check-success")
          .append("<br/>");
      }
      function drawFailure(name, message, location) {
        var link = $("<span>");
        if(location.value) {
          highlightLineAt(uiOptions.cm, location.value, 'lineError');
          link = makeScrollingLocationLink(uiOptions.cm, location.value);
        }
        return $('<div>').text(name + ": " + message)
          .addClass("check check-failure")
          .append("<br/>")
          .append(link)
          .append("<br/>");
      }
      function drawException(name, exception, location) {
        if(typeof exception === "string") {
          return $('<div>')
            .addClass("check check-failure")
            .append($("<p>").text(name))
            .append($("<p>").text(exception))
            .append("<br/>")
            .append($("<span>").text("In check starting at:"))
            .append("<br/>")
            .append(makeScrollingLocationLink(uiOptions.cm, location.value));
        }
        var link = $("<span>");
        var loc = exception.trace[0] || location.value;
        if(loc) {
          highlightLineAt(uiOptions.cm, location.value, 'lineError');
          link = makeScrollingLocationLink(uiOptions.cm, location.value);
        }
        var traceDom = drawErrorLocations(
           exception.trace.map(function (l) {
             return makeScrollingLocationLink(uiOptions.cm,l)
           }));
        return $('<div>').text(name + ": " + exception.message)
          .addClass("check check-failure")
          .append("<br/>")
          .append($("<span>").text("In check starting at:"))
          .append("<br/>")
          .append(makeScrollingLocationLink(uiOptions.cm, location.value))
          .append("<br/>")
          .append($("<span>").text("Using (at least) these lines:"))
          .append("<br/>")
          .append(traceDom)
          .append("<br/>");
      }

      var blockResultsJSON = pyretMaps.pyretToJSON(obj);

      if(blockResultsJSON.results.length === 0) {
        whalesongFFI.callPyretFun(
            whalesongFFI.getPyretLib("torepr"),
            [pyretMaps.get(pyretMaps.toDictionary(obj), "val")],
            function(s) {
              var str = pyretMaps.getPrim(s);
              output.append(jQuery("<pre class='repl-output'>").text(str));
              output.append(jQuery('<br/>'));
            }, function(e) {
              ct_err("Failed to tostring: ", result);
            });
        return true;
      }

      var somethingFailed = false;
      blockResultsJSON.results.map(function(result) {
        result.map(function(checkBlockResult) {
          var container = $("<div>");
          var errorLink;
          var name = checkBlockResult.name;
          container.append($("<p>").text(name + ": Avast, there be bugs!"));
          container.addClass("check-block");
          var messageText = "";
          console.log(checkBlockResult);
          if (checkBlockResult.err) {
            somethingFailed = true;
            if (checkBlockResult.err.message) {
              messageText = checkBlockResult.err.message;
            }
            else {
              messageText = checkBlockResult.err;
            }
            if(checkBlockResult.err.trace) {
              var loc = checkBlockResult.err.trace[0] || checkBlockResult.err.location;
              if(loc) {
                errorLink = makeScrollingLocationLink(uiOptions.cm, loc);
                container.append(drawErrorMessageWithLoc(messageText, errorLink));
              }
              else {
                container.append(drawErrorMessage(messageText));
              }
            } else {
              container.append(drawErrorMessage(messageText));
            }
            container.addClass("check-block-failed");
          }
          checkBlockResult.results.forEach(function(individualResult) {
            if ("reason" in individualResult) {
              somethingFailed = true;
              container.append(
                drawFailure(
                    individualResult.name,
                    individualResult.reason,
                    individualResult.location
                  ));
            } else if ("exception" in individualResult) {
              somethingFailed = true;
              container.append(
                drawException(
                    individualResult.name,
                    individualResult.exception,
                    individualResult.location
                  ));
            } else {
              var successDiv = drawSuccess(
                  individualResult.name,
                  "Success!",
                  individualResult.location
              );
              if(!individualResult.location.value) {
                container.append(successDiv);
              }
            }
          });
          if(somethingFailed) {
            output.append(container);
          }
        });
      });
      if(!somethingFailed) {
        var container = $("<div>")
          .addClass("check-block")
          .addClass("check-block-success");
        var text = "";
        if(successCount === 1) {
          text = "Your test passed, mate!";
        }
        else {
          text = "All " + successCount + " tests passed, mate!";
        }
        container.append($("<p>").text(text));
        output.append(container);
      }
      return true;
    };}


    var theseUIOptions = merge(uiOptions, {
        wrappingOnError: highlightingOnError
    });
    if (options.check) {
      theseUIOptions.wrappingReturnHandler = highlightingCheckReturn;
    }
    clear();
    codeRunner(src, theseUIOptions, options);
  }
}

function autoSaver(container, options) {
  // save every 5 seconds, if things have been edited
  var minSaveInterval = 5000;
  var statusElt = $("<span>").text("Saved").addClass("saved");
  var goingToSave = false;
  var setSaving = function() {
    statusElt.text("Saving...").removeClass("saved").addClass("saving");
    goingToSave = true;
  };
  var setSaved = function() {
    statusElt.text("Saved").addClass("saved").removeClass("saving");
    goingToSave = false;
  };
  var setFailure = function() {
    statusElt.text("Unable to save.");
    goingToSave = false;
  };

  container.append(statusElt);

  return {
    onEdit: function() {
      if(!goingToSave) {
        setSaving();
        setTimeout(function() {
          options.save(function() {
              setSaved();
            },
            function() {
              setFailure();
            });
        }, minSaveInterval);
      }
    }
  };

}


function createTabPanel(container, options) {
  var tabContainer = $("<div>").addClass("tabPanel");
  var panelRow = $("<div>").addClass("tabPanels");
  var current = false;
  var tabs = [];
  var titleRow = $("<div>").addClass("tabTitles");

  var minimize;
  var maximize;
  if (options && options.maximizable) {
    var maximize = drawPanelMaximizeButton();
    maximize.html("&larr;");
    maximize.click(function () {
      if (options.minimize) {
        minimize.toggle();
      }
      maximize.toggleClass("button-activated");
      if (maximize.hasClass("button-activated")) {
        maximize.html("&rarr;");
      } else {
        maximize.html("&larr;");
      }
      container.toggleClass("maximized");
    });
    tabContainer.append(maximize);
  }
  if (options && options.minimize) {
    var minimize = drawPanelMinimizeButton();
    minimize.html("&rarr;");
    minimize.click(function () {
      if (options.maximizable) {
        maximize.toggle();
      }
      minimize.toggleClass("button-activated");
      if (minimize.hasClass("button-activated")) {
        minimize.html("&larr;");
      } else {
        minimize.html("&rarr;");
      }
      container.toggleClass("minimized");
      $(options.minimize).toggleClass("maximized");
    });
    tabContainer.append(minimize);
  }

  function switchToCurrent() {
    tabContainer.find(".tab").hide();
    tabContainer.find(".tabTitle").removeClass("currentTab");
    if (current.tab && current.title) {
      current.tab.show();
      current.title.addClass("currentTab");
      // NOTE(dbp 2013-08-06): This is somewhat of a hack - Codemirror
      // instances can't really work until they are shown, and need
      // a manual refresh() (or mouse / keyboard input) to work.
      current.tab.find(".CodeMirror").each(function(_, cmDom) {
        cmDom.CodeMirror.refresh();
      });
    }
  }
  tabContainer.append(titleRow).append(panelRow);
  container.append(tabContainer);
  return {
    addTab: function(title, dom, inputOptions)
    /*: String, Dom, { cannotClose: Bool, prioritize: Bool } -> Undef */
    {
      var options = inputOptions ? inputOptions : {};
      function switchHere() {
        current = { tab: tab, title: title };
        switchToCurrent();
      }
      var tab = $("<div>").addClass("tab").append(dom);
      var title = $("<div>")
        .addClass("tabTitle")
        .text(title)
        .on("click", switchHere);
      var tabData = {title: title, tab: tab, index: tabs.length};
      if (options.prioritize) {
        tabData.prioritize = true;
      } else {
        tabData.prioritize = false;
      }
      tabs.push(tabData);
      function close() {
        tab.remove();
        title.remove();
        ct_log("tabs: ", tabs);
        tabs = tabs.filter(function (tabStructure) {
          return tabStructure.tab !== tab;
        });

        if (current.tab.length > 0 && current.tab[0] === tab[0]) {
          var newTab = _.find(tabs,
                              function (t) { return t.prioritize });
          ct_log("tab: ", newTab);
          if (!newTab) {
            newTab = tabs[0];
          }
          newTab = newTab || {};
          current = {
            tab: newTab.tab,
            title: newTab.title
          };
          switchToCurrent();
        }
      }
      if(!options.cannotClose) {
        var closeButton = $("<span>×</span>").addClass("closeTab").on("click", close);
        title.append(closeButton);
      }

      titleRow.append(title);
      panelRow.append(tab);

      switchHere();

      return { close: close };
    },
    container: tabContainer
  };
}

function readOnlyEditorFromParts(container,
                                 delimiterValues,
                                 parts,
                                 sharedOptions) {
  var cm = makeEditor(
    container,
    merge({
      initial: "",
      run: makeLoggingRunCode(makeHighlightingRunCode(RUN_CODE), "review-editor")
    }, sharedOptions)
  );
  var thisEditorOptions = merge(sharedOptions, {
    initial: parts
  });

  var editor = createEditor(cm, delimiterValues, thisEditorOptions);
  editor.disableAll();
  return editor;
}


function showReview(editor,
                    step,
                    review,
                    feedback,
                    saveFeedback,
                    abuseData) {

  var container = drawReviewContainer();

  var review = drawReview(review, step.type, abuseData.review);

  container.append(review);

  if (feedback === null) {
    var feedbackDiv = drawFeedback(function (score, comments) {
      var feedback = {
        helpfullness: score,
        comments: comments
      };
      saveFeedback(feedback, function () {
        feedbackDiv.remove();
        container.append(drawSubmittedFeedback(feedback, abuseData.feedback));
      }, function () {
        ct_error("Couldn't save feedback");
      });
    });

    container.append(feedbackDiv);
  } else {
    container.append(drawSubmittedFeedback(feedback, abuseData.feedback));
  }

  editor.addWidgetAt(step.name, container[0]);
}
