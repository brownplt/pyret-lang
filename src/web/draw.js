

function drawRunButton() {
  return $("<div>")
    .addClass("runButton")
    .addClass("blueButton")
    .html("run &rarr;");
}

function drawResetButton() {
  return $("<div>")
    .addClass("blueButton")
    .addClass("resetButton")
    .html("reset editor");
}

function drawClearFix() {
  return $("<div>").addClass("clearfix");
}

function drawErrorMessageWithLoc(message, link) {
  var errorMessage = $("<span>").text(message);
  return $("<div>").addClass("errorMessage")
    .append(link.addClass("errorLocation"))
    .append("<span>:&nbsp;</span>")
    .append(errorMessage);
}

function drawErrorMessage(message) {
  var errorMessage = $("<span>").text(message);
  return $("<div>").addClass("errorMessage")
    .append(errorMessage);
}

function drawErrorLocations(links) {
  var container = $("<div>").addClass("errorLocations");
  links.forEach(function(l) {
    container.append($("<div>").addClass("errorLocation")
                               .append(l));
  })
  return container;
}

function drawPromptArrow() {
  return jQuery("<span>&gt;&nbsp;</span>").addClass("repl-prompt-arrow");
}
