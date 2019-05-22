var loader = require("./runtime-loader");

const projectPrefix = "_project";

function getProjects() {
  var bfs = window["BrowserFS"];
  let fs = bfs.BFSRequire("fs");
  let files = fs.readdirSync("./");
  let projectList = [];

  /*
  files.forEach(function(file) {
    let statResult = fs.statSync(file);
    if (statResult.isDirectory()) {
      if (file.startsWith(projectPrefix)) {
        projectList.push({ file: file });   
      }
    }
  });
   */
}

function runPath(path) {
  
}

console.log("LOADING");
loader();
console.log("FINISHED LOADING");
console.log(getProjects());
console.log("FINISHED GETTING");
