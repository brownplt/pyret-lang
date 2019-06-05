var loader = require("./runtime-loader.ts");

const projectPrefix = "_project";

function getProjects() {
  var bfs = window["BrowserFS"];
  let fs = bfs.BFSRequire("fs");
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

console.log("LOADING RUNTIME FILES");
loader();
console.log("FINISHED LOADING RUNTIME FILES");
