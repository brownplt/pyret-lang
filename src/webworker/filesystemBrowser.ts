// filesystemBrowser.ts
//
// Provides functionality for displaying an interactive file system
// viewer on a webpage.

// Creates a list item for the file system viewer. If the
// item is clicked on it expands to either a nested list of
// items (if path refers to a directory), or the contents
// of a file (if path refers to a file).
//
// theFS: the BrowserFS object to use. It can be created with
//        BrowserFS.BFSRequire("fs").
// path: the root node of the file system viewer, likely "/".
export function createFileNode(theFs: any, path: string): HTMLLIElement {
  const fileNode = document.createElement("li");

  theFs.stat(path, (err, stats) => {
    if (err) {
      throw err;
    }

    if (stats.isDirectory()) {
      if (path !== "/") {
        path = path + "/";
      }

      fileNode.onclick = (e) => {
        e.stopPropagation();

        if (fileNode.children.length === 0) {
          const children = document.createElement("ul");
          fileNode.appendChild(children);

          theFs.readdir(path, (err, files) => {
            if (err) {
              throw err;
            }

            files.forEach((file) => {
              const child = createFileNode(theFs, path + file);
              children.appendChild(child);
            });
          });
        } else {
          while (fileNode.firstChild) {
            fileNode.removeChild(fileNode.firstChild);
          }
          fileNode.innerHTML = path;
        }
      };
    } else if (stats.isFile()) {
      fileNode.onclick = (e) => {
        e.stopPropagation();

        if (fileNode.children.length === 0) {
          const readFileOptions = {
            encoding: "utf-8",
          };
          theFs.readFile(path, readFileOptions, (err, contents) => {
            if (err) {
              throw err;
            }

            const overflowDiv = document.createElement("div");
            fileNode.appendChild(overflowDiv);
            overflowDiv.style["overflow-y"] = "scroll";
            overflowDiv.style["max-height"] = "200px";
            overflowDiv.style["border"] = "2px solid black";
            overflowDiv.style["padding"] = "5px";
            overflowDiv.style["font-family"] = "monospace";
            const codeElement = document.createElement("p");
            overflowDiv.appendChild(codeElement);
            codeElement.style["white-space"] = "pre-wrap";
            codeElement.innerHTML = contents;
          });
        } else {
          while (fileNode.firstChild) {
            fileNode.removeChild(fileNode.firstChild);
          }
          fileNode.innerHTML = path;
        }
      };
    }
  });

  fileNode.innerHTML = path;

  return fileNode;
};
