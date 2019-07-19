// filesystemBrowser.ts
//
// Provides functionality for displaying an interactive file system
// browser on a webpage.

// Creates a list element which can be clicked on to reveal
// the contents of a file.
//
// theFS: the BrowserFS object to use. It can be created with
//        BrowserFS.BFSRequire("fs").
// path: the path to the file.
const createFileListElement = (theFS: any, path: string): HTMLLIElement => {
  const fileListElement = document.createElement("li");

  fileListElement.innerHTML = path;

  fileListElement.onclick = (e) => {
    e.stopPropagation();

    if (fileListElement.children.length === 0) {
      const readFileOptions = {
        encoding: "utf-8",
      };
      theFS.readFile(path, readFileOptions, (err, contents) => {
        if (err) {
          throw err;
        }

        const overflowDiv = document.createElement("div");
        fileListElement.appendChild(overflowDiv);
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
      while (fileListElement.firstChild) {
        fileListElement.removeChild(fileListElement.firstChild);
      }
      fileListElement.innerHTML = path;
    }
  };

  return fileListElement;
}

// Creates a list element which can be clicked on to reveal a
// nested list of file system objects.
//
// theFS: the BrowserFS object to use. It can be created with
//        BrowserFS.BFSRequire("fs").
// path: the path to the directory.
const createDirectoryListElement = (theFS: any, path: string): HTMLLIElement => {
  const directoryListElement = document.createElement("li");

  directoryListElement.innerHTML = path;

  if (path !== "/") {
    path = path + "/";
  }

  directoryListElement.onclick = (e) => {
    e.stopPropagation();

    if (directoryListElement.children.length === 0) {
      const children = document.createElement("ul");
      directoryListElement.appendChild(children);

      theFS.readdir(path, (err, files) => {
        if (err) {
          throw err;
        }

        files.forEach((file) => {
          const child = createListElement(theFS, path + file);
          children.appendChild(child);
        });
      });
    } else {
      while (directoryListElement.firstChild) {
        directoryListElement.removeChild(directoryListElement.firstChild);
      }
      directoryListElement.innerHTML = path;
    }
  };

  return directoryListElement;
}

// Creates a list element representing either a file or a directory. If the path
// is to a file, the element can be clicked on to reveal the contents of the
// file. If the path is to a directory, clicking on it reveals a nested list of
// file system objects.
//
// theFS: the BrowserFS object to use. It can be created with
//        BrowserFS.BFSRequire("fs").
// path: a path string to either a file or a directory. Use "/" for the root
//       node of the file system.
export const createListElement = (theFs: any, path: string): HTMLLIElement => {
  let listElement: HTMLLIElement;

  theFs.stat(path, (err, stats) => {
    if (err) {
      throw err;
    }

    if (stats.isDirectory()) {
      listElement = createDirectoryListElement(theFs, path);
    } else if (stats.isFile()) {
      listElement = createFileListElement(theFs, path);
    }
  });

  return listElement;
};

// Creates a file system browser and inserts it into the webpage as a child of
// element.
//
// theFS: the BrowserFS object to use. It can be created with
//        BrowserFS.BFSRequire("fs").
// path: a path string to either a file or a directory. Use "/" for the root
//       node of the file system.
export const createBrowser = (theFS: any, path: string, element: HTMLUListElement): void => {
  element.appendChild(createListElement(theFS, path));
}
