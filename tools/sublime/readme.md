This folder contains an easily-editable syntax definition (pyret.YAML-tmLanguage) and its Sublime-readable converted output (pyret.tmLanguage). 

**For users looking to get Pyret syntax highlighting in Sublime3:** simply make a copy of the tmLanguage file, and move it to the appropriate directory.

Linux:
	cp pyret.tmLanguage "/Users/USER/Library/Application Support/Sublime Text 3/Packages/User/"
	
Windows: copy the file to your equivalent of
  C:\Users\USER\AppData\Roaming\Sublime Text 3\Packages\User
  
Once the file is in this directory, Pyret should be a selectable option in the syntax chooser.


**For users looking to edit the syntax definition:** you will want to edit the YAML-tmLanguage file, then build it into the .tmLanguage file that Sublime will use to perform the syntax highlighting. 

To build the .YAML-tmLanguage file into the .tmLanguage file, install [the AAAPackageDev plugin](https://github.com/SublimeText/AAAPackageDev) using the package manager. Open the .YAML-tmLanguage file in Sublime, set the syntax to `Sublime Text Syntax Def (YAML)`, and do `Tools > Build`.

Use [PackageResourceViewer](https://github.com/skuroda/PackageResourceViewer) to modify your theme files for compatability with the Pyret style file. For help, see [this StackOverflow answer](http://stackoverflow.com/a/25691811/4283301). Right now the names defined in the syntax file are fairly arbitrary, to fit the default Monokai color scheme. If you want to standardize the naming, check out TextMate's [naming conventions](http://manual.macromates.com/en/language_grammars#naming_conventions). 

