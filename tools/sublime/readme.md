This folder contains an easily-editable syntax definition (pyret.YAML-tmLanguage) and its Sublime-readable converted output (pyret.tmLanguage). 

For Sublime(3) to recognize your pyret.tmLanguage file, you will need to run 

	cp pyret.tmLanguage "/Users/USER/Library/Application Support/Sublime Text 3/Packages/User/"

To build the .YAML-tmLanguage file into the .tmLanguage file Sublime will recognize, you will want to use [the AAAPackageDev plugin](https://github.com/SublimeText/AAAPackageDev).

Use [PackageResourceViewer](https://github.com/skuroda/PackageResourceViewer) to modify your theme files for compatability with the Pyret style file. For help, see [this StackOverflow answer](http://stackoverflow.com/a/25691811/4283301). Right now the names defined in the syntax file are fairly arbitrary, to fit the default Monokai color scheme. If you want to standardize the naming, check out TextMate's [naming conventions](http://manual.macromates.com/en/language_grammars#naming_conventions). 

