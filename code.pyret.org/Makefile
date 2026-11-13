# NOTE: Needs TWO blank lines here, dunno why
define \n


endef
ifneq ($(findstring .exe,$(SHELL)),)
	override SHELL:=$(COMSPEC)$(ComSpec)
	MKDIR = $(foreach dir,$1,if not exist "$(dir)". (md "$(dir)".)$(\n))
	RMDIR = $(foreach dir,$1,if exist "$(dir)". (rd /S /Q "$(dir)".)$(\n))
	RM = if exist "$1". (del $1)
else
	MKDIR = mkdir -p $1
	RMDIR = rm -rf $1
	RM = rm -f $1
endif

NODE_MODULE = $(shell node -e "console.log(require('node:path').dirname(require.resolve('$1')))")


# These paths get special treatment: their import paths have
# trailing lib/ or mode/ or build/something at the end, but we need files from
# other paths in them
CM=$(call NODE_MODULE,codemirror)/..

# Pyret's CodeMirror mode and the Pyret language now live in this monorepo
# rather than as separate GitHub npm packages.
#
# PYRET_MODE is a LOCAL path `mode`: in the monorepo it's a gitignored symlink
# to the shared ../codemirror-mode sibling (created by the `mode` target below).
# Deploys may choose to *copy in* the codemirror-mode directory instead of
# symlinking it if they e.g. slice out just the code.pyret.org subdirectory
PYRET_MODE=mode

# The images the web build needs are copied and committed into src/web/img, so
# the `web` target has no dependency on ../lang at all -- only the compiler
# build (deploy-cpo-main / link-pyret) does.
PYRET=../lang

CPOMAIN=build/web/js/cpo-main.jarr
CPOGZ=build/web/js/cpo-main.jarr.gz.js
PHASEA=pyret/build/phaseA/pyret.jarr
COMMITID=$(shell git rev-parse --short HEAD)

.PHONY : test_node_module
test_node_module:
	@echo $(call NODE_MODULE,codemirror)
	@echo $(CM)


BUNDLED_DEPS=build/web/js/bundled-npm-deps.js

.PHONY : post-install
post-install: compress-pyret

install-link: link-pyret

.PHONY : selenium-test-local
selenium-test-local:
	TEST_LOC="local" node test/test.js test/browser

.PHONY : selenium-test-sauce
selenium-test-sauce:
	TEST_LOC="sauce" node test/test.js test/browser/pyret

build/web/editor.embed.html: src/web/editor.html .env.embed
	node make-template.js $< .env.embed > $@
build/web/editor.html: src/web/editor.html
	cp $< $@

OUT_HTML := $(patsubst src/web/%.template.html,build/web/views/%.html,$(wildcard src/web/*.template.html))

build/web/views/%.html: src/web/%.template.html
	node make-template.js $< > $@

COPY_HTML := $(patsubst src/web/%.html,build/web/views/%.html,$(wildcard src/web/*.html))

build/web/views/%.html: src/web/%.html
	cp $< $@

# Self-contained variant of the editor template: shell scripts/styles inlined so
# the page boots where its assets are served without an executable MIME type
# (see src/scripts/inline-selfcontained.js). Depends on the built shell js/css
# it inlines -- the RULE lives further down, after the COPY_*/MISC_* variables
# are assigned: make expands a rule's prerequisite list when it READS the rule,
# so a rule up here would see every one of those `:=` variables as empty and
# never rebuild the template when an inlined asset changes (fresh builds hide
# this; every incremental build served a stale webview).
SELFCONTAINED = build/web/views/editor.selfcontained.html

OUT_CSS := $(patsubst src/web/%.template.css,build/web/%.css,$(wildcard src/web/css/*.template.css))

build/web/css/%.css: src/web/css/%.template.css
	node make-template.js $< > $@

COPY_CSS := $(patsubst src/web/%.css,build/web/%.css,$(wildcard src/web/css/*.css))
build/web/css/%.css: src/web/css/%.css
	cp $< $@

COPY_LIB_CSS := $(patsubst lib/css/%.css,build/web/css/%.css,$(wildcard lib/css/*.css))
build/web/css/%.css: lib/css/%.css
	cp $< $@

COPY_LIB_CSS := $(patsubst lib/css/%.css,build/web/css/%.css,$(wildcard lib/css/*.css))
build/web/css/%.css: lib/css/%.css
	cp $< $@

COPY_LIB_IMAGES := $(patsubst lib/css/images/%.png,build/web/css/images/%.png,$(wildcard lib/css/images/*.png))
build/web/css/images/%.png: lib/css/images/%.png
	cp $< $@

COPY_THEMES := $(patsubst src/web/%.css,build/web/%.css,$(wildcard src/web/css/themes/*.css))

build/web/css/themes/%.css: src/web/css/themes/%.css
	cp $< $@

COPY_FONTS := $(patsubst src/web/%,build/web/%,$(wildcard src/web/css/fonts/*))

build/web/css/fonts/%: src/web/css/fonts/%
	cp $< $@

build/web/css/codemirror.css: $(CM)/lib/codemirror.css
	cp $< $@

build/web/css/foldgutter.css: $(CM)/addon/fold/foldgutter.css
	cp $< $@

build/web/css/dialog.css: $(CM)/addon/dialog/dialog.css
	cp $< $@

build/web/css/matchesonscrollbar.css: $(CM)/addon/search/matchesonscrollbar.css
	cp $< $@

MISC_CSS = build/web/css/codemirror.css \
	build/web/css/foldgutter.css \
	build/web/css/dialog.css \
	build/web/css/matchesonscrollbar.css

COPY_GIF := $(patsubst src/web/img/%.gif,build/web/img/%.gif,$(wildcard src/web/img/*.gif))

COPY_SVG := $(patsubst src/web/img/%.svg,build/web/img/%.svg,$(wildcard src/web/img/*.svg))

COPY_PNG := $(patsubst src/web/img/%.png,build/web/img/%.png,$(wildcard src/web/img/*.png))

build/web/img/%.gif: src/web/img/%.gif
	cp $< $@

build/web/img/%.svg: src/web/img/%.svg
	cp $< $@

build/web/img/%.png: src/web/img/%.png
	cp $< $@

COPY_JS := $(patsubst src/web/js/%.js,build/web/js/%.js,$(wildcard src/web/js/*.js))

build/web/js/%.js: src/web/js/%.js
	cp $< $@

COPY_LIB_JS := $(patsubst lib/js/%.js,build/web/js/%.js,$(wildcard lib/js/*.js))

build/web/js/%.js: lib/js/%.js
	cp $< $@

COPY_GOOGLE_JS := $(patsubst src/web/js/google-apis/%.js,build/web/js/google-apis/%.js,$(wildcard src/web/js/google-apis/*.js))

build/web/js/google-apis/%.js: src/web/js/google-apis/%.js
	cp $< $@

build/web/js/events.js: src/web/js/events.js
	cp $< $@

build/web/js/vega.min.js: $(call NODE_MODULE,vega)/vega.min.js
	cp $< $@
build/web/js/vega-tooltip.min.js: lib/js/vega-tooltip.min.js
	cp $< $@

build/web/js/snap: $(call NODE_MODULE,snap)
	mkdir -p build/web/js/snap
	cp -r $</src build/web/js/snap
	cp -r $</pyret build/web/js/snap
	cp -r $</libraries build/web/js/snap

build/web/js/transpile.xml: src/web/js/transpile.xml
	cp -r $< $@

build/web/js/beforePyret.js: src/web/js/beforePyret.js
	npx webpack

build/web/js/beforeBlocks.js: src/web/js/beforeBlocks.js
	npx webpack


build/web/js/q.js: $(call NODE_MODULE,q)/q.js
	cp $< $@

build/web/js/s-expression-lib.js: $(call NODE_MODULE,s-expression)/index.js
	cp $< $@

build/web/js/colorspaces.js: $(call NODE_MODULE,colorspaces)/colorspaces.js
	cp $< $@

build/web/js/es6-shim.js: $(call NODE_MODULE,es6-shim)/es6-shim.min.js
	cp $< $@

build/web/js/seedrandom.js: $(call NODE_MODULE,seedrandom)/seedrandom.js
	cp $< $@

build/web/js/source-map.js: $(call NODE_MODULE,source-map)/dist/source-map.js
	cp $< $@

build/web/js/url.js: $(call NODE_MODULE,url.js)/url.js
	cp $< $@

build/web/js/require.js: $(call NODE_MODULE,requirejs)/r.js
	cp $< $@

build/web/js/codemirror.js: $(CM)/lib/codemirror.js
	cp $< $@

build/web/js/rulers.js: $(CM)/addon/display/rulers.js
	cp $< $@

build/web/js/scrollpastend.js: $(CM)/addon/scroll/scrollpastend.js
	cp $< $@

build/web/js/mark-selection.js: $(CM)/addon/selection/mark-selection.js
	cp $< $@

build/web/js/runmode.js: $(CM)/addon/runmode/runmode.js
	cp $< $@

build/web/js/pyret-fold.js: $(PYRET_MODE)
	cp $(PYRET_MODE)/addon/pyret-fold.js $@

build/web/js/matchkw.js: $(PYRET_MODE)
	cp $(PYRET_MODE)/addon/matchkw.js $@

build/web/js/foldcode.js: $(CM)/addon/fold/foldcode.js
	cp $< $@

build/web/js/foldgutter.js: $(CM)/addon/fold/foldgutter.js
	cp $< $@

build/web/js/comment.js: $(CM)/addon/comment/comment.js
	cp $< $@

build/web/js/dialog.js: $(CM)/addon/dialog/dialog.js
	cp $< $@

build/web/js/search.js: $(CM)/addon/search/search.js
	cp $< $@

build/web/js/searchcursor.js: $(CM)/addon/search/searchcursor.js
	cp $< $@

build/web/js/annotatescrollbar.js: $(CM)/addon/scroll/annotatescrollbar.js
	cp $< $@

build/web/js/matchesonscrollbar.js: $(CM)/addon/search/matchesonscrollbar.js
	cp $< $@

build/web/js/jump-to-line.js: $(CM)/addon/search/jump-to-line.js
	cp $< $@

build/web/js/pyret-mode.js: $(PYRET_MODE)
	cp $(PYRET_MODE)/mode/pyret.js $@

build/web/js/mousetrap.min.js: $(call NODE_MODULE,mousetrap)/mousetrap.min.js
	cp $< $@

build/web/js/mousetrap-global-bind.min.js: $(call NODE_MODULE,mousetrap)/plugins/global-bind/mousetrap-global-bind.min.js
	cp $< $@

MISC_JS = build/web/js/q.js \
	   build/web/js/url.js \
	   build/web/js/require.js \
	   build/web/js/codemirror.js \
	   build/web/js/rulers.js \
	   build/web/js/mark-selection.js \
	   build/web/js/pyret-mode.js \
	   build/web/js/s-expression-lib.js \
	   build/web/js/seedrandom.js \
	   build/web/js/source-map.js \
	   build/web/js/pyret-fold.js \
	   build/web/js/scrollpastend.js \
	   build/web/js/matchkw.js \
	   build/web/js/foldcode.js \
	   build/web/js/foldgutter.js \
	   build/web/js/comment.js \
	   build/web/js/dialog.js \
	   build/web/js/search.js \
	   build/web/js/searchcursor.js \
	   build/web/js/annotatescrollbar.js \
	   build/web/js/matchesonscrollbar.js \
	   build/web/js/jump-to-line.js \
	   build/web/js/colorspaces.js \
	   build/web/js/es6-shim.js \
	   build/web/js/runmode.js \
	   build/web/js/mousetrap.min.js \
	   build/web/js/mousetrap-global-bind.min.js \
	   build/web/js/vega.min.js \
	   build/web/js/vega-tooltip.min.js

EDITOR_MISC_JS = build/web/js/q.js \
		  build/web/js/loader.js \
		  build/web/js/codemirror.js \
		  build/web/js/rulers.js \
		  build/web/js/scrollpastend.js \
		  build/web/js/foldcode.js \
		  build/web/js/foldgutter.js \
		  build/web/js/comment.js \
		  build/web/js/dialog.js \
		  build/web/js/search.js \
		  build/web/js/searchcursor.js \
		  build/web/js/annotatescrollbar.js \
		  build/web/js/matchesonscrollbar.js \
		  build/web/js/jump-to-line.js \
		  build/web/js/mark-selection.js \
		  build/web/js/runmode.js \
		  build/web/js/pyret-mode.js \
		  build/web/js/pyret-fold.js \
		  build/web/js/matchkw.js \
		  build/web/js/mousetrap.min.js \
		  build/web/js/mousetrap-global-bind.min.js \
		  build/web/js/log.js \
		  build/web/js/share.js \
		  build/web/js/google-apis/api-wrapper.js \
		  build/web/js/google-apis/drive.js \
		  build/web/js/google-apis/picker.js \
		  build/web/js/google-apis/sheets.js \
		  build/web/js/authenticate-storage.js

build/web/js/editor-misc.min.js: $(EDITOR_MISC_JS)
	npx uglifyjs --compress -o $@ -- $^

# These images were accessed through the lang/ symlink.
# They are now directly copied into src/web/img and picked up by the
# COPY_PNG / COPY_GIF rules above, so the web build needs nothing from ../lang.
MISC_IMG = build/web/img/pyret-icon.png build/web/img/pyret-logo.png build/web/img/pyret-spin.gif build/web/img/up-arrow.png build/web/img/down-arrow.png

COPY_ARR := $(patsubst ./pyret/src/arr/trove/%.arr,build/web/arr/%.arr,$(wildcard ./pyret/src/arr/trove/*.arr))
COPY_ARR :=

# build/web/arr/%: pyret/src/arr/trove/%
# 	cp $< $@


WEB = build/web
WEBV = build/web/views
WEBJS = build/web/js
WEBJSGOOG = build/web/js/google-apis
WEBCSS = build/web/css
WEBTHEMES = build/web/css/themes
WEBIMAGES = build/web/css/images
WEBFONTS = $(WEBCSS)/fonts
WEBIMG = build/web/img
WEBARR = build/web/arr

$(WEBV):
	@$(call MKDIR,$(WEBV))

$(WEB):
	@$(call MKDIR,$(WEB))

$(WEBJS):
	@$(call MKDIR,$(WEBJS))

$(WEBJSGOOG):
	@$(call MKDIR,$(WEBJSGOOG))

$(WEBCSS):
	@$(call MKDIR,$(WEBCSS))

$(WEBIMAGES):
	@$(call MKDIR,$(WEBIMAGES))

$(WEBTHEMES):
	@$(call MKDIR,$(WEBTHEMES))

$(WEBFONTS):
	@$(call MKDIR,$(WEBFONTS))

$(WEBIMG):
	@$(call MKDIR,$(WEBIMG))

$(WEBARR):
	@$(call MKDIR,$(WEBARR))

# The self-contained template's rule (see the SELFCONTAINED comment near the
# top for why it must sit below the COPY_*/MISC_* assignments it depends on).
# beforePyret.js is spelled out because no COPY_* var contains it (it has its
# own webpack rule) -- without it the inlined copy in the template goes stale.
$(SELFCONTAINED): build/web/views/editor.html src/scripts/inline-selfcontained.js \
    $(COPY_JS) $(COPY_LIB_JS) $(COPY_NEW_JS) $(MISC_JS) build/web/js/editor-misc.min.js \
    build/web/js/beforePyret.js \
    $(COPY_CSS) $(COPY_LIB_CSS) $(COPY_THEMES) $(OUT_CSS) $(MISC_CSS) $(COPY_NEW_CSS)
	node src/scripts/inline-selfcontained.js build/web/views/editor.html build/web $@

web-local: $(WEB) $(WEBV) $(WEBJS) $(WEBJSGOOG) $(WEBCSS) $(WEBTHEMES) $(WEBFONTS) $(WEBIMG) $(WEBIMAGES) $(WEBARR) $(OUT_HTML) $(COPY_HTML) $(OUT_CSS) $(COPY_CSS) $(COPY_LIB_CSS) $(COPY_THEMES) $(COPY_FONTS) $(COPY_JS) $(COPY_LIB_JS) $(COPY_LIB_IMAGES) $(COPY_ARR) $(COPY_GIF) $(COPY_SVG) $(COPY_PNG) $(MISC_JS) $(MISC_CSS) $(MISC_IMG) $(COPY_NEW_CSS) $(COPY_NEW_JS) $(COPY_GOOGLE_JS) $(CPOMAIN) $(CPOGZ) build/web/js/editor-misc.min.js build/web/js/snap build/web/js/transpile.xml build/web/editor.html build/web/editor.embed.html $(SELFCONTAINED) 

web: $(WEB) $(WEBV) $(WEBJS) $(WEBJSGOOG) $(WEBCSS) $(WEBTHEMES) $(WEBFONTS) $(WEBIMG) $(WEBIMAGES) $(WEBARR) $(OUT_HTML) $(COPY_HTML) $(OUT_CSS) $(COPY_CSS) $(COPY_LIB_CSS) $(COPY_THEMES) $(COPY_FONTS) $(COPY_JS) $(COPY_LIB_JS) $(COPY_LIB_IMAGES) $(COPY_ARR) $(COPY_GIF) $(COPY_SVG) $(COPY_PNG) $(MISC_JS) $(MISC_CSS) $(MISC_IMG) $(COPY_NEW_CSS) $(COPY_NEW_JS) $(COPY_GOOGLE_JS) build/web/js/editor-misc.min.js build/web/js/snap build/web/js/transpile.xml build/web/editor.html build/web/editor.embed.html $(SELFCONTAINED)

link-pyret:
	ln -s $(PYRET) pyret
	(cd $(PYRET) && $(MAKE) phaseA-deps)

# See the comment near PYRET_MODE above. In the monorepo this creates a symlink
# to the shared codemirror-mode sibling. It's a no-op when `mode` already exists,
# if CI or a deploy step copies it in.
mode:
	ln -s ../codemirror-mode mode

deploy-cpo-main: link-pyret $(CPOMAIN) cpo-main-release

cpo-main-release: $(CPOGZ)
	mkdir -p build/release/$(COMMITID);
	cp $(CPOGZ) build/release/$(COMMITID)/

TROVE_JS := src/web/js/trove/*.js
TROVE_ARR := src/web/arr/trove/*.arr

$(PHASEA): libpyret ;

.PHONY: libpyret
libpyret:
	$(MAKE) phaseA -C pyret/

$(BUNDLED_DEPS): src/scripts/npm-dependencies.js
	# Explicitly exclude crypto, buffer, and stylus, nested npm dependencies that aren't needed
	npx browserify src/scripts/npm-dependencies.js -x crypto -x stylus -o $(BUNDLED_DEPS)

$(CPOMAIN): $(BUNDLED_DEPS) $(TROVE_JS) $(TROVE_ARR) $(WEBJS) src/web/js/*.js src/web/arr/*.arr cpo-standalone.js cpo-config.json src/web/arr/cpo-main.arr $(PHASEA)
	mkdir -p compiled/;
	#cp pyret/build/phaseA/compiled/*.js ./compiled/
	node pyret/build/phaseA/pyret.jarr \
    --builtin-js-dir src/web/js/trove/ \
    --builtin-js-dir pyret/src/js/trove/ \
    -allow-builtin-overrides \
    --builtin-arr-dir src/web/arr/trove/ \
    --builtin-arr-dir pyret/src/arr/trove/ \
    --require-config cpo-config.json \
    --build-runnable src/web/arr/cpo-main.arr \
    --standalone-file cpo-standalone.js \
    --compiled-dir ./compiled \
    --deps-file $(BUNDLED_DEPS) \
    --outfile $(CPOMAIN) -no-check-mode

# NOTE(joe): Need to do .gz.js because Firefox doesn't like gzipped JS having a
# non-.js extension.
$(CPOGZ): $(CPOMAIN)
	cp $(CPOMAIN) $(CPOMAIN).js
	npx uglifyjs --compress -o $(CPOMAIN).min -- $(CPOMAIN)
	gzip -c -f $(CPOMAIN).min > $(CPOGZ)

# ============================================================
# TypeScript compiler flavor (strictly additive, opt-in).
# Builds a second jarr (cpo-main-ts.jarr, no Pyret-hosted compiler
# modules) plus a browser bundle of lang/src/ts-compiler. The editor
# page selects between flavors via ?compiler=ts or CPO_COMPILER=ts
# (see src/web/editor.html and src/server.js).
#
# The servable ts artifacts are gzip-at-rest under canonical names next
# to cpo-main.jarr.gz.js -- cpo-main-ts.jarr.gz.js and ts-compiler.gz.js
# -- because every consumer derives their URLs from PYRET's directory
# (see editor.html). The plain browserify bundle is an intermediate and
# deliberately lives OUTSIDE build/web so no packaging step ships it.
# ============================================================

TS_CPOMAIN=build/web/js/cpo-main-ts.jarr
TS_CPOGZ=build/web/js/cpo-main-ts.jarr.gz.js
TS_COMPILER_JS=build/ts-compiler.js
TS_COMPILER_GZ=build/web/js/ts-compiler.gz.js

.PHONY: ts-libpyret
ts-libpyret:
	$(MAKE) ts-compiler -C pyret/

$(TS_COMPILER_JS): src/scripts/make-ts-compiler-entry.js ts-libpyret
	@$(call MKDIR,build)
	node src/scripts/make-ts-compiler-entry.js build/ts-compiler-entry.js
	npx browserify build/ts-compiler-entry.js -s PyretTSCompiler -o $(TS_COMPILER_JS)

$(TS_COMPILER_GZ): $(TS_COMPILER_JS)
	@$(call MKDIR,build/web/js)
	npx uglifyjs --compress -o $(TS_COMPILER_JS).min -- $(TS_COMPILER_JS)
	gzip -c -f $(TS_COMPILER_JS).min > $(TS_COMPILER_GZ)

$(TS_CPOMAIN): $(BUNDLED_DEPS) $(TROVE_JS) $(TROVE_ARR) $(WEBJS) src/web/js/*.js src/web/arr/*.arr cpo-standalone.js cpo-config.json src/web/arr/cpo-main-ts.arr $(PHASEA)
	mkdir -p compiled/;
	node pyret/build/phaseA/pyret.jarr \
    --builtin-js-dir src/web/js/trove/ \
    --builtin-js-dir pyret/src/js/trove/ \
    -allow-builtin-overrides \
    --builtin-arr-dir src/web/arr/trove/ \
    --builtin-arr-dir pyret/src/arr/trove/ \
    --require-config cpo-config.json \
    --build-runnable src/web/arr/cpo-main-ts.arr \
    --standalone-file cpo-standalone.js \
    --compiled-dir ./compiled \
    --deps-file $(BUNDLED_DEPS) \
    --outfile $(TS_CPOMAIN) -no-check-mode

$(TS_CPOGZ): $(TS_CPOMAIN)
	cp $(TS_CPOMAIN) $(TS_CPOMAIN).js
	npx uglifyjs --compress -o $(TS_CPOMAIN).min -- $(TS_CPOMAIN)
	gzip -c -f $(TS_CPOMAIN).min > $(TS_CPOGZ)

.PHONY: web-ts
web-ts: $(TS_CPOMAIN) $(TS_CPOGZ) $(TS_COMPILER_GZ)

clean:
	rm -rf build/
	rm -rf compiled/
