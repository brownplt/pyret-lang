# browser-test/Makefile -- prerequisites + runners for the five environments.
#
#   make cpo            make embed           make embed-static
#   make vscode         make vscode-ovsx     make all-envs
#
#   make embed SUITES=url-imports
#   make cpo   SUITES=tables,url-imports GREP='url-file'
#
# Freshness of the big builds is delegated to the real Makefiles in lang/ and
# code.pyret.org/, which are already incremental -- this file only encodes
# WHICH prerequisites each environment needs, and starts/stops the CPO server.
#
# Each env needs a different subset, which is the thing that's easy to get wrong:
#
#   env           cpo build   CPO server   embed/dist   vscode/dist
#   cpo               y           y            -            -
#   embed             y           y            -            -
#   embed-static      y           -            y            -
#   vscode            -           -            -            y
#   vscode-ovsx       -           -            -            y

HERE := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
ROOT := $(abspath $(HERE)/..)
CPO  := $(ROOT)/code.pyret.org
# NB: not LANG -- that is the locale env var, and assigning it here would mark
# it environment-origin, exporting the overwrite to every recipe shell and
# sub-make. node exits silently under LANG=<a path>, and code.pyret.org's
# $(shell node -e ... require.resolve ...) then expands to the empty string,
# failing the build with a baffling "No rule to make target '/q.js'".
PYRET_LANG := $(ROOT)/lang

BASE_URL ?= http://localhost:4999
PORT     ?= 4999
SUITES   ?= all
GREP     ?=

RUNFLAGS := --suites=$(SUITES)
ifneq ($(strip $(GREP)),)
RUNFLAGS += --grep=$(GREP)
endif

# Mirrors the "Start CPO server" step in .github/workflows/browser-test.yml.
# NODE_ENV=development is load-bearing, not a nicety: it is what mounts
# code.pyret.org/test-util/ (server.js:192), which serves both the embed host
# page (/embed/embed1.html) and the url-imports fixtures (/pyret-programs/...).
SERVER_ENV := BASE_URL=$(BASE_URL) PORT=$(PORT) \
  PYRET=$(BASE_URL)/js/cpo-main.jarr POSTMESSAGE_ORIGIN='*' \
  NODE_ENV=development SESSION_SECRET=not-so-secret REDISCLOUD_URL= \
  URL_FILE_MODE=all-remote IMAGE_PROXY_BYPASS=true \
  SHARED_FETCH_SERVER=https://code.pyret.org CURRENT_PYRET_RELEASE=

.PHONY: help cpo embed embed-static vscode vscode-ovsx all-envs \
        cpo-build embed-build vscode-build deps server stop-server clean-stamps

help:
	@sed -n '2,20p' $(firstword $(MAKEFILE_LIST))

# ---------------------------------------------------------------- prerequisites

node_modules:
	cd $(HERE) && npm ci --ignore-scripts

# One Chromium, installed explicitly (npm ci ran with --ignore-scripts).
.chromium.stamp: | node_modules
	cd $(HERE) && npx playwright install --with-deps chromium
	touch $@

deps: .chromium.stamp

# -sfn, not -sf: onto an existing symlink-to-a-directory, plain `ln -sf`
# dereferences it and links INSIDE lang/ rather than replacing the link.
$(CPO)/pyret:
	ln -sfn ../lang $@

$(PYRET_LANG)/node_modules:
	cd $(PYRET_LANG) && npm install

$(CPO)/node_modules:
	cd $(CPO) && npm ci --ignore-scripts

$(ROOT)/embed/node_modules:
	cd $(ROOT)/embed && npm ci --ignore-scripts

$(ROOT)/vscode/node_modules:
	cd $(ROOT)/vscode && npm ci --ignore-scripts

# code.pyret.org/build/web -- the editor, editor.embed.html, cpo-main.jarr.
# `make web-local` recursively drives lang's phaseA, so both sub-makes decide
# for themselves what actually needs rebuilding.
cpo-build: | $(CPO)/pyret $(PYRET_LANG)/node_modules $(CPO)/node_modules
	cd $(PYRET_LANG) && $(MAKE) phaseA-deps
	cd $(CPO) && $(MAKE) web-local

# The pyret-embed library (embed/dist/pyret.js) -- embed-static only.
embed-build: | $(ROOT)/embed/node_modules
	cd $(ROOT)/embed && npx webpack

# The extension bundle (vscode/dist) -- vscode + vscode-ovsx only. The build
# symlink is how the webview picks up the CPO assets.
vscode-build: cpo-build | $(ROOT)/vscode/node_modules
	ln -sf ../code.pyret.org/build $(ROOT)/vscode/build
	cd $(ROOT)/vscode && npm run compile

# --------------------------------------------------------------- the CPO server

results:
	mkdir -p $@

server: cpo-build | results
	@if curl -fs -o /dev/null $(BASE_URL)/editor; then \
	  echo "CPO server already up at $(BASE_URL)"; \
	else \
	  echo "starting CPO server on port $(PORT)..."; \
	  ( cd $(CPO) && $(SERVER_ENV) nohup node src/run.js \
	      </dev/null > $(HERE)/results/cpo-server.log 2>&1 & ) >/dev/null 2>&1 ; \
	  for _ in $$(seq 1 60); do \
	    curl -fs -o /dev/null $(BASE_URL)/editor && break; sleep 1; done; \
	  curl -fs -o /dev/null $(BASE_URL)/editor \
	    || { echo "server never came up; see results/cpo-server.log"; exit 1; }; \
	  echo "CPO server up at $(BASE_URL)"; \
	fi

stop-server:
	@pid=$$(lsof -ti tcp:$(PORT) 2>/dev/null | head -1); \
	if [ -n "$$pid" ]; then kill $$pid && echo "stopped CPO server (pid $$pid)"; \
	else echo "no CPO server listening on port $(PORT)"; fi

# ----------------------------------------------------------------- run an env

# cpo and embed load the editor FROM the CPO server, so they get BASE_URL.
cpo: server deps
	cd $(HERE) && BASE_URL=$(BASE_URL) node run.js --env=cpo $(RUNFLAGS)

embed: server deps
	cd $(HERE) && BASE_URL=$(BASE_URL) node run.js --env=embed $(RUNFLAGS)

# The remaining three serve the editor from their OWN origin and run no CPO
# server, so they get no BASE_URL. They still run the "no external network"
# url-file tests: run.js serves those fixtures itself, for every env.
embed-static: embed-build cpo-build deps
	cd $(HERE) && env -u BASE_URL node run.js --env=embed-static $(RUNFLAGS)

vscode: vscode-build deps
	cd $(HERE) && env -u BASE_URL node run.js --env=vscode $(RUNFLAGS)

vscode-ovsx: vscode-build deps
	cd $(HERE) && env -u BASE_URL node run.js --env=vscode-ovsx $(RUNFLAGS)

all-envs:
	@rc=0; for e in cpo embed embed-static vscode vscode-ovsx; do \
	  echo "=== $$e ==="; $(MAKE) --no-print-directory $$e || rc=1; done; \
	echo "done (rc=$$rc)"; exit $$rc

clean-stamps:
	rm -f $(HERE)/.chromium.stamp
