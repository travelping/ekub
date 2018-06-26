PROJECT = ekub

VERSION = $(shell cat src/$(PROJECT).app.src | grep vsn | cut -d\" -f2)
GIT_SHA = $(shell git rev-parse HEAD | cut -c1-8)

REBAR = $(shell which ./rebar3 || which rebar3)

BUILD_DIR = _build

all:
	$(REBAR) compile
	$(REBAR) unlock

shell:
	$(REBAR) shell
	$(REBAR) unlock

upgrade:
	$(REBAR) upgrade
	$(REBAR) unlock

check:
	$(REBAR) eunit

clean:
	$(REBAR) clean -a
	$(REBAR) unlock

distclean: clean
	rm -rf $(BUILD_DIR)

version:
	@echo "Version $(VERSION) (git-$(GIT_SHA))"
