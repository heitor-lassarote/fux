PACKAGE := fux

DEV_OPTIONS = --fast --ghc-options -freverse-errors

.DEFAULT_GOAL := watch

build:
	stack build $(DEV_OPTIONS)

watch:
	stack build $(DEV_OPTIONS) --file-watch

test:
	stack test $(DEV_OPTIONS)

.PHONY: build watch test
