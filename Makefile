PACKAGE := fux

DEV_OPTIONS = --fast --ghc-options -freverse-errors --file-watch

.DEFAULT_GOAL := build

build:
	stack build $(DEV_OPTIONS)
