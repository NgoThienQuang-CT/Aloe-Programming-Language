.PHONY: all build test fmt install uninstall clean

PREFIX ?= $(HOME)/.local

all: build

build:
	dune build

release:
	dune build --profile release bin/main.exe

test:
	dune runtest

fmt:
	dune fmt

install: release
	install -d $(PREFIX)/bin
	install -m 755 _build/default/bin/main.exe $(PREFIX)/bin/aloe

uninstall:
	rm -f $(PREFIX)/bin/aloe

clean:
	dune clean