
all: build

build:
	nix-shell --command "cabal v1-build"

configure:
	nix-shell --command "cabal v1-configure"

doc:
	nix-shell --command "cabal v1-haddock --executables"

hoogle:
	nix-shell --command "cabal v1-haddock --executables --hoogle"

ghci:
	nix-shell --command "cabal v1-repl"

.PHONY: all build configure doc hoogle ghci


