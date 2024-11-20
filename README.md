# rash: asynchronous shell

rash is a minimal shell which supports asynchronous operations by saving
its program state to a file, then terminating itself, and later being
started again by the user, where it will read and continue from the
saved program state.

## Building

Run `cabal build` to build.  This requires the `cabal` tool to be
present on your system.  Please see
https://cabal.readthedocs.io/en/latest/getting-started.html for how to
get this.

Alternatively, if you have [Nix](https://nixos.org/), you can run
`nix-shell` to get into a shell with the required development
dependencies, or `nix-build` to build the program, which will put a
binary into `result/bin`.

---

rash was originally developed within the
[EggsML monorepo](https://github.com/athas/EggsML) as part of the
toolset of an IRC bot, but has now been set loose upon the rest of the
world.
