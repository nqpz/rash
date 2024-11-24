# rash: asynchronous shell

rash is a minimal shell which supports asynchronous operations by saving
its program state to a file, then terminating itself, and later being
started again by the user, where it will read and continue from the
saved program state.

**rash is currently in the process of being migrated from a bespoke
setup and into the wild, and does not work in a well-documented manner
yet.**

For examples programs, please see the results of this search for now:
https://github.com/search?q=repo%3Aathas%2FEggsML+%22%23%21%2Fusr%2Fbin%2Fenv+rash%22&type=code

## Building

Run `cabal build` to build.  This requires the `cabal` tool to be
present on your system.  Please see
https://cabal.readthedocs.io/en/latest/getting-started.html for how to
get this.

Alternatively, if you have [Nix](https://nixos.org/), you can run
`nix-shell` to get into a shell with the required development
dependencies, or `nix-build` to build the program, which will put a
binary into `result/bin`.

## License

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero
General Public License for more details.

---

rash was originally developed within the
[EggsML monorepo](https://github.com/athas/EggsML) as part of the
toolset of an IRC bot, but has now been set loose upon the rest of the
world.
