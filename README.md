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

## Building and installing

Run `cabal install` to build the `rash` executable and place it in
`~/.local/bin`.  This requires the `cabal` tool to be present on your
system.  Please see
https://cabal.readthedocs.io/en/latest/getting-started.html for how to
install this.

Alternatively, you can install [Nix](https://nixos.org/) and then either

  - run `nix-shell` to get into a sub-shell with the required development
    dependencies present, and then run `cabal install` there, or
  - run `nix-build` to build the program straight away, which will put a
    binary into `result/bin` that you can then run directly or manually
    copy to `~/.local/bin` or another directory of your choosing.

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
