# rash: asynchronous shell

rash is a minimal shell which supports asynchronous operations by saving
its program state to a file, then terminating itself, and later being
started again by the user, where it will read and continue from the
saved program state.

## Examples

Example programs are located in the [examples](./examples) subdirectory.

You can find more programs here:
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

Run `cabal test` to run unit tests.

## User documentation

`rash` is a state-saving pseudo-shell tailored for IRC bots, but
hopefully also applicable in other cases.

When its `read` instruction is encountered, the program exits.  When the
program is run again, it restarts at that `read` instruction and uses
the command-line arguments as the read input instead of using standard
in.  This continues until there are no more instructions to run.

`rash` is an assembly-like language.  Each line is an instruction.  It
has the following types of instructions:

  - `# Some text.`
    - A comment.
    - `Instruction` constructor: None, optimized away
  - `a_var=some text`
    - Assignment of text "some text" to variable `a_var`.
    - `Instruction` constructor: `Assign`
  - `>command arg0 arg1 ... argN`
    - Run `command arg0 arg1 ... argN` in a shell, wait for it to
      finish, and print its output.
    - `Instruction` constructor: `Run`
  - `read x`
    - Exit.  When restarted, read the command line arguments into the
      variable `x`.
    - `Instruction` constructor: `Read`
  - `:your_label`
    - A label for jumping.
    - `Instruction` constructor: None, optimized away
  - `j your_label`
    - Jump unconditionally to the `your_label` label.
    - `Instruction` constructor: `Jump`
  - `jz somewhere`
    - Jump to the `somewhere` label if the previous command exited with
      return code 0.  If no command has previously run, the return code
      is 0.
    - `Instruction` constructor: `JumpIfRetZero`
  - `exit`
    - Remove all state and exit.  This also happens if the end of the
      program is reached.
    - `Instruction` constructor: `Exit`
  - `tribbles=>ls stuff`
    - Run `ls stuff` in a shell, wait for it to finish, and redirect its
      standard out into the `tribbles` variable.
    - `Instruction` constructor: `AssignRun`
  - `<some text>grep -vi bar`
    - Run `grep -vi bar` with "some text" redirected into standard in,
      and print its output.  This can be used to simulate pipes.
    - `Instruction` constructor: `Run`
  - `foo=<some text>grep -vi bar`
    - Run `grep -vi bar` with "some text" redirected into standard in,
      and redirect its standard out into the `foo` variable.
    - `Instruction` constructor: `AssignRun`

To use a variable in text fields, write `${variable}`.  This can be used
in commands, assignment values, and command inputs, and it can be mixed
with just text.  For example, this:

```
read url_base
>grep http://${url_base}/robots.txt crawls
```

first reads command line arguments into the `url_base` variable, and
then runs `grep` with, among other text, the `url_base` value as the
first argument.

Whitespace and lack thereof is significant, because why not?

For now, no special characters can be escaped.

You can use the special variable `${initial_arguments}` to get the
initial arguments passed to your program.

## State directory

Rash saves its state to files in a directory.  The directory is
determined by this algorithm:

  1. Is `$RASH_STATE_DIR` set?  Then save the state in
     `$RASH_STATE_DIR`.  If not, go to step 2.
  2. Is `$XDG_HOME_STATE` set?  Then save the state in
     `$XDG_HOME_STATE/rash`, as per the [XDG Base Directory
     Specification](https://specifications.freedesktop.org/basedir-spec/latest/#variables).
     If not, go to step 3.
  3. Save the state in `$HOME/.local/state/rash`.

If the given directory does not exist, rash will create it.

## Developer documentation

The pipeline of `rash` loading a new file looks like this:

  1. Parse the file into a list of intermediate
     `Rash.Representation.Parse.Instruction` instructions.

  2. Convert the `Rash.Representation.Parse.Assembly` into an optimized
     `Rash.Representation.Internal.Assembly` type, where jumps to labels
     are converted to jumps to absolute instructions, and variable names
     are converted to variable indexes.  Also turn all strings into
     `Data.Text.Text` values.

  3. Interpret.

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
