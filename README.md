# Vila

A minimal **Vim clone** written in **Scala 3** using the
[TamboUI](https://github.com/tamboui/tamboui) TUI library.

Built with an immutable, purely functional style — all state
transitions are pure functions returning new values.

## Quick Start

```bash
# Run with scala-cli
scala-cli run /Users/bsan0009/tmp/vila

# Or build a fat JAR
scala-cli --power package . --assembly -o vila.jar
java -jar vila.jar

# Or build a native binary (requires GraalVM)
native-image -jar vila.jar -o vila
./vila
```

## Modes

| Mode              | Enter       | Cursor Colour  |
|-------------------|-------------|----------------|
| Normal            | `Esc`       | White block    |
| Insert            | `i` `a` `I` `A` `o` `O` | Green block |
| Visual (char)     | `v`         | Magenta block  |
| Visual (line)     | `V`         | Magenta block  |
| Visual (block)    | `Ctrl-V`    | Magenta block  |
| Command           | `:`         | Yellow prompt  |
| Search            | `/` `?`     | Yellow prompt  |

## Operators

| Key         | Action                      |
|-------------|-----------------------------|
| `d{motion}` | Delete                      |
| `c{motion}` | Change (delete + insert)    |
| `y{motion}` | Yank (copy)                 |
| `D`         | Delete to end of line       |
| `C`         | Change to end of line       |
| `Y`         | Yank line                   |
| `dd`/`cc`/`yy` | Linewise delete/change/yank |
| `p`/`P`     | Put after/before cursor     |
| `>`/`<`     | Indent / outdent            |
| `~`         | Toggle case                 |
| `x`         | Delete character            |

## Motions

`hjkl` `w` `b` `e` `W` `B` `E` `0` `$` `^`
`gg` `G` `{` `}` `%` `f` `F` `t` `T` `;` `,`

## Text Objects

`iw` `aw` `iW` `aW` `i"` `a"` `i'` `a'` `i(` `a(` `i[` `a[` `i{` `a{`

## Visual Mode

`v` (char), `V` (line), `Ctrl-V` (block). Use with `d` `c` `y` `>` `<` `~`.

## Search

`/pattern` `?pattern` `n` `N` `*` `#`
`:nohl` `:set hlsearch` `:set nohlsearch`

## Commands

`:q` `:w` `:wq` `:nohl` `:help` `:map <from> <to>`

## Architecture

| Component        | Role                                  |
|------------------|---------------------------------------|
| `TextBuffer`     | Immutable text model                  |
| `EditorState`    | Immutable editor snapshot             |
| `InputHandler`   | Pure state transitions                |
| `VimRenderer`    | Pure rendering (state → Frame)        |
| `VimApp`         | Application shell (event loop)        |

## Build Outputs

- **Fat JAR** (`vila.jar`, 11 MB) — runs on any JDK 17+
- **Native binary** (`vila`, 16 MB) — standalone arm64 executable
  with built-in GC (Serial GC by default)
