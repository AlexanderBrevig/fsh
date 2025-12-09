# fsh - Forth Shell

A stack-based shell that brings the power of concatenative programming to Unix command-line tools.

[![asciicast](https://asciinema.org/a/z3WG3kSNKrGz0vrL6mu9v5xWZ.svg)](https://asciinema.org/a/z3WG3kSNKrGz0vrL6mu9v5xWZ)

## The Big Idea

Traditional shell uses pipes:

```bash
cat /etc/legal | grep "free"
```

fsh uses a stack (Reverse Polish Notation):

```forth
/etc/legal cat free grep \ → automatically prints and pipes!
```

Commands automatically:

- Print their output (so you see results immediately)
- Keep output on the stack (so you can pipe to the next command)
- Chain together naturally

## Quick Start

**Build:**

```bash
cd fsh
opam install . --deps-only
dune build
./_build/default/bin/main.exe
```

**Or create an alias:**

```bash
alias fsh='/path/to/fsh/_build/default/bin/main.exe'
```

## Core Concepts

### Stack-Based Execution

Push arguments, then run commands:

```forth
/etc/legal cat \ → cat /etc/legal (prints result)
"hello" lib 1  \ → ls lib (prints directory listing) hello still on stack
```

### Automatic Piping

Command outputs automatically pipe to the next command:

```forth
/etc/legal cat free grep  \ → cat /etc/legal | grep free
lib 1 ls eval 1 grep      \ → ls lib | grep eval
```

### Stack-Aware Prompt

```forth
fsh>          ← Empty stack
fsh[2]>       ← 2 arguments ready
fsh[:1]>      ← 1 command output ready to pipe
fsh[1:1]>     ← 1 argument + 1 output (mixed)
```

## Essential Commands

### Stack Manipulation

```forth
dup   \ → duplicate top item
swap  \ → swap top two items
drop  \ → remove top item
clear \ → clear entire stack
.s    \ → show entire stack
.     \ → print and remove top (with newline)
type  \ → print and remove top (no newline)
```

### Custom Words

```forth
: lsgrep 1 ls 1 grep \ → define reusable command
eval lib lsgrep      \ → use it: ls lib | grep eval
```

### Control Flow

```forth
\ Conditionals
5 3 > if greater . else smaller . then

\ Loops
0 5 do i . loop           \ → print 0 through 4
5 begin dup . 1 - dup 0 = until drop \ → countdown

\ Process each line
lib 1 ls each cat then    \ → cat each file in lib/
```

### String Operations

```forth
"hello " "world" concat .     \ → hello world
*.ml .s                       \ → glob expansion (shows all .ml files)

\ Conditional concatenation (great for prompts!)
"main" "@" ?prefix .          \ → @main
"" "@" ?prefix .              \ → (empty - no separator added)
"user" ":" ?suffix .          \ → user:
"branch" "[" "]" ?wrap .      \ → [branch]
```

### Environment & Files

```forth
HOME getenv .             \ → get env variable
MY_VAR hello setenv       \ → set env variable
/tmp cd                   \ → change directory
test.txt pwd >file        \ → redirect output to file
?                         \ → get exit code of last command
```

### Math & Comparison

```forth
5 3 + .                   \ → 8
10 3 /mod                 \ → quotient and remainder
5 3 > .                   \ → 1 (true)
```

### Help

```forth
words                     \ → list all words
help                      \ → show help
see dup                   \ → show definition of a word
see ls                    \ → show help for external command (--help or man)
```

### Customizable Prompt

Define your own prompt with `$prompt` and helper words:

```forth
\ Default (stack-aware)
: $prompt "fsh" $stack concat "> " concat ;
→ fsh[1:1]>

\ Git-aware with conditional separator
: $prompt "fsh" $gitbranch "@" ?prefix concat $stack concat "> " concat ;
→ fsh@main[2]>  (in git repo)
→ fsh[2]>       (not in git repo)

\ Custom stack format
: $prompt "fsh(" $in concat ";" concat $out concat ")> " concat ;
→ fsh(1;1)>

\ Full example with conditionals
: $prompt $username "@" ?suffix $hostname concat ":" concat $basename concat $stack concat " $ " concat ;
→ alice@laptop:fsh[2] $
```

**Prompt helpers:**

- `$stack` - formatted stack state `[n:m]`
- `$in` - count of input items (Int)
- `$out` - count of output items (Int)
- `$gitbranch` - current git branch or empty
- `$basename` - current directory name
- `$cwd` - full working directory path
- `$hostname` - system hostname
- `$username` - current username
- `$exitcode` - last command exit code
- `$time` - current time `HH:MM`

**Conditional helpers:**

- `?prefix` - prepend separator only if string is non-empty: `"main" "@" ?prefix` → `"@main"`
- `?suffix` - append separator only if string is non-empty: `"user" ":" ?suffix` → `"user:"`
- `?wrap` - wrap with prefix/suffix if non-empty: `"dev" "(" ")" ?wrap` → `"(dev)"`

**Config file:** Define `$prompt` and other customizations in `~/.fshrc` - loaded automatically on startup!

## Quick Reference

### Depth Control

Control how many arguments a command consumes:

```forth
foo bar baz echo      \ → echo foo bar baz (all args)
foo bar 1 echo        \ → echo bar (only 1 arg, "foo" stays on stack)
```

### Quoted vs Unquoted Strings

```forth
hello                     \ → tries PATH lookup, executes if found, else pushes "hello"
"hello"                   \ → always pushes literal string
*.ml                      \ → glob expands to matching files
"*.ml"                    \ → literal string "*.ml"
```

### Output vs String Types

- **String** - User input, becomes command arguments
- **Output** - Command results, automatically pipes to next command

```forth
\ .s to see the difference:
hello     \ → String "hello"
pwd cat   \ → Output «/home/...»
```

Convert between types:

```forth
hello >output cat         \ → treat string as pipeable output
pwd cat >string echo      \ → treat output as string argument
```

## Examples

### Basic Piping

```forth
/etc/legal cat
0 ls wc
lib 1 ls README 1 grep
```

### Custom Utilities

```forth
: findf 1 find grep  ;
: catgrep 1 cat 1 grep  ;
: ll -la 1 ls  ;

TODO src findf
```

### Processing Multiple Files

```forth
\ Cat each .ml file in lib/
lib 1 ls each "lib/" swap concat cat then
```

### Scripting

```forth
\ Factorial
: factorial 1 swap 1 + 1 do i * loop ;
5 factorial \ → 120

\ FizzBuzz
: fizzbuzz
  1 16 do
    i 15 mod 0 = if FizzBuzz type else
      i 3 mod 0 = if Fizz type else
        i 5 mod 0 = if Buzz type else
          i .
        then
      then
    then
  loop ;
```

## Why RPN?

Reverse Polish Notation might feel backwards at first, but it provides:

1. **Zero ambiguity** - no operator precedence rules
2. **Natural composition** - `f g` means "do f, then do g"
3. **Stack manipulation** - powerful reordering without variables
4. **No parentheses** - structure emerges from order

Once you internalize the flow, it becomes second nature.

## Contributing

This is an experimental project exploring concatenative shell design. Ideas, issues, and PRs welcome!

Areas that need work:

- Better error messages
- Tab completion and history support
- Expanded test suite
- Variables and local bindings

## Inspiration

- **Forth** - The original concatenative language
- **Factor** - Modern concatenative with batteries included
- **Unix Shell** - Pipes, composition, text streams

## License

MIT

---

**fsh** - where Forth meets Unix 🚀
