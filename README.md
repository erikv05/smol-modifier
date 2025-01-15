# smol-misinterpreters

Misinterpreters of SMoL

## Usage

First of all, you need to install this directory as a racket package:

```sh
$ raco pkg install
```

The following shell interactions illustrate how to run a program with
all misinterpreters found in the paper. (There a few more
misinterpreters in this repository; they are experimental.)

```sh
$ echo '(defvar x 12)\n(defvar y x)\n(set! x 0)\nx\ny' | racket runner.rkt
Misinterpreter,Result
DefByRef,0 0
$ echo '(defvar x 12)\n(deffun (f x) (set! x 0))\n(f x)\nx' | racket runner.rkt
Misinterpreter,Result
FlatEnv,0
CallByRef,0
$
```

See the source code of [./runner.rkt](./runner.rkt) for the Racket interface.
