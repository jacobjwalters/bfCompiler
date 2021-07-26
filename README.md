# bfCompiler
At some point in every Haskeller's career,
they write a brainfuck interpreter.
To go the extra mile
(towards offing myself),
I've decided to write an optimising compiler
that generates native amd64 linux executables
(with the help of [nasm](https://nasm.us/)).

## Features
- Full brainfuck compatibility
- Redundant code cancellation
(e.g. `,>><<+><-[+><><-].` -> `,.`)
- An interpreter
(which may be built as an executable for any GHC-compatible platform)

## To-do
- Actually make the compiling part
- Use a rich IR for the compiler to allow for smaller assembly output.
Ideally, a hello world BF program should output an idiomatic assembly HW.
No excessive `inc` or `dec` instructions.
- Provide a step-wise debugger for the interpreter

## Contributing
Why?
This is a weekend project,
there's really not too much complexity to it.
You'd be much better off spending the time you'd take understanding my code
on actually implementing your own version.

That being said, it's under MIT licensing, so do what you will with the code itself.
