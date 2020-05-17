Brainfuck IR to Turing Machine compiler
=======================================

This little program compiles Brainfuck IR outputted by [bfc](https://github.com/Wilfred/bfc) to Turing Machine code accepted by [Turing Machine Simulator](https://turingmachinesimulator.com/).

The compiled code is not that quick, it contains a lot of repeat instructions. This is more of a proof-of-concept that it can be done.

It currently requires JSON-formatted BFIR, but 'raw' BFIR parsing is on the way. A bfc variant which supports this can be obtained via [here](https://github.com/Discookie/bfc/tree/bfir-json).

Instructions
------------

* The byte values from 0 to 255 are mapped to characters, which are printed into the output
* Read (,) operations are not supported
* Write (.) operations do the following:
  * If a 0 is written, the program jumps into the 'end' accepting state.
  * If a 'normal' (``33 <= x < 127``) ASCII character is written, execution continues.
  * Otherwise, execution stops in a rejected state.
* If the ``--two-tape`` flag is specified, 'normal' ASCII characters are printed onto the second tape, from left to right.
  * Printing empty spaces is currently not supported

Example commands
----------------

```sh
$ bfc example.bf --dump-ir-json | bf-to-turing > example.txt
$ bfc example.bf --dump-ir-json | bf-to-turing --two-tape > example.txt
```

License
-------

GPLv3 or later license. Uses bfir.rs from [bfc](https://github.com/Wilfred/bfc), which is under GPLv2 or later license.