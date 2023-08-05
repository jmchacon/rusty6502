Ideas for features/usage

1 thread with a controller running the 65xx and has the ability to instrument/query it.

Controller functions:

* Run continually but check on every step for new commands over channel - RUN
* Stop running - STOP
* Run to break point - BP <ADDR>
* List breakpoints - BPL
* Delete breakpoint - DB <num>
* Step instruction - S
* Tick instruction (1 clock cycle) - T
* Read a RAM value - R <ADDR>
* Read a RAM range - RR <ADDR> <LEN>
* Change RAM value - W <ADDR> <VAL>
* Set a range of RAM values - WR <ADDR> <LEN> <VAL>
* Dump state (CPU) - CPU
* Dump RAM - RAM
* Disassemble at an addr - D <ADDR>
* Disassemble a range starting at a PC - DR <ADDR> <LEN>
* Watchpoint on a location in RAM - WP <ADDR>
* List watchpoints - WPL
* Delete watchpoint - DW <num>
* Load a file into RAM - L <PATH> [<START>]
* Dump RAM to a file - BIN <PATH>
* Jump to PC - PC <ADDR>
* Reset the CPU - RESET

Initially implement in a terminal and the convert to bracket-terminal

Initial state comes up with blank 64k RAM and a powered off CPU
Have to load or poke values or else it's just going to run BRK continually
