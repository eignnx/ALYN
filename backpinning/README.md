# backpinning
A register allocator based on [Traub, Halloway, and Smith's *Second-chance Binpacking*](https://dl.acm.org/doi/10.1145/277652.277714) algorithm. Also influenced by the article [Cranelift, Part 4: A new Register Allocator](https://cfallin.org/blog/2022/06/09/cranelift-regalloc2/) by Chris Fallin.

## Interval Diagram
Here is an example of a generated interval diagram. The instructions of the program text are shown on the right-hand side of the diagram, so it should be read top-to-bottom.

There are (currently) two phases to an instruction's execution: `ReadArgs` and `WriteBack`. When a line is drawn horizontally from the left of an instruction (see instruction `03` below) it signifies the `ReadArgs` phase. A corner (`─(w)─┘`) drawn out of the bottom is for the `WriteBack` phase. The dotted vertical lines represent a temporary's non-live interval, and the solid vertical lines represent live intervals.

```
  %x %v %n %low %high %mid %vmid %elem %retval   
.-+--+--+--+----+-----+----+-----+-----+--------.
| '  '  '  '    '     '    '     '     '        (R)─00: %x ← _ 
| #..!..!..!....!.....!....!.....!.....!........|──(W)─┘
| #  '  '  '    '     '    '     '     '        (R)─01: %v ← _ 
| #  #..!..!....!.....!....!.....!.....!........|──(W)─┘
| #  #  '  '    '     '    '     '     '        (R)─02: %n ← _ 
| #  #  #..!....!.....!....!.....!.....!........|──(W)─┘
| #..#..#..!....!.....!....!.....!.....!........|─(a)─┘
:---------------[subr__binsearch]---------------: BB7
| #..#..#..!....!.....!....!.....!.....!........|──(b)─┐
| #  #  #  '    '     '    '     '     '        (R)─04: %low ← 0
| #  #  #  #....!.....!....!.....!.....!........|─(W)─┘
| #  #  #..#....!.....!....!.....!.....!........|──(R)─05: %high ← %n ± 1
| #  #  '  #    #.....!....!.....!.....!........|─(W)─┘
| #  #  '  #    #     '    '     '     '        (R)─06: jmp local<loop_cond>
| #..#..!..#....#.....!....!.....!.....!........|──(a)─┘
:---------------[local__loop_top]---------------: BB6
| #..#..!..#....#.....!....!.....!.....!........|──(b)─┐
| #  #  '  #    #     '    '     '     '        (R)─08: %mid ← %low ± %high
| #  #  '  #    #     #....!.....!.....!........|─(W)─┘
| #  #  '  #    #     #    '     '     '        (R)─09: %vmid ← %v ± %mid
| #  #  '  #    #     #    #.....!.....!........|──(W)─┘
| #  #  '  #    #     #    #.....!.....!........|──(R)─10: %elem ← MEM[%vmid]
| #  #  '  #    #     #    '     #.....!........|─(W)─┘
| #  #  '  #    #     #    '     #     '        (R)─11: branch to local<else_if> if %x <> %elem
| #..#..!..#....#.....#....!.....#.....!........|──(a)─┘
:-----------------------------------------------: BB5
| #..#..!..#....!.....#....!.....!.....!........|──(b)─┐
| #  #  '  #    '     #....!.....!.....!........|─(R)─12: %high ← %mid ± 1
| #  #  '  #    #.....!....!.....!.....!........|──(W)─┘
| #  #  '  #    #     '    '     '     '        (R)─13: jmp local<end_if>
| #..#..!..#....#.....!....!.....!.....!........|──(a)─┘
:---------------[local__else_if]----------------: BB4
| #..#..!..!....#.....#....!.....#.....!........|──(b)─┐
| #  #  '  '    #     #    '     #.....!........|─(R)─15: branch to local<else> if %x <> %elem
| #..#..!..!....#.....#....!.....!.....!........|──(a)─┘
:-----------------------------------------------: BB3
| #..#..!..!....#.....#....!.....!.....!........|──(b)─┐
| #  #  '  '    #     #....!.....!.....!........|─(R)─16: %low ← %mid ± 1
| #  #  '  #....#.....!....!.....!.....!........|──(W)─┘
| #  #  '  #    #     '    '     '     '        (R)─17: jmp local<end_if>
| #..#..!..#....#.....!....!.....!.....!........|──(a)─┘
:-----------------[local__else]-----------------: BB2
| '  '  '  '    '     #....!.....!.....!........|──(b)─┐
| '  '  '  '    '     #....!.....!.....!........|─(R)─19: %retval ← %mid
| '  '  '  '    '     '    '     '     #........|──(W)─┘
| '  '  '  '    '     '    '     '     #........|──(R)─20: _ ← %retval
| '  '  '  '    '     '    '     '     '        (R)─21: ret
:-------[local__end_if; local__loop_cond]-------: BB1
| #..#..!..#....#.....!....!.....!.....!........|──(b)─┐
| #  #  '  #    #     '    '     '     '        (R)─24: branch to local<loop_top> if %low <> %high
| #..#..!..#....#.....!....!.....!.....!........|─(a)─┘
:-----------------------------------------------: BB0
| '  '  '  '    '     '    '     '     '        (R)─25: %retval ← -1
| '  '  '  '    '     '    '     '     #........|──(W)─┘
| '  '  '  '    '     '    '     '     #........|──(R)─26: _ ← %retval
| '  '  '  '    '     '    '     '     '        (R)─27: ret
'-+--+--+--+----+-----+----+-----+-----+--------'
  %x %v %n %low %high %mid %vmid %elem %retval   
```

## Algorithm Overview
(WIP: I'm still reading the paper, this is my current understanding)

### 1. Compute Live Ranges
The algorithm first computes for each temporary a set of live ranges (`LiveRange`) via a backwards pass over the program text.

Note: the algorithm largely treats the program text as a flat sequence of instructions, not as a control flow graph. This is intentional, but causes some problems that have to be patched back up in the Resolution step.

### 2. Assign Registers and Rewrite
...

### 3. Resolution
...

## Title Explanation

I kept reading the word "binpacking" as "backpinning" for some reason, so I'm using that (non-)word as the title (for now).
