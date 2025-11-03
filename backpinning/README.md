# backpinning
A register allocator based on [Traub, Halloway, and Smith's *Second-chance Binpacking*](https://dl.acm.org/doi/10.1145/277652.277714) algorithm. Also influenced by the article [Cranelift, Part 4: A new Register Allocator](https://cfallin.org/blog/2022/06/09/cranelift-regalloc2/) by Chris Fallin.

## Interval Diagram
Here is an example of a generated interval diagram. The instructions of the program text are shown on the right-hand side of the diagram, so it should be read top-to-bottom.

There are (currently) two phases to an instruction's execution: `ReadArgs` and `WriteBack`. When a line is drawn horizontally from the left of an instruction (see instruction `03` below) it signifies the `ReadArgs` phase. A rounded corner (`┈╯`) drawn out of the bottom is for the `WriteBack` phase. The dotted vertical lines represent a temporary's non-live interval, and the solid vertical lines represent live intervals.

```
  %w %x %yeet %z
╔═╪══╪══╪═════╪═══╗
║ ┊  ┊  ┊     ┊   ╟┈00: local<test>
║ ┊  ┊  ┊     ┊   ╟┈01: %x ← 𜱪
║ ┊  𜸛┄┄┄┄┄┄┄┄┄┄┄┈╫┈╯
║ ┊  𜸩  ┊     ┊   ╟┈02: %yeet ← 𜱪
║ ┊  𜸩  𜸛┄┄┄┄┄┄┄┄┈╫┈╯
║ ┊  𜸽┄┄𜸩┄┄┄┄┄┄┄┄┈╫┈03: 𜱪  ← %x
║ ┊  ┊  𜸩     ┊   ╟┈04: 𜱪  ← %yeet
║ ┊  ┊  𜸩     ┊   ╟┈05: %z ← 𜱪
║ ┊  ┊  𜸩     𜸛┄┄┈╫┈╯
║ ┊  ┊  𜸽┄┄┄┄┄𜸩┄┄┈╫┈06: 𜱪  ← %yeet
║ ┊  ┊  ┊     𜸩   ╟┈07: 𜱪  ← %z
║ ┊  ┊  ┊     𜸽┄┄┈╫┈08: %w ← %z
║ 𜸛┄┄┄┄┄┄┄┄┄┄┄┄┄┄┈╫┈╯
║ 𜸩  ┊  ┊     ┊   ╟┈09: 𜱪  ← %w
║ 𜸽┄┄┄┄┄┄┄┄┄┄┄┄┄┄┈╫┈10: %x ← %w
║ ┊  𜸛┄┄┄┄┄┄┄┄┄┄┄┈╫┈╯
║ ┊  𜸽┄┄┄┄┄┄┄┄┄┄┄┈╫┈11: 𜱪  ← %x
╚═╪══╪══╪═════╪═══╝
  %w %x %yeet %z
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
