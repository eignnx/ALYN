# ALYN
A compiler backend. Goal is to have loosely-coupled target language.

## Phases

1. Parsing: builds the `ast`
1. Typechecking/assign types to every subexpression.
1. Convert `ast` to `ir`
1. Instruction selection: convert `ir` to machine-specific intermediate form
1. Register allocation
1. Code emission
