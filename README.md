# usbar

Usbar is a literate programming system for LaTeX and an arbitrary source language. In the example client implementation in `app/Main.lhs`, I specifically use C as an output, but the code could easily be adapted to interpret any language (as long as that language does not overlap with the three Usbar commands).

An example Usbar file is included in the `test` directory, under `test/ChaCha.u`. You can see all three commands Usbar uses in that example, showing how it's intended to be written. In progress are more extensive tests for all the ways an Usbar file could break, and an actual test suite that will read and produce expected results.

# TODO:
1. More comprehensive tests
2. Automated test suite that can be run with `stack test`
3. Documentation for `app/Main.lhs`
4. A simple client for another language, like Python or Haskell
5. Un-hardcode the C-specific `listings` formatting directive
6. More comprehensive error checking in the library
