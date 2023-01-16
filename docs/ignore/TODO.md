- Instead of TAST expr variants wrap AST expr variants in node with type info
- Remove TL_Section from TAST
- Rework name resolution (AST -> symbol table)
- Replace typechecker input with symbol table instead of AST
- Rework typechecker symbol handling
<br>
- Fix issue where value used in submodule is canonicalized wrongly
<br>
- Switch to toplevel external decl instead of implicit
<br>
- Rename struct fields with prefix?
  