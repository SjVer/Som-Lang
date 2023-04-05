# The Prelude Module

The `prelude` module provides basic features and is (when not [disabled](../compiler/compilation.md#prelude)) automatically included in every Som file.

```txt,doc
  Standard module implicitly imported by default into all Som modules unless
  explicitly disabled using the `no_prelude` or `no_stdlib` directive.
```

Manually having to import even the most trivial and commonly used symbols in every file would be overly verbose and tedious. Hence, this prelude is automatically imported in each Som file. It's kept as simple and light-weight as possible.

<style>h3 code::before { content: "std::prelude::" }</style>

## Values
---

### `return`
```som
let return v : 'a -> !'a
```
Wraps any value `v` in an effect.

### `exit`
```som
!!no_return
ext exit status : Int -> Nll
```
Terminates the calling process immediately with the given status.

### `assert`
```som
let assert cond msg : Bln -> Str -> Nll
```
Asserts that `cond` is true, and if not, prints `msg` to [`stderr`](io.md#stderr) before exiting with code 1.

## Imports
---

Publicly imported symbols:

```som
from std::types use *
from std::ops use *
from std::str use Str
```

Private dependencies:

```som
use std::types
use std::list
use std::ops
use std::str
from std::io use putsf, stderr
```

View the source code [here](../sources/prelude.md).