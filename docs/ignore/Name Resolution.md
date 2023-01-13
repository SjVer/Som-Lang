### Algorithm
1. Add all symbols to a symbol table with absolute paths for the current file and all imported files recursively
2. Apply import statements recursively adding to the symbol table of the importing file
3. Remove duplicates(?) and unused symbols

### Example

#### Source
```haskell
-- mymod.som
let mymod_fn x = x
let mymod_fn2 y = mymod_fn y
```
```haskell
-- myfile.som
use mymod_fn from mymod
use mymod

mod submod {
  let submod_fn z = z
}

let main =
  mymod_fn 1,
  mymod::mymod_fn2 2,
  submod::submod_fn 3
```

#### Step 1
Renaming symbols to absolute paths
```haskell
-- mymod.som
let mymod::mymod_fn x = x
let mymod::mymod_fn2 y = mymod::mymod_fn y
```
```haskell
-- myfile.som
use mymod_fn from mymod
use mymod

-- mod submod
let myfile::submod::submod_fn z = z

let main =
  myfile::mymod_fn 1,
  myfile::mymod::mymod_fn2 2,
  myfile::submod::submod_fn 3
```

##### Symbol tables:
mymod.som:

| Path | Id | Value |
|---|---|---|
| mymod::mymod_fn | 01 | `\x -> x` |
| mymod::mymod_fn2 | 02 | `\y -> mymod::mymod_fn y` |

myfile.som:

| Path | Id | Value |
|---|---|---|	
| myfile::submod::submod_fn | 03 | `\z -> z` |
| | myfile::main | 04 | `...` |

#### Step 2
Applying import statements
```haskell
-- use mymod_fn from mymod
let myfile::mymod_fn x = x

-- use mymod
let myfile::mymod::mymod_fn x = x
let myfile::mymod::mymod_fn2 y = mymod::mymod_fn y

-- mod submod
let myfile::submod::submod_fn z = z

let main =
  myfile::mymod_fn 1,
  myfile::mymod::mymod_fn2 2,
  myfile::submod::submod_fn 3
```

##### Symbol table:

| Path | Id | Value |
|---|---|---|
| myfile::mymod_fn | 01 | `\x -> x` |
| myfile::mymod::mymod_fn | 01 | `\x -> x` |
| myfile::mymod::mymod_fn2 | 02 | `\y -> mymod::mymod_fn y` |
| myfile::submod::submod_fn | 03 | `\z -> z` |
| myfile::main | 04 | `...` |

#### Step 3
Removing duplicates and unused symbols
```haskell
-- use mymod_fn from mymod
let myfile::mymod_fn x = x

-- use mymod
let myfile::mymod::mymod_fn x = x -- duplicate?
let myfile::mymod::mymod_fn2 y = mymod::mymod_fn y

-- mod submod
let myfile::submod::submod_fn z = z

let main =
  myfile::mymod_fn 1,
  myfile::mymod::mymod_fn2 2,
  myfile::submod::submod_fn 3
```

##### Symbol table:

| Path | Id | Value |
|---|---|---|
| myfile::mymod_fn | 01 | `\x -> x` |
| myfile::mymod::mymod_fn (?) | 01 | `\x -> x` |
| myfile::mymod::mymod_fn2 | 02 | `\y -> mymod::mymod_fn y` |
| myfile::submod::submod_fn | 03 | `\z -> z` |
| myfile::main | 04 | `...` |
