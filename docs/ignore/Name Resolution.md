### Algorithm
1. Add all symbols to a symbol table with absolute paths for the current file and all imported files recursively
2. Apply import statements recursively adding to the symbol table of the importing file

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

| Path | Pointing to |
|---|---|
| mymod::mymod_fn | `\x -> x` |
| mymod::mymod_fn2 | `\y -> mymod::mymod_fn y` |

myfile.som:

| Path | Pointing to |
|---|---|	
| myfile::submod::submod_fn | `\z -> z` |
| myfile::main | `...` |

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

| Path | Pointing to |
|---|---|
| myfile::mymod_fn | `\x -> x` |
| myfile::mymod::mymod_fn | `\x -> x` |
| myfile::mymod::mymod_fn2 | `\y -> mymod::mymod_fn y` |
| myfile::submod::submod_fn | `\z -> z` |
| myfile::main | `...` |

### Design

Types:
```ocaml
module IMap = Map.Make(Ident)

type 'a entry =
  {
    symbol: 'a;
    original_ident: Ident.t;
    usages: Span.t list;
  }

type ('v, 't) t =
  {
    values: 'v entry IMap.t;
    types: 't entry IMap.t;
  }
```

Usage:
1. Parsing:
   Just construct the AST as usual.
2. Name resolution:
   Execute algorithm with a `(value_definition, type_definition) t`.
   Pass the table and the AST to the typechecker.
3. Typechecking:
   Transform existing symbol table into a `(???, ???) t`.
