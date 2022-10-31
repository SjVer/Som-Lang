# Code Style for stdlib implementation

## Symbols

All symbols provided for the stdlib must be prefixed with the `_som` followed by the full section path of that symbol separated by underscores.

For example:

```haskell
-- Som
foo {
  FooTy := ...

  bar {
    baz: FooTy = ...
  }
}
```
```c
// C
_som_foo_FooTy _som_foo_bar_baz;
```

**NOTE:** In most cases symbols are prefixed with `som_`, instead of the desired prefix. After compilation these symbols are automatically renamed to their proper names using `objcopy` by the Makefile.

## Types

```haskell
-- Som
MyType := ...
```
```c
// C
typedef ... _som_MyType;
```

### Records

Fields that are provided for the stdlib must be named identically to their Som header counterparts. Fields that are only used internally by the implementation must be prefixed with an underscore.

```haskell
-- Som
MyRecord :=
  {
    foo: FooTy,
    bar: BarTy
  }
```
```c
// C
typedef struct {
    _som_FooTy foo;
    _som_BarTy bar;
    int _internal_field;
} _som_MyRecord;
```

**NOTE:** It is vital that the ordering of fields is identical in both the implementation and header.

## Functions

Function arguments must be named identically to their Som header counterparts.

```haskell
-- Som
foo bar baz : BarTy -> BazTy -> FooTy = ...
```
```c
// C
_som_FooTy _som_foo(_som_BarTy bar, _som_BazTy baz); 
```
