# The IO Module

The `io` module provides I/O functionality, such as opening files and printing to the terminal.

```som
-- Standard module containing types and functions for (file) input/output.
```

This module's most trivial functionality, such as printing to the terminal, is included in the prelude.

<style>h3 code::before { content: "std::io::" }</style>

## Types
---

### `IOMode`
```som
type IOMode of
  read,
  write,
  append,
  read_write
```
The different modes for opening a [file](#file).

<!-- This kind of entry could be generated in the future -->
### <code class="language-som hljs">File</code> 
<pre><code class="language-som hljs">type File of
  path: <a href="str.md#str">Str</a>,
  descr: <a href="types.md#int">Int</a>,
  mode: <a href="#iomode">IOMode</a>
</code></pre>
A handle to a file.

## Values
---

### `stdin`
```som
ext stdin : File
```
A read-only [file handle](#file) to the standard input, "stdin".

### `stdout`
```som
ext stdout : File
```
An append-only [file handle](#file) to the standard output, "stdout".

### `stderr`
```som
ext stderr : File
```
An append-only [file handle](#file) to the standard error output, "stderr".

### `openf`
```som
ext openf path mode : Str -> IOMode -> !File
```
Opens the file with the given `path` and [`IOMode`](#iomode).

### `closef`
```som
ext closef file : File -> !Nll
```
Closes the given file.

### `putsf`
```som
ext putsf file str : File -> Str -> !Nll
```
Writes [string](str.md#Str) `str` to the given [File](#file), if it has been opened with mode [`write`](#iomode), [`read_write`](#iomode) or [`append`](#iomode).

### `puts`
```som
let puts str : Str -> !Nll
```
Prints the given [string](str.md#Str) to [`stdout`](#stdout).

## Imports

---

Publicly imported symbols:

```som
from std::str use Str
```
