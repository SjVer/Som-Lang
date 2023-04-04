# Introduction

Som is functional programming made simple. It offers everything the casual functional programmer would want in a straightforward and consise manner. Because of this, it is great for both those who are newcomers to functional programming and those who feel like the big functional languages like Haskell are a bit too complex or hard-to-read.

Naturally, the official Som compiler, *somc*, is written in OCaml and uses LLVM as backend to generate high-performance executables. This makes Som interoperable with most non-functional languages such as C and C++.

Som also comes with a [standard library](standard_library.md) that's not bloated, yet provides everything necessary for most casual projects. And thanks to Som's interoperability with low-level languages such as C, any other low-level functionalities or existing libraries in other languages can easily be added.

Not unlike most reference manuals this one is not (yey) complete and might leave some questions unanswered. Please feel free to [open an issue](https://github.com/SjVer/Som-Lang/issues/new) if you have any questions. Do however mind that Som, including its documentation, is still in development. Some example files can be found in the `test/` directory Som's [github repository](https://github.com/SjVer/Som-Lang).

<i class="right-side">"form follows function"</i>