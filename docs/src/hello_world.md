# Hello World

The traditional "Hello World!" program implemented in Som looks as follows:

```som
from std::io use puts

let main = puts "Hello, World!"
```

[test](test.md)

Let's break this down line by line.

The code starts with an import statement. Since we want to output, or "print" a string, we'll need to import the function that allows us to do that. In this case, that's the function [`puts`]() from the [`io`]() module of the [standard library](standard_library.md).

Next is the [value binding](language/value-bindings.md) of `main`. Every Som program needs to define a value called `main`, which is where that program's execution starts. In this case, all that `main` does is use the imported `puts` function to print a string.

And that's it! Since Som is designed to be easy to use, there's practically no boiler-plate needed; As long as there's an entrypoint like `main` the program will run!

Now let's test the program by compiling and executing it. If you haven't installed the compiler yet, follow the [installation instructions](compiler/installation.md) first. After installing the compiler, save the program as `hello_world.som` and then invoke `somc`. You can then run the resulting executable:

```shell
$ # Linux or MacOS
$ somc hello_world.som
$ ./hello_world
Hello World!
$
```

```console
> # Windows
> somc.exe hello_world.som
> ./hello_world.exe
Hello World!
>
```

