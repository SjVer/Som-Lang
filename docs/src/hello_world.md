# Hello World

The traditional "Hello World!" program implemented in Som looks as follows:

```som,file=hello_world.som
{{#include ../som/hello_world.som}}
```

Let's break this down line by line.

The code starts with an import statement. Since we want to output, or "print" a string, we'll need to import the function that allows us to do that. In this case, that's the function [`puts`]() from the [`io`]() module of the [standard library](standard_library.md).

Next is the [value binding](language/value-bindings.md) of `main`. Every Som program needs to define a value called `main`, which is where that program's execution starts. In this case, all that `main` does is use the imported `puts` function to print a string.

And that's it! Since Som is designed to be easy to use, there's practically no boiler-plate needed; As long as there's an entrypoint like `main` the program will run!

Now let's test the program by compiling and executing it:

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

