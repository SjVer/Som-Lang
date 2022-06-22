# Som

Som is a functional programming language that is (should be) easy to use.

**FizzBuzz example:**
```haskell
#std::io

main() =
    1..100 for_each() => n (
        n % 15 == 0 ? io::println("fizzbuzz")
        : n % 3 == 0 ? io::println("fizz")
        : n % 5 == 0 ? io::println("buzz")
        : io::println("%d", n)
    ),
    0
```
