# Som

Som is a functional programming language that is (should be) easy to use.

**FizzBuzz example:**
```haskell
#std::io

main() =
  1..100.for_each() => (
    it % 15 == 0 ? (
      io::println("FizzBuzz")
    ) : it % 3 == 0 ? (
      io::println("Fizz")
    ) : it % 5 == 0 ? (
      io::println("Buzz")
    ) : (
      io::println("%d", it)
    )
  ),
  0
```
