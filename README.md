# Som

Som is a functional programming language that is (should be) easy to use.

**FizzBuzz example:**
```haskell
#std::io

fizzbuzz(n) =
  n % 15 == 0 ? println("fizzbuzz") :
	n % 3 == 0 ? println("fizz") :
	n % 5 == 0 ? println("buzz") :
	println("%d", n)

main() =
  1..100 for_each() => n fizzbuzz(n),
  
  0
```
