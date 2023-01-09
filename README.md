# Som

Som is a functional programming language focussed on readability and simplicity.

As of right now it is a work in progress.

**FizzBuzz example:**

```haskell
-- imports
use puts from std::io
use iter from std::list
use show_int from std::int

-- fizzbuzz function
let fizzbuzz = switch
  | n if n % 15 = 0 then puts "FizzBuzz"
  | n if n % 5 = 0 then puts "Fizz"
  | n if n % 3 = 0 then puts "Buzz"
  | n then puts (show_int n)

-- main function
let main _ =
  let ns = 1..100 in
  iter fizzbuzz ns,
  0
```
