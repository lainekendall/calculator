# calculator
This repository holds my very first Haskell project: a calculator. This calculator uses instrumental Haskell concepts such as Monads and Applicatives as well as instrumental *parsing* concepts such as parser combinators. In order to really dive deep into these concepts I split them into different versions of a calculator.

### Parser Model
For all 3 versions of the calculator I used the same Parser model which is a record newtype containing a function that takes a `String` and returns a `Maybe` tuple of the parsed value and the rest of the string to parse. We write Monad, Applicative, Functor, and Alternative in order to give our Parser object enough capacity to combine and extract.

### Evaluator
The evaluator picks up where the `Parser` leaves off. This function takes the return type of `runParse` and returns an `Integer` which is the computed value of the input.

### Calculator Part One
The first version of the calculator is as simple a parser that I could make. It uses Monads and Applicatives but not any Parser Combinators. It bluntly matches over characters to parse the operators, for example, using a case statement to match on each operator character (`+ - * /`). In this version, parsers are never *combined* or reused except for in the parse expression functions, where they have to be. That is where Applicatives and Monads come into the picture. 

### Calculator Part Two
Calculator Part Two simplifies Part One by using the `Parser`'s `Functor` instance and `fmap`ing over functions. By allowing ourselves to use the more complicated concept of `Functor` we save complexity and code. Instead of clunkily case matching each operator character, we can `fmap` over a list of these operators and the parser function.

### Calculator Part Three
The final version of the calculator introduces a concept that is fundamental to parsing in Haskell: Parser combinators. Parser combinators are why parsing in Haskell is so easy. The concept is that you can combine two simple parsers to make a more complicated parser. The parser combinators defined in my calculator are based on concepts in the [Parser Combinators package](https://hackage.haskell.org/package/parser-combinators-1.2.1/docs/Control-Applicative-Combinators.html). 

## Developing

##### Set Up
1. Download [Stack](https://docs.haskellstack.org/en/stable/README/)
1. Build the Calculator for the first time with `stack build`

##### Running
To use the calculator run `stack run` after building the project with `stack build` and enter in executable expressions into the calculator like "2 * 18" 

##### Testing
Watch for file changes while running tests

```
stack test --file-watch
```

If Hlint fails, run `./hlint-fix` to fix all hlint errors. Re-run `stack test`.
