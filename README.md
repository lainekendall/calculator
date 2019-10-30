# calculator

## Calculator Part One

To use the calculator run `stack run` after building the project with `stack build` and enter in executable expressions into the calculator like "2 * 18" 
### Developing

##### Set Up
Download Stack

Build the Calculator for the first time with `stack build`

##### Writing new code and testing it

Watch for file changes while running tests

```
stack test --file-watch
```

If Hlint fails, run `./hlint-fix` to fix all hlint errors. Re-run `stack test`.
