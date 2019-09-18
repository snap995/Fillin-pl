# Fillin-pl

A [fill-in puzzle](https://en.wikipedia.org/wiki/Fill-In_(puzzle)) solver written in prolog using constraint logic programming.

## Usage

Call the `main` predicate with the puzzle, wordlist and desired output file. Samples are provided in the `samples` directory.

### Format

Puzzles must be supplied as a text document, using `_` to represent empty blocks, and `#` filled blocks, with each line of the file representing one row of the puzzle.

Word lists are newline-separated text documents, containing a case-sensitive word on each line.

### Example

```
$ swipl
?- [proj2]
?- main(samples/puzzle1, samples/words1, out)
```
