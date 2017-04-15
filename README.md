# arithmetic-meme

This repo contains a solution in prolog to a simple arithmetic puzzle that went viral-enough on the internet that it popped up on reddit or hackernews or somewhere.

This puzzle seemed like the perfect excuse to write my first Prolog program.

# Problem statement

Here are the relevant bits of the problem statement, copy/pasted from [here][puzzle].

> All you need to do is place the digits from 1 to 9 in the the grid. Easy, right?
>
> ![puzzle][puzzlepng]
>
> ...
>
> You need to fill in the gaps with the digits from 1 to 9 so that the equation makes sense, following the order of operations - multiply first, then division, addition and subtraction last.

# Running the code

## Example SWI-Prolog session

```
?- [solve].
true.

?- run_tests.
% PL-Unit: solve ................................................................................................................................................................................................................................................................................................ done
% All 288 tests passed
true.

?- solutions(X).
X = [3, 9, 2, 8, 1, 5, 6, 7, 4] ;
X = [3, 9, 2, 8, 1, 5, 7, 6, 4] ;
X = [8, 9, 2, 3, 1, 5, 6, 7, 4] ;
X = [8, 9, 2, 3, 1, 5, 7, 6, 4] .
```

## Example lisp session

```
CL-USER> (load "solve")
T
CL-USER> (run-tests)
T
CL-USER> (solve)
...list of solutions...
```

# Runtime comparisons

Here is a table showing the runtimes of the various solutions when run on my aging laptop. They range from "pretty fast" when using the [clpfd][clpfd] library on SICStus to "dog slow" when using the [clpq][clpqr] library on SWI Prolog. In fairness, part of the reason the SWI Prolog solutions are slow is that I still have no idea how to write Prolog.

| variant (time units are seconds)   | SWI Prolog | SICStus |
|------------------------------------|------------|---------|
| normal_precedence_constraint_clpfd |      6.236 |   0.150 |
| linear_precedence_constraint_clpfd |      3.360 |   0.060 |
| normal_precedence_constraint_clpq  |     62.731 |  12.230 |
| linear_precedence_constraint_clpq  |     63.987 |  12.690 |

This repo also contains a reference solution in lisp. The lisp solution is a brute-force search over the 9! possible input permutations, and makes no attempt to be fast.

| variant (time units are seconds)         |  SBCL | CLISP |   CCL |
|------------------------------------------|-------|-------|-------|
| normal-precedence-constraint-satisfied-p | 0.596 |  2.12 | 0.977 |
| linear-precedence-constraint-satisfied-p | 0.722 |  3.13 | 1.217 |


[puzzle]: https://www.theguardian.com/science/alexs-adventures-in-numberland/2015/may/20/can-you-do-the-maths-puzzle-for-vietnamese-eight-year-olds-that-has-stumped-parents-and-teachers
[puzzlepng]: https://github.com/appleby/arithmetic-meme/raw/master/puzzle.png
[clpfd]: https://sicstus.sics.se/sicstus/docs/3.7.1/html/sicstus_33.html
[clpqr]: http://www.swi-prolog.org/pldoc/man?section=clpqr
