%% -*- mode: Prolog; -*-
%%
%% Solutions to the following arithmetic puzzle. See the included
%% README file for more details.
%%
%% You need to fill in the gaps with the digits from 1 to 9 so that
%% the equation makes sense, following the order of operations -
%% multiply first, then division, addition and subtraction last.
%%
%% +----+    +----+----+----+    +----+
%% |    |    |    |  - |    |    | 66 |
%% +----+    +----+----+----+    +----+
%% |  + |    |  x |    |  - |    |  = |
%% +----+    +----+    +----+    +----+
%% | 13 |    | 12 |    | 11 |    | 10 |
%% +----+    +----+    +----+    +----+
%% |  x |    |  + |    |  + |    |  - |
%% +----+    +----+    +----+    +----+
%% |    |    |    |    |    |    |    |
%% +----+----+----+    +----+----+----+
%% |  : |    |  + |    |  x |    |  : |
%% +----+----+----+    +----+----+----+
%%
%% This source file is compatible with (at least):
%%     SWI Prolog 7.4.1
%%     SICStus 4.3.5
%%
%% Here is an example swipl session:
%%
%% ?- [solve].
%% true.
%%
%% ?- run_tests.
%% % PL-Unit: solve ................................................................................................................................................................................................................................................................................................ done
%% % All 288 tests passed
%% true.
%%
%% ?- solutions(X).
%% X = [3, 9, 2, 8, 1, 5, 6, 7, 4] ;
%% X = [3, 9, 2, 8, 1, 5, 7, 6, 4] ;
%% X = [8, 9, 2, 3, 1, 5, 6, 7, 4] ;
%% X = [8, 9, 2, 3, 1, 5, 7, 6, 4] .

%% The normal_precedence_constraint_* goals represent solutions to the
%% puzzle when we interpret it using normal operator precedence rules.
%%
%% The linear_precedence_constraint_* goals represent solutions to the
%% puzzle when we interpret it as applying operations in a linear
%% fashion. That is, the highest precedence operator is always the
%% left-most. A better prefix for these linear_* goals might be
%% same_precedence_left_associative_, but that's quite a mouthful. The
%% problem statement above makes it clear that normal precedence rules
%% are to be used. But the version I first came across did not
%% explicitly state precedence rules. I mistakenly assumed that I was
%% meant to apply operations in a linear fashion, due to the way the
%% puzzle is depicted. Hence, I've left the linear_* constraints here
%% for comparison purposes.
%%
%% The *_clpq variants use the clpq library for handling constraints
%% over the rationals, and are more-or-less direct translations of the
%% problem statement into algebraic form. Constants have been folded
%% together on the left-hand-side of the equation, but no other
%% simplifications have been made. This approach has the advantage of
%% making it easy to see at a glance that the clpq variants do compute
%% the intended function. The downside is that the clpq variants are
%% much slower than the corresponding clpfd variants. Hence, the clpq
%% variants are only used for testing, as a way to verify the results
%% produced by the faster clpfd variants.
%%
%% The *_clpfd variants encode the same constraints as the clpq
%% variants, but the equations have been expanded to get rid of any
%% rationals.

:- use_module(library(clpfd)).
:- use_module(library(clpq)).

linear_precedence_constraint_clpq(A1,A2,A3,A4,A5,A6,A7,A8,A9) :-
    {76 = (((((((((((A1 + 13) * A2) / A3) + A4) + 12) * A5) - A6) - 11) + A7) * A8) / A9)}.

linear_precedence_constraint_clpfd(A1,A2,A3,A4,A5,A6,A7,A8,A9) :-
    %% The intermediate Z term is not needed, but doesn't seem to
    %% affect performance and makes the equation easier to read.
    Z #= 76 * A9 + A8 * (A6 - A7 + 11 - 12 * A5 - A4 * A5),
    Z * A3 #= (A1 + 13) * A2 * A5 * A8.

normal_precedence_constraint_clpq(A1,A2,A3,A4,A5,A6,A7,A8,A9) :-
    {87 = A1 + (13 * A2 / A3) + A4 + 12 * A5 - A6 + (A7 * A8 / A9)}.

normal_precedence_constraint_clpfd(A1,A2,A3,A4,A5,A6,A7,A8,A9) :-
    Z #= 87 - (A1 + A4 + 12 * A5 - A6),
    Z * A3 * A9 #= 13 * A2 * A9 + A3* A7 * A8.

%% SICStus has clpfd:domain/3, SWI has clpfd:ins/2.
:- if(\+ current_predicate(clpfd:domain/3)).
:- if(current_predicate(clpfd:ins/2)).
goal_expansion(domain(Vars, Min, Max), ins(Vars, Min..Max)).
:- endif.
:- endif.

solutions(Vars) :-
    solutions(normal_precedence_constraint_clpfd, Vars).

solutions(Constraint, Vars) :-
    Vars = [A1,A2,A3,A4,A5,A6,A7,A8,A9],
    domain(Vars,1,9),
    all_distinct(Vars),
    call(Constraint,A1,A2,A3,A4,A5,A6,A7,A8,A9),
    labeling([ffc,enum], Vars).


%% SWI has time/1, but SICStus does not. Define a (somewhat) portable
%% timing predicate to keep output consistent between SWI and SICStus.
timeit(P) :-
    statistics(runtime, [T0|_]),
    P,
    statistics(runtime, [T1|_]),
    T is T1 - T0,
    format('~w took ~3d sec.~n', [P, T]).

:- if(\+ current_predicate(forall/2)).
%% Not sure how to portably determine where to load forall/2 from,
%% since SICStus doesn't have source_exports/2 or require/1.  Forall/2
%% is a built-in on SWI, so if it's not available, just assume SICStus
%% and load library(aggregate).
:- use_module(library(aggregate)).
:- endif.
print_timings :-
    forall(member(C,[normal_precedence_constraint_clpfd,
		     normal_precedence_constraint_clpq,
		     linear_precedence_constraint_clpfd,
		     linear_precedence_constraint_clpq]),
	   user:timeit(forall(solutions(C,_), true))).

%% Here is an example repl session showing output of print_timings/0
%% on SWI and SICStus:
%%
%% ? sicstus --noinfo -l solve.pl --goal print_timings.
%% SICStus 4.3.5 (x86_64-linux-glibc2.17): Tue Dec  6 10:41:06 PST 2016
%% Licensed to Mike Appleby
%% forall(solutions(normal_precedence_constraint_clpfd,_1597),true) took 0.150 sec.
%% forall(solutions(normal_precedence_constraint_clpq,_1597),true) took 12.230 sec.
%% forall(solutions(linear_precedence_constraint_clpfd,_1597),true) took 0.060 sec.
%% forall(solutions(linear_precedence_constraint_clpq,_1597),true) took 12.690 sec.
%%
%% ? swipl -l solve.pl -g version. -g print_timings.
%% Welcome to SWI-Prolog (threaded, 64 bits, version 7.4.1)
%% ...
%% forall(solutions(normal_precedence_constraint_clpfd,_62),true) took 6.236 sec.
%% forall(solutions(normal_precedence_constraint_clpq,_66),true) took 62.731 sec.
%% forall(solutions(linear_precedence_constraint_clpfd,_60),true) took 3.360 sec.
%% forall(solutions(linear_precedence_constraint_clpq,_54),true) took 63.987 sec.

:- use_module(library(plunit)).
:- begin_tests(solve).

check_solution(Constraint, Vars) :-
    sort(Vars, [1,2,3,4,5,6,7,8,9]),
    % SICStus doesn't have apply/2, so use call/10 instead.
    Vars = [A1,A2,A3,A4,A5,A6,A7,A8,A9],
    call(Constraint,A1,A2,A3,A4,A5,A6,A7,A8,A9).

test(solve_normal, [forall(solutions(normal_precedence_constraint_clpfd, Vars))]) :-
    check_solution(normal_precedence_constraint_clpq, Vars).

test(solve_linear, [forall(solutions(linear_precedence_constraint_clpfd, Vars))]) :-
    check_solution(linear_precedence_constraint_clpq, Vars).

%% To verify the number of solutions generated as well as each
%% individual solution, we could do the following:
%%
%% :- use_module(library(lists)).
%%
%% check_num_solutions(N, Constraint1, Constraint2) :-
%%     bagof(V, solutions(Constraint1, V), Vs),
%%     length(Vs,N),
%%     maplist(check_solution(Constraint2), Vs).
%%
%% test(solve_normal_verify_number_of_solutions) :-
%%     check_num_solutions(136, normal_precedence_constraint_clpfd, normal_precedence_constraint_clpq).
%%
%% test(solve_linear_verify_number_of_solutions) :-
%%     check_num_solutions(152, linear_precedence_constraint_clpfd, linear_precedence_constraint_clpq).

:- end_tests(solve).
