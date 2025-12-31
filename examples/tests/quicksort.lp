% =============================================================================
% Quicksort in λProlog
% =============================================================================
% Classic divide-and-conquer sorting algorithm using higher-order features.
%
% Run with:
%   rvm phi examples/λProlog.phi examples/tests/quicksort.lp -- "qsort([5,4,8,2,4,1], X)."

% -----------------------------------------------------------------------------
% Base case: empty list is sorted
% -----------------------------------------------------------------------------

qsort([], []).

% -----------------------------------------------------------------------------
% Recursive case: partition around pivot, sort sublists, concatenate
% -----------------------------------------------------------------------------

qsort([Pivot|Rest], Sorted) :-
    partition(Pivot, Rest, Less, Greater),
    qsort(Less, SortedLess),
    qsort(Greater, SortedGreater),
    append(SortedLess, [Pivot|SortedGreater], Sorted).

% -----------------------------------------------------------------------------
% Partition: split list into elements ≤ pivot and > pivot
% -----------------------------------------------------------------------------

partition(_, [], [], []).

partition(Pivot, [H|T], [H|Less], Greater) :-
    H =< Pivot,
    partition(Pivot, T, Less, Greater).

partition(Pivot, [H|T], Less, [H|Greater]) :-
    H > Pivot,
    partition(Pivot, T, Less, Greater).

% -----------------------------------------------------------------------------
% Standard list append
% -----------------------------------------------------------------------------

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% -----------------------------------------------------------------------------
% Example queries
% -----------------------------------------------------------------------------
%
% ?- qsort([5, 4, 8, 2, 4, 1], X).
%    X = [1, 2, 4, 4, 5, 8]
%
% ?- qsort([3, 1, 4, 1, 5, 9, 2, 6], X).
%    X = [1, 1, 2, 3, 4, 5, 6, 9]
%
% ?- qsort([], X).
%    X = []
%
% ?- qsort([42], X).
%    X = [42]

% -----------------------------------------------------------------------------
% Higher-order variant using filter (λProlog style)
% -----------------------------------------------------------------------------

qsort_ho([], []).

qsort_ho([P|Xs], Sorted) :-
    filter (x\ x =< P) Xs Less,
    filter (x\ x > P) Xs Greater,
    qsort_ho(Less, SL),
    qsort_ho(Greater, SG),
    append(SL, [P|SG], Sorted).

% Filter with higher-order predicate
filter _ [] [].
filter P [H|T] [H|R] :- P H, filter P T R.
filter P [H|T] R :- not (P H), filter P T R.
