% Quicksort in λProlog / Prolog
% Classic divide-and-conquer sorting algorithm

% Base case: empty list is already sorted
qsort([], []).

% Recursive case: partition around pivot, sort each half
qsort([H|T], Sorted) :-
    partition(H, T, Less, Greater),
    qsort(Less, SortedLess),
    qsort(Greater, SortedGreater),
    append(SortedLess, [H|SortedGreater], Sorted).

% Partition: split list into elements less than and greater than pivot
partition(_, [], [], []).

partition(Pivot, [H|T], [H|Less], Greater) :-
    H =< Pivot,
    partition(Pivot, T, Less, Greater).

partition(Pivot, [H|T], Less, [H|Greater]) :-
    H > Pivot,
    partition(Pivot, T, Less, Greater).

% Standard append
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% Example queries:
% ?- qsort([5, 4, 8, 2, 4, 1], X).
% X = [1, 2, 4, 4, 5, 8]
%
% ?- qsort([3, 1, 4, 1, 5, 9, 2, 6], X).  
% X = [1, 1, 2, 3, 4, 5, 6, 9]
