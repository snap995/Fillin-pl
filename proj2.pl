/** Fillin Puzzle Solver
*
* Solves fill in style puzzles read from an input file where underscores
* represent spaces to-be-filled and hashes represent unfillable spaces and
* another input file with the word list to be used
*
* Outputs to another file similarly formatted, with the spaces filled with the
* fitting characters.
*
* For Declarative Programming (COMP30020)
* Semester 2, 2018
*
* @author Peter Schachte (Skeleton Code)
* @author James Taranto [640092] (Implementation)
*/

:- ensure_loaded(library(clpfd)).

:- use_module(library(pairs)).

main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(same_length(Row), Rows).


% solve_puzzle(Puzzle0, WordList, Puzzle)
% should hold when Puzzle is a solved version of Puzzle0, with the
% empty slots filled in with words from WordList.  Puzzle0 and Puzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  WordList is also a list of lists of
% characters, one list per word.
%
% This code is obviously wrong: it just gives back the unfilled puzzle
% as result.  You'll need to replace this with a working
% implementation.

%% 
sort_lists_by_length(Lists, ByLength) :-
        map_list_to_pairs(atom_length, Lists, Pairs),
		sort(1, @>=, Pairs, Sorted),
        pairs_values(Sorted, ByLength).

atoms_to_vars([],[]).
atoms_to_vars([A|As],[X|Xs]):-
     (A = '_' ->
      true;
      A = X),
    atoms_to_vars(As,Xs).

puzzle_to_vars(P, X) :-
	maplist(atoms_to_vars, P, X).

solve_puzzle(Puzzle0, WordList, Puzzle) :-
	puzzle_to_vars(Puzzle0, Puzzle),
	sort_lists_by_length(WordList, ByLength).
