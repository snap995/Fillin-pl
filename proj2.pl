/** Fillin Puzzle Solver
*
* Solves fill in style puzzles, reading the puzzle from PuzzleFile and words
* from WordlistFile, outputting the solved puzzle to SolutionFile
*
* PuzzleFile is a text file with underscores representing places to be filled and
* hashes corresponding to unfillable blocks, with each line representing a row
* of the puzzle
*
* WordlistFile is a text file containg the words used to fill the puzzle, each
* on a new line
*
* The SolutionFile is similarly formatted to the PuzzleFile, with the
* underscores replaced by the fitting word characters
*
* For Declarative Programming (COMP30020)
* Semester 2, 2018
*
* @author Peter Schachte (Skeleton Code)
* @author James Taranto [640092] (Implementation)
*/

:- ensure_loaded(library(clpfd)). % loads appropriate implementation of transpose/2

:- use_module(library(pairs)). % Used to sort lists of lists by length

/*
* main(+PuzzleFile, +WordlistFile, +SolutionFile).
*
* Entry point for program, solves PuzzleFile using WordlistFile, outputting to
* SolutionFile
*/
main(PuzzleFile, WordlistFile, SolutionFile) :-
    read_file(PuzzleFile, Puzzle),
    read_file(WordlistFile, Wordlist),
    valid_puzzle(Puzzle),
    solve_puzzle(Puzzle, Wordlist, Solved),
    print_puzzle(SolutionFile, Solved).

/*
* read_file(+Filename, -Content).
*
* Reads file Filename into Content
*/
read_file(Filename, Content) :-
    open(Filename, read, Stream),
    read_lines(Stream, Content),
    close(Stream).

/*
* read_lines(+Stream, -Content).
*
* Reads lines of input (Stream) and appends to Content
*/
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

/*
* read_line(+Stream, -Line, -Last).
*
* Does the actual Stream processing, handling EOF, new lines and chars to atoms
*/
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

/*
* print_puzzle(+SolutionFile, -Puzzle).
*
* Converts the internal solved puzzle to a text stream
*/
print_puzzle(SolutionFile, Puzzle) :-
    open(SolutionFile, write, Stream),
    maplist(print_row(Stream), Puzzle),
    close(Stream).

/*
* print_row(+Stream, -Row).
*
* Helper function for print_puzzle, parses every row
*/
print_row(Stream, Row) :-
    maplist(put_puzzle_char(Stream), Row),
    nl(Stream).

/*
* put_puzzle_char(+Stream, -Char).
*
* Maps the internal puzzle representation to text format, helper function for
* print_row
*/
put_puzzle_char(Stream, Char) :-
    (   var(Char)
    ->  put_char(Stream, '_')
    ;   put_char(Stream, Char)
    ).

/* 
* valid_puzzle(+Puzzle).
*
* Tests Puzzle, holds when all rows of puzzle are the same length
*/
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(same_length(Row), Rows).


/******************************************************************************
* 		                          Solution
******************************************************************************/

/*
* slot_from_row(+Row, ?CurrentSlot, -Slots).
*
* Base case for slots_from_row, if the CurrentSlot is greater in length that 1,
* add it to the slot list (Slots)
*/
slots_from_row([], CurrentSlot, Slots) :-
    length(CurrentSlot, N),
    (   N > 1
    ->  Slots = [CurrentSlot]
    ;   Slots = []
    ).

/*
* The meat-of-her, generates logical "Slots" from a list of logical variables
* and solids (hashes), finds runs of consecutive logical variables greater than
* 1, and adds these to a list of possible slots (Slots)
*/
slots_from_row([Var|Vars], CurrentSlot, Slots) :-
    (   Var \== '#'
    ->  append(CurrentSlot, [Var], CurrentSlot1),
        slots_from_row(Vars, CurrentSlot1, Slots)
    ;   length(CurrentSlot, N),
        (   N > 1
        ->  Slots = [CurrentSlot|Slots1]
        ;   Slots = Slots1
        ),
        slots_from_row(Vars, [], Slots1)
    ).

/*
* slots_from_rows(+Puzzle, -Slots).
*
* Gets slots from each row in a puzzle
*/
slots_from_rows([], []).
slots_from_rows([Row|Rows], Slots) :-
    slots_from_row(Row, [], Slots1),
    slots_from_rows(Rows, Slots2),
    append(Slots1, Slots2, Slots).

/*
* slots_from_puzzle(+Puzzle, -Slots).
*
* The interesting one, appends the SlotLists from the Slots found both
* horizontally and vertically in a puzzle, so that a word can be filled in in
* either direction
*/
slots_from_puzzle(Puzzle, Slots) :-
    slots_from_rows(Puzzle, Slots1),
    transpose(Puzzle, PuzzleT),
    slots_from_rows(PuzzleT, Slots2),
    append(Slots1, Slots2, Slots).

/*
* sort_lists_by_length(+Lists, -ByLength).
*
* Satisfied when ByLength is equivalent to Lists, sorted by the length of each
* list, descending.
*/
sort_lists_by_length(Lists, ByLength) :-
    map_list_to_pairs(length, Lists, Pairs),
    sort(1, @>=, Pairs, Sorted),
    pairs_values(Sorted, ByLength).

/*
* atoms_to_vars(+Atoms, -Vars).
*
* Satisfied when the list [X|Xs] is a list of atoms and logical variables where
* the list [A|As] is a list of atoms and underscores and the logical variables
* are replaced with underscores
*/
atoms_to_vars([],[]).
atoms_to_vars([A|As],[X|Xs]):-
     (A = '_' ->
      true;
      A = X),
    atoms_to_vars(As,Xs).

/*
* fill_puzzle(+Words, +Slots).
*
* Satisfied when all words in Words are unified with the slots in Slots.
*
* Picks a slot with the least matching words to fill, removing that Slot
* from the Slots and Words, unifying the slot with a word in the process, and
* recurses through the new word and slot lists until all slots and words are
* depleted.
*/
fill_puzzle([], []).
fill_puzzle(Words, Slots) :-
    find_best_slot(Words, Slots, Slot, Matches),
    member(Slot, Matches),
    select(Slot, Slots, Slots1),
    select(Slot, Words, Words1),
    fill_puzzle(Words1, Slots1).

/*
* find_slot_word_matches(+Words, +Slots, -SlotMatchPairs).
*
* This funky looking predicate takes a word list and slot list and recurses
* through the slots, finding all possible words that can fit in that slot, and
* appending them to a pair of Matches-Slot.
*/
find_slot_word_matches(_, [], []).
find_slot_word_matches(Words, [Slot|Slots], [Matches-Slot|Pairs]) :-
    bagof(Slot, member(Slot, Words), Matches),
    find_slot_word_matches(Words, Slots, Pairs).

/*
* length_of_key(+Pairs, -PairOfPairs).

* Helper predicate, takes a pair and gives a pair of pairs, where the key of the
* new pair of pairs is the length of the key of nester pair.
* In otherwords, it's a mappable predicate for getting the length of the keys in
* a list of pairs
*/
length_of_key(Key-Value, N-Pair) :-
    length(Key, N),
    Pair = Key-Value.

/*
* find_best_slot(+Words, +Slots, -Slot, -Matches).
*
* Finds the possible words for each slot, sorts them by amount of possible
* words, and gives you the slot with the least amount of possible matches.
*
* Along with the best Slot, it returns a list of the words that match that
* slot, potentially reducing the search space when selecting a word to fill
* a slot.
*/
find_best_slot(Words, Slots, Slot, Matches) :-
    find_slot_word_matches(Words, Slots, SlotWordMatches),
    maplist(length_of_key, SlotWordMatches, SlotWordMatchesWithLength),
    keysort(SlotWordMatchesWithLength, SlotWordMatchesByLength),
    SlotWordMatchesByLength = [_-(Matches-Slot)|_].

/* 
* solve_puzzle(+Puzzle, +Words, -Solved).
*
* Satisfied when Solved is a solution for Puzzle using Words to fill the puzzle
* Replaces the underscores with logical variables, finds the runs of logical
* variables (slots) both horizontally and vertically, unifies the words from
* the word list with the slots of the puzzle until a solution is found.
*/
solve_puzzle(Puzzle, Words, Solved) :-
    maplist(atoms_to_vars, Puzzle, Solved),
    slots_from_puzzle(Solved, Slots),
    sort_lists_by_length(Words, WordsByLength),
    fill_puzzle(WordsByLength, Slots).