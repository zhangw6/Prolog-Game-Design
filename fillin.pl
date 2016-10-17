% Author: Wen Zhang
% Student Id: 769355

% Game Description:
% A fillin puzzle is like a crossword puzzle,
% except that instead of being given obscure 
% clues telling which words go where
% Players are given a list of all the words 
% to place in the puzzle
% but not told where they go.

% Strategy Applied: 
% first, we transpose the columns of puzzle to rows
% second, we process the row to transform filtering 
% the "#" character and all the words which are less 
% than 2 characters to slots
% third, we find the slots with least candidate possible words
% fourth, we fill in the slot with the correct word
% we repeat the third and fourth steps
% finally, we get the correct Answer for the puzzle

% ensure that we use the right transpose function from clpfd library
:- ensure_loaded(library(clpfd)).

% The main entrypoint into the program
% PuzzleFile	The puzzle file to read in
% WordlistFile	The list of words to fill the puzzle with
% SolutionFile	The file to output the solution to
main(PuzzleFile, WordlistFile, SolutionFile) :-
	% Read in the puzzle and wordlist
	% Note: an underscore will be converted ino a variable
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),

	% Ensure the puzzle is actually valid
	valid_puzzle(Puzzle),

	% Actually solve the puzzle
	solve_puzzle(Puzzle, Wordlist),

	% Output the solved puzzle to the specified file
	print_puzzle(SolutionFile, Puzzle).

% Reads the given file
% Filename	The name of the file to read
% Content	Where the content will be stored to
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

% Reads all the lines of the given stream
% Stream	The stream to read from
% Content	Where to store the content onto
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

% Reads a single line of the given steam
% Stream	The stream to read from
% Line		The line to store to
% Last		Whether this is the last line or not
read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	; Char = '_'
	->	Line = [NewVariable|Line1],
	    read_line(Stream, Line1, Last)
	; Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

% Outputs a puzzle to the given file
% SolutionFile	Name of the file to print to
% Puzzle		The puzzle to actually print
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

% Outputs the given row to the given stream
% Stream	The stream to print to
% Row		The row to print
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

% Outputs a the given character to the given stream
% Stream	The stream to write to
% Char		The character to write
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

% Checks if the given puzzle is valid
% Puzzle	The puzzle to check
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).

% Checks of the dimensions match in the given puzzle
% List1	    The list to compare against List2
% List2	    The list to compare against List1
samelength([], []).
samelength([_|L1], [_|L2]) :-
	samelength(L1, L2).

% Generarate    the slots for whole puzzle
% Puzzle        the Puzzle to be filled with words
% WordList      the Wordlist to be filled into the slots
solve_puzzle(Puzzle, WordList) :-
	% transpose the colums of the Puzzle into rows
	transpose(Puzzle, PuzzleTransposed),

	% generate the slots for rows of puzzle
	slotsGenerator(Puzzle, [], Slots1),

	% generate the slots for columns(transposed rows) of puzzle
	slotsGenerator(PuzzleTransposed, Slots1, Slots),

    % fill in the remaining slots
	fillSlots(Slots, WordList).

% Generate the slots for the list of rows of puzzle
% [Row|OtherRows]    the list of rows of puzzle
% InputSlots         the slots we have generarated already
% OutputSlots        the final slots generarated from the list of rows
slotsGenerator([], InputSlots, InputSlots).
slotsGenerator([Row|OtherRows], InputSlots, OutputSlots) :-
    % generate  slots for one single row
    rowSlotGenerator(Row, [], [], TempSlots),

    % append the generarate slots to InputSlots
    append(TempSlots,InputSlots,Temp),

    % generarate the slots for remaining rows 
    slotsGenerator(OtherRows, Temp, OutputSlots).

% Generate the slots for a single row of puzzle
% [Char|Rest]      the row of puzzle 
% CurrentWord      the words at current stage 
% CurrentInputs    the generarated slots at current stage
% OutputSlots      the final result slots generarated from the slots
rowSlotGenerator([],[],CurrentInputs,CurrentInputs).
rowSlotGenerator([],CurrentWord,CurrentInputs,OutputSlots) :-
   (
       length(CurrentWord,L),L>1
   ->  % if '#' is not met and the length of CurrentWord is more than 1
       % append the CurrentWord to CurrentInputs
       append(CurrentInputs,[CurrentWord],TempInputs),
       rowSlotGenerator([],[],TempInputs, OutputSlots)
    ;  % if not, remain as the old CurrentInputs
       rowSlotGenerator([],[],CurrentInputs, OutputSlots)
    ).
rowSlotGenerator([Char|Rest], CurrentWord,CurrentInputs, OutputSlots) :-
   (	Char == '#',length(CurrentWord,L),L>1
   ->   % if we met '#' and the length of CurrentWord is more than 1
        % append the CurrentWord to CurrentInputs
        append(CurrentInputs,[CurrentWord],TempInputs),
        rowSlotGenerator(Rest,[],TempInputs, OutputSlots)
   ;
   (    
        Char == '#',length(CurrentWord,L),L<2
   ->   % if we met '#' and the length of CurrentWord is less than 2
        % remain as the old CurrentInputs
        rowSlotGenerator(Rest,[],CurrentInputs, OutputSlots)
    )
    ;
    (   
        length([Char],L),L>0
   ->    % if '#' is not met
         % concatenate the CurrentWord and [Char] into one single list
         concatenate(CurrentWord,[Char],Temp),
         rowSlotGenerator(Rest,Temp,CurrentInputs, OutputSlots)
    )
         % Generarate the slots for the rest of characters
    ;rowSlotGenerator(Rest,CurrentWord, CurrentInputs,OutputSlots)
    ).

% concatenate two lists into one single list
% List1    the first list to be concatenated
% List2    the second list to be concatenated
% Result   the concatenate list as output
concatenate(List1, List2, Result):-
   append(List1, List2, Result).

% fill in the lists of slots with the wordlist
% [Slot|Slots]    the list of slots to be filled in 
% WordList        the wordlist to be 
fillSlots([], _).
fillSlots([Slot|Slots], WordList) :-
    % get the length of current slot
    length(Slot,L),
   	% Calculate the slot with the least matches
	fewestCandidates(Slots, WordList, 50, Slot, BestSlot, [], NewSlots),

	% Fill a slot
	fillSlot(BestSlot, WordList, NewWordList, WordList),

	% Fill remaining slots
	fillSlots(NewSlots, NewWordList).

% fill in a slot with current WordList
% Slot               the slot to be filled in
% [Word|WordList]    the current Wordlist
% NewWordList        the filtered wordlist
% TotalWords         the total WordList without being filtered
fillSlot(Slot, [Word|WordList],NewWordList,TotalWords) :-
   % check if the slot can be bound to the candidate word
   bound(Slot, Word),
   % filter out the filled word from the TotalWordlist
   filterList(Word,TotalWords,NewWordList)
   ;
   % if cannot be bound, move to the next candidate word
   fillSlot(Slot, WordList, NewWordList,TotalWords).

% check if two instances are identical 
ifIdentical(X, Y) :-
   X == Y.

% filter a typical element from a list
% A       the element we want to disgard
% In      the original list
% Out     the filtered list
filterList(A, In, Out) :-
   exclude(ifIdentical(A), In, Out).

% find the slot with fewest candidate words
% [Slot|OtherSlots] the list of slots
% WordList            the candidate wordlist
% LeastMatches        the number the slot with 
%                     fewest Candidates at current stage
% CurrentSlot         the slot with
%                     fewest Candidates at current stage
% BestSlot            the slot with fewest Candidate words
% BoundSlots          the list of slots filtered cuurentslot
% FilteredSlots       the filtered list of slots without the BestSlot
fewestCandidates([], _, Answer, CurrentSlot, CurrentSlot,
	BoundSlots, BoundSlots).
fewestCandidates([Slot|OtherSlots], WordList, LeastMatches, CurrentSlot,
	BestSlot, BoundSlots, FilteredSlots) :-
	countCandidates(Slot, WordList, 0, TotalMatches),
	(	TotalMatches < LeastMatches
	->	
		append(BoundSlots, [CurrentSlot], NewSlotsTemp),
		fewestCandidates(OtherSlots, WordList, TotalMatches, Slot, BestSlot,
			NewSlotsTemp, FilteredSlots)

	;	
		append(BoundSlots, [Slot], NewSlotsTemp),
		fewestCandidates(OtherSlots, WordList, LeastMatches, CurrentSlot,
			BestSlot, NewSlotsTemp, FilteredSlots)
	).

% find the number of candidate words with a specific slot
% Slot                  the slot to be counted with the the number of 
%                       candidate words 
% CurrentMatches        the number we find candidate words at current stage
% TotalMatches          the number of candidate words with a specific slot
countCandidates(_, [], CurrentMatches, CurrentMatches).
countCandidates(Slot, [Word|OtherWords], CurrentMatches, TotalMatches) :-
	(	ifBound(Slot, Word)
		% if can be bound, CurrentMatches plus 1
	->	countCandidates(Slot, OtherWords, CurrentMatches+1, TotalMatches)
	    % if not, remain as the old CurrentMatches 
	;	countCandidates(Slot, OtherWords, CurrentMatches, TotalMatches)
	).
 
% check if a slot and a word can be bound 
% [W1|Word1]     the slot to be bound
% [W2|Word2]     the word bound to the slot
ifBound([], []).
ifBound([W1|Word1], [W2|Word2]) :-
	(	(W1 == W2; var(W1); var(W2))
	->	ifBound(Word1, Word2)
	).

% bound a slot with a word
% Word1     the slot or word
bound(Word1, Word1).

% get the numer of words who have the same length with a specifc slot
% [Word|OtherWords]               the current WordList
% SamelengthNumber                the cuurent slots having same length 
%                                 with Slot at cuurent stage
% CurrentSameLengthNumber         the total slots having same length 
%                                 with Slot at cuurent stage
sameLengthWord(Slot,[],CurrentSameLengthNumber,CurrentSameLengthNumber).
sameLengthWord(Slot,[Word|OtherWords],CurrentSameLengthNumber,SamelengthNumber) :-
((length(Slot,Ls),length(Word,Lw),Ls==Lw)
-> 	sameLengthWord(Slot,OtherWords,CurrentSameLengthNumber+1,SamelengthNumber)
;   sameLengthWord(Slot,OtherWords,CurrentSameLengthNumber,SamelengthNumber)
).

