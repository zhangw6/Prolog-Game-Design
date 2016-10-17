sameLengthWord(Slot,WordList,0,SameLenNum),

	length(Wordlist,L),

	% Calculate the slot with the least matches
	leastCombos(Slots, WordList, 50, Slot, BestSlot, [], NewSlots),



sameLengthWord(Slot,[],CurrentSameLengthNumber,CurrentSameLengthNumber).
sameLengthWord(Slot,[Word|OtherWords],CurrentSameLengthNumber,SamelengthNumber) :-
((length(Slot,Ls),length(Word,Lw),Ls==Lw)
-> 	sameLengthWord(Slot,OtherWords,CurrentSameLengthNumber+1,SamelengthNumber)
;   sameLengthWord(Slot,OtherWords,CurrentSameLengthNumber,SamelengthNumber)
).


leastCombos([], Wordlist, Answer, CurrentSlot, CurrentSlot).
leastCombos([Slot|OtherSlots], WordList, LeastMatches, CurrentSlot,BestSlot) :-
% Workout how many combos there are for the given slot
getCombos(Slot, WordList, 0, TotalMatches),
(	TotalMatches < LeastMatches
->	% This is the new best slot, update our values

leastCombos(OtherSlots, WordList, TotalMatches, Slot, BestSlot)

;	% Old Slot is better, ignore this slot
leastCombos(OtherSlots, WordList, LeastMatches, CurrentSlot,BestSlot)
).











fillSlot(Slot, [Word|WordList],NewWordList,TotalWords) :-



unify(Slot, Word),
filterList(Word,TotalWords,NewWordList)


;

fillSlot(Slot, WordList, NewWordList,TotalWords).




are_identical(X, Y) :-
X == Y.

filterList(A, In, Out) :-
exclude(are_identical(A), In, Out).









buildSlots([], InputSlots, InputSlots).
buildSlots([Row|OtherRows], InputSlots, OutputSlots) :-
processRow(Row, [], [], TempSlots),
append(TempSlots,InputSlots,Temp),
buildSlots(OtherRows, Temp, OutputSlots).







processRow([],[],CurrentInputs,CurrentInputs).


processRow([],CurrentWord,CurrentInputs,OutputSlots) :-
(
length(CurrentWord,L),L>1
->
append(CurrentInputs,[CurrentWord],TempInputs),
processRow([],[],TempInputs, OutputSlots)


;
processRow([],[],CurrentInputs, OutputSlots)



).

processRow([Char|Rest], CurrentWord,CurrentInputs, OutputSlots) :-
(	Char == '#',length(CurrentWord,L),L>1
->
append(CurrentInputs,[CurrentWord],TempInputs),
processRow(Rest,[],TempInputs, OutputSlots)

;
(
Char == '#',length(CurrentWord,L),L<2
->     processRow(Rest,[],CurrentInputs, OutputSlots)

)
;
(
length(Char,L),L>0
->
append(CurrentWord,Char,Temp),
processRow(Rest,Temp,CurrentInputs, OutputSlots)
)
;processRow(Rest,CurrentWord, CurrentInputs,OutputSlots)
).




