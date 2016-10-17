




rowSlotsGenerator([],TempSlots,SlotsAppendFrom) :-
(

append([1],TempSlots,SlotsAppendFrom);

).




rowSlotsGenerator([Elem|OtherElems],TempSlots,SlotsAppendFrom) :-
(  Elem =='#'
-> (
rowSlotsGenerator(OtherElems,newTempSlots,SlotsAppendFrom)
)
)
;  append(TempSlots,[Elem],newTempSlots),
rowSlotsGenerator(OtherElems,newTempSlots,SlotsAppendTo,SlotsAppendFrom)
).





