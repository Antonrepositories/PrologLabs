animal(собака, має_шерсть, немає_крил).
animal(сапсан, немає_шерсті, має_крила).
animal(свиня, має_шерсть, немає_крил).
animal(сова, немає_шерсті, має_крила).
animal(сорока, немає_шерсті, має_крила).
animal(саламандра, немає_шерсті, немає_крил).
animal(снігурка, немає_шерсті, має_крила).
animal(сорокопуд, немає_шерсті, має_крила).


тварина_має_шерсть(X) :-
    animal(X, має_шерсть, _).

тварина_має_крила(X) :-
    animal(X, _, має_крила).

тварина_без_шерсті_і_крил(X) :-
    animal(X, немає_шерсті, немає_крил).

птах(X) :- animal(X), (X = сова; X = сорока; X = снігурка; X = сорокопуд; X = сапсан).

ссавець(X) :- animal(X), (X = собака; X = свиня).

земноводна(X) :- animal(X), X = саламандра.

хижак(X) :- animal(X), X = сапсан.

