:- use_module(library(readutil)).


transition(Left, Right).


read_transitions(File, Transitions) :-
    open(File, read, Stream),
    read_transitions_from_stream(Stream, Transitions),
    close(Stream).

read_transitions_from_stream(Stream, Transitions) :-
    read_line_to_codes(Stream, Line),
    ( Line \= end_of_file ->
        parse_line(Line, Transition),
        read_transitions_from_stream(Stream, RestTransitions),
        Transitions = [Transition | RestTransitions]
    ; Transitions = []
    ).

parse_line(Line, transition(Left, Right)) :-
    atom_codes(AtomLine, Line),
    atomic_list_concat([LeftAtom, RightAtom], '>', AtomLine),
    atom_string(LeftAtom, Left),
    atom_string(RightAtom, Right).


getUppercaseChars(String, UppercaseChars) :-
    atom_chars(String, Chars),
    findall(C, (member(C, Chars), char_type(C, upper)), UppercaseChars).

get_right_parts(_, [], []).
get_right_parts(Left, [transition(Left, Right)|T], [Right|Rest]) :-
    get_right_parts(Left, T, Rest).
get_right_parts(Left, [_|T], Right) :-
    get_right_parts(Left, T, Right).


getUppercaseCharsList([], []).
getUppercaseCharsList([Word|Words], UppercaseCharsList) :-
    getUppercaseChars(Word, UppercaseChars),
    getUppercaseCharsList(Words, RestUppercaseCharsList),
    append(UppercaseChars, RestUppercaseCharsList, UppercaseCharsList).



findUnreachable1([], _, Visited, Visited).
findUnreachable1([X|Xs], Lines, Visited, Result) :-
    member(X, Visited),
    findUnreachable1(Xs, Lines, Visited, Result).
findUnreachable1([X|Xs], Lines, Visited, Result) :-
    \+ member(X, Visited),
    %writeln(Visited),
    %writeln(X),
    append([X], Visited, NewVisited),
    %writeln(NewVisited),
    get_right_parts(X, Lines, RelevantLines),
    %writeln(RelevantLines),
    getUppercaseCharsList(RelevantLines, UppercaseChars),
    %writeln(UppercaseChars),
    append(Xs, UppercaseChars, UpdatedXs),
    findUnreachable1(UpdatedXs, Lines, NewVisited, Result).


main :- 
    read_transitions('grammar.txt', Trn),
    writeln(Trn),
    %get_right_parts("A", Trn, Relevant),
    %writeln(Relevant),
    findUnreachable1(["S"], Trn, [], Result),
    write("Reachable non-terminals: "),
    writeln(Result).

:- initialization(main).




