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
    %writeln(">>>>>>>>>>>"),
    %write("Visited: "),
    %writeln(Visited),
    %writeln(X),
    append([X], Visited, NewVisited),
    %write("NewVisited: "),
    %writeln(NewVisited),
    atom_string(X, StringX), 
    get_right_parts(StringX, Lines, RelevantLines),
    %write("Relevant: "),
    %writeln(X),
    getUppercaseCharsList(RelevantLines, UppercaseChars),
    %write("Chars: "),
    %writeln(UppercaseChars),
    append(Xs, UppercaseChars, UpdatedXs),
    %write("Updated: "),
    %writeln(UpdatedXs),
    %writeln(">>>>>>>>>>>"),
    findUnreachable1(UpdatedXs, Lines, NewVisited, Result).


print_list([]).
print_list([transition(Left, Right)|Xs]) :-
    format('~w -> ~w~n', [Left, Right]),
    print_list(Xs).


get_unique_lefts(Transitions, UniqueLefts) :-
    findall(Left, member(transition(Left, _), Transitions), Lefts),
    list_to_set(Lefts, UniqueLefts).



element_to_string(Element, Element) :-
    string(Element).

element_to_string(Element, String) :-
    atom(Element),
    atom_string(Element, String).

element_to_string(Element, String) :-
    number(Element),
    number_string(Element, String).

element_to_string(Element, String) :-
    term_to_atom(Element, Atom),
    atom_string(Atom, String).

list_to_string([], []).

list_to_string([Head|Tail], [StringHead|StringTail]) :-
    element_to_string(Head, StringHead),
    list_to_string(Tail, StringTail).


transition_member(Element, List) :-
    member(transition(Element, _), List).

filter_transition_left([], _, []).
filter_transition_left([transition(Left, Right) | T], List1, [transition(Left, Right) | Filtered]) :-
    member(Left, List1),
    filter_transition_left(T, List1, Filtered).
filter_transition_left([_ | T], List1, Filtered) :-
    filter_transition_left(T, List1, Filtered).


main :- 
    read_transitions('grammar.txt', Trn),
    print_list(Trn),
    %get_right_parts("B", Trn, Relevant),
    %writeln(Relevant),
    findUnreachable1(["S"], Trn, [], Result),
    get_unique_lefts(Trn, R),
    list_to_string(Result, Result1),
    list_to_string(R, R1),
    write("Reachable non-terminals: "),
    writeln(Result1),
    write("All non-terminals: "),
    writeln(R1),
    subtract(R1, Result1, Unreachable),
    write("Unreachable non-terminals: "),
    writeln(Unreachable),
    filter_transition_left(Trn, Result1, Res),
    writeln("Filtered grammar: "),
    print_list(Res).

:- initialization(main).




