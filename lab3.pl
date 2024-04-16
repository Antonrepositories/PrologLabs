:- use_module(library(readutil)).

transition(CurrentState, InputSymbol, NextState).

automaton(Transitions, InitialState, FinalStates).

is_final_state(State, FinalStates) :-
    member(State, FinalStates).

find_words(_, _, [], _, []).
find_words(CurrentState, VisitedStates, _, FinalStates, Words) :-
    is_final_state(CurrentState, FinalStates),
    Words = [""].
find_words(CurrentState, VisitedStates, Transitions, FinalStates, Words) :-
    \+ member(CurrentState, VisitedStates), 
    findall(Word,
            ( member(transition(CurrentState, Symbol, NextState), Transitions),
              find_words(NextState, [CurrentState | VisitedStates], Transitions, FinalStates, NextWords),
              member(NextWord, NextWords),
              atom_concat(Symbol, NextWord, Word)
            ),
            Words).


has_even_length([]) :-
    fail.
has_even_length([String|_]) :-
    string_length(String, Length),
    Length mod 2 =:= 0.
has_even_length([_|Rest]) :-
    has_even_length(Rest).

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

parse_line(Line, transition(CurrentState, InputSymbol, NextState)) :-
    atom_codes(AtomLine, Line),
    atomic_list_concat([CurrentStateAtom, InputSymbolAtom, NextStateAtom], ' ', AtomLine),
    atom_string(CurrentStateAtom, CurrentState),
    atom_string(InputSymbolAtom, InputSymbol),
    atom_string(NextStateAtom, NextState).

read_states(FileName, States) :-
    open(FileName, read, Stream),        
    read_string(Stream, _, String),      
    close(Stream),                      
    split_string(String, " ", "", States).

main :-
    % Визначення переходів та інших характеристик автомату
    %Transitions = [transition('q0', 'a', 'q1'), transition('q1', 'b', 'q2'), transition('q2', 'a', 'q3')],
    InitialState = "0",
    %FinalStates = ["1"],
    read_transitions('transitions.txt', Trn),
    read_states('finalstates.txt', States),
    write(Trn),
    writeln(States),
    find_words(InitialState, [], Trn, States, Words),
    writeln(Words),
    (has_even_length(Words) ->
        writeln('At least one string has even length')
    ;   writeln('No string has even length')
    ),
    halt.

:- initialization(main).
