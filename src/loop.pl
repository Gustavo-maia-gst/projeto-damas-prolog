:- module(loop, [loop/2]).
:- use_module('Handles/Ui/screen_wrapper', [hide_cursor/0, show_cursor/0]).
:- use_module('Handles/plinio', [handle_turn/2]).
:- use_module('Handles/handle_action', [handle_action/2]).
:- use_module('Handles/navigation', [moveUp/2, moveDown/2, moveLeft/2, moveRight/2]).
:- use_module('Handles/Ui/render', [refresh/1]).
:- use_module('Handles/Ui/screen_wrapper', [refresh_matrix/3, refresh_header/3, clear_screen/0]).

loop(StateAux, Choice) :-
    ((Choice =:= 1, StateAux.turn == p2) ->
        handle_turn(StateAux, State)
    ;
        State = StateAux),
    refresh(State),
    check_end_game(Choice, State),
    get_single_char(Code),
    char_code(Input, Code),
    ( Input = 'q' -> (show_cursor, halt)
    ; update_state(Input, Code, State, NewState),
      loop(NewState, Choice)
    ).

update_state(Command, Code, State, NewState) :-
    ( 
      Command = ' ' -> handle_action(State, NewState)
    ; Command = '\r' -> handle_action(State, NewState)

    ; Command = 'w' -> moveUp(State, NewState)
    ; Code =:= 65 -> moveUp(State, NewState)

    ; Command = 'a' -> moveLeft(State, NewState)
    ; Code =:= 68 -> moveLeft(State, NewState)

    ; Command = 's' -> moveDown(State, NewState)
    ; Code =:= 66 -> moveDown(State, NewState)

    ; Code =:= 67 -> moveRight(State, NewState)
    ; Command = 'd' -> moveRight(State, NewState)
    
    ; NewState = State
    ).

check_end_game(Choice, State) :-
    P1Count = State.p1_count,
    P2Count = State.p2_count,
    ( P1Count =:= 0 ->
        ( Choice =:= 1 ->
            print_end("Plínio")
        ; print_end("Jogador 2")
        )
    ; P2Count =:= 0 ->
        print_end("Jogador 1")
    ; true
    ).

print_end(Winner) :-
    hide_cursor,
    clear_screen,
    format("~n~w Ganhou!! 🥳🥳~n", [Winner]),
    halt.