:- set_prolog_flag(encoding, utf8).
:- use_module('States/game_state', [make_initial_state/1]).
:- use_module('Handles/Ui/render', [render_board/1]).
:- use_module('Handles/navigation', [moveUp/2, moveDown/2, moveLeft/2, moveRight/2]).
:- use_module('Handles/handle_selection', [handle_selection/2]).
:- use_module('Handles/handle_action', [handle_action/2]).
:- use_module('Handles/plinio', [handle_turn/2]).


main :-
    hide_cursor,
    make_initial_state(State),
    loop(State).

loop(StateIn) :-
    % (StateIn.turn == p2 -> 
    %     handle_turn(StateIn, State)
    % ; 
    %     State = StateIn
    % ),
    State = StateIn,
    render_board(State),

    get_single_char(Code),
    char_code(Input, Code),
    ( Input = 'q' -> (show_cursor, halt)
    ; update_state(Input, State, NewState),
    loop(NewState)
    ).

update_state(Command, State, NewState) :-
    ( Command = 'w' -> moveUp(State, NewState)
    ; Command = 'a' -> moveLeft(State, NewState)
    ; Command = 's' -> moveDown(State, NewState)
    ; Command = 'd' -> moveRight(State, NewState)
    ; Command = ' ' -> handle_action(State, NewState)
    ; NewState = State
    ).

% Predicados para controle do cursor
hide_cursor :- format('~s', ['\033[?25l']), flush_output.
show_cursor :- format('~s', ['\033[?25h']), flush_output.
