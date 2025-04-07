:- module(initial_screen, [show_initial_screen/1]).
:- use_module(screen_wrapper, [clear_screen/0, move_cursor/2]).
:- use_module(library(readutil)).


show_initial_screen(Choice) :-
    clear_screen,
    loop_menu(0, Choice).

loop_menu(CursorPos, Choice) :-
    clear_screen,
    print_menu(CursorPos),
    get_single_char(Code),
    char_code(Input, Code),
    (
        ( (Input = 'w')) -> 
            NewCursorPos is max(0, CursorPos - 1),
            loop_menu(NewCursorPos, Choice)

    ;   ( (Input = 's')) ->
            NewCursorPos is min(2, CursorPos + 1),
            loop_menu(NewCursorPos, Choice)

    ;   ( (Input = ' ') ; (Code = 13) ; (Code = 10) ) ->
            ( CursorPos =:= 2 ->
                how_to_play,
                loop_menu(2, Choice)
            ; 
                Choice = CursorPos
            )

    ;   loop_menu(CursorPos, Choice)
    ).

print_menu(CursorPos) :-
    Options = ["Contra outro jogador", "Contra Plínio (bot)", "Como jogar"],
    InitialLine = 5,
    print_centered("Mova com W,S ou ↑, ↓. Para selecionar aperte ESPAÇO ou ENTER", 0),
    print_options(Options, 0, CursorPos, InitialLine).

print_centered(Text, Y) :-
    string_length(Text, Len),
    screen_width(W),
    X is max(0, (W - Len) // 2),
    move_cursor(X, Y),
    write(Text),
    flush_output.

screen_width(150).
screen_height(48).



print_options([], _, _, _).
print_options([Opt|Rest], Index, CursorPos, Line) :-
    ( Index =:= CursorPos ->
        string_concat("-> ", Opt, Display)
    ; Display = Opt
    ),
    print_centered(Display, Line),
    NextIndex is Index + 1,
    NextLine is Line + 2,
    print_options(Rest, NextIndex, CursorPos, NextLine).

text_how_to_play(TextLines) :-
    RawText = "Para se mover utilize as teclas W, A, S, D, ou as setas\nPara selecionar use a tecla ESPAÇO ou ENTER\nAo selecionar uma peça aparecerá os possiveis movimentos para ela, caso seja de múltiplos movimentos terá que fazer movimento por movimento\nAo virar dama poderá se mover para as 4 diagonais",
    split_string(RawText, "\n", "", TextLines).

how_to_play :-
    clear_screen,
    text_how_to_play(Lines),
    print_lines(Lines, 0, 3),  % Começa a imprimir da linha 3

    % Desenha a opção "-> Voltar" no fim da tela
    screen_height(H),
    print_centered("-> Voltar", H - 2),

    flush_output,
    wait_for_enter_or_space.


print_lines([], _, _).
print_lines([Line|Rest], Index, StartY) :-
    Y is StartY + Index * 2,
    print_centered(Line, Y),
    NextIndex is Index + 1,
    print_lines(Rest, NextIndex, StartY).

wait_for_enter_or_space :-
    get_single_char(Code),
    ( Code = 32 ; Code = 13 ; Code = 10) -> true ; wait_for_enter_or_space.



