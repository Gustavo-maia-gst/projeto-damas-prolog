:- module(screen_wrapper, [refresh_matrix/3, clear_screen/0, refresh_header/3, move_cursor/2]).

clear_screen :- write('\033[2J').
move_cursor(X, Y) :- format('\033[~d;~dH', [Y, X]).

get_cell_char((Char, _, _), Char).
get_cell_bgcolor((_, BgColor, _), BgColor).
get_cell_color((_, _, Color), Color).

bgcolor_ansi(0, '\033[49m').  
bgcolor_ansi(1, '\033[43m').  
bgcolor_ansi(2, '\033[44m').  
bgcolor_ansi(3, '\033[42m').  

color_ansi(0, '\033[39m').    
color_ansi(1, '\033[33m').    
color_ansi(2, '\033[34m').    
color_ansi(3, '\033[32m').    

apply_bgcolor(Code) :-
    bgcolor_ansi(Code, Ansi),
    write(Ansi).

apply_color(Code) :-
    color_ansi(Code, Ansi),
    write(Ansi).

reset_formatting :-
    write('\033[0m').

refresh_header(State, InitialLine, InitialColumn) :-
    move_cursor(InitialColumn, InitialLine),
    write('Turno:'),

    PlayerLine is InitialLine + 1,
    move_cursor(InitialColumn, PlayerLine),
    (State.turn == p1 -> write('Jogador 1') ; write('Jogador 2')),

    PecasHeaderLine is PlayerLine + 2,
    move_cursor(InitialColumn, PecasHeaderLine),
    write('Peças:'),

    P1Line is PecasHeaderLine + 1,
    move_cursor(InitialColumn, P1Line),
    format('P1: ~d', [State.p1_count]),

    P2Line is P1Line + 1,
    move_cursor(InitialColumn, P2Line),
    format('P2: ~d', [State.p2_count]).

refresh_matrix(Matrix, InitialLine, InitialColumn) :-
    forall(nth0(Line, Matrix, LineContent), 
        forall(nth0(Column, LineContent, _),
            writeChar(Matrix, Line, Column, InitialLine, InitialColumn)
        )
    ).

writeChar(Matrix, Line, Column, InitialLine, InitialColumn) :-
    nth0(Line, Matrix, LineContent),
    nth0(Column, LineContent, Cell),

    RealLine is InitialLine + Line,
    RealColumn is InitialColumn + Column,
    move_cursor(RealColumn, RealLine),

    get_cell_char(Cell, Char),
    get_cell_bgcolor(Cell, BgColor),
    get_cell_color(Cell, Color),
    apply_bgcolor(BgColor),
    apply_color(Color),

    write(Char),

    reset_formatting.

