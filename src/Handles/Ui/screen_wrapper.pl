:- module(screen_wrapper, [refresh_screen/3]).

:- use_module(library(readline)).

% Constantes para os códigos ASCII das teclas
:- dynamic key_code/2.
key_code(w, 119).
key_code(s, 115).
key_code(a, 97).
key_code(d, 100).
key_code(q, 113).

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

refresh_screen(Matrix, InitialLine, InitialColumn) :-
    clear_screen,
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

