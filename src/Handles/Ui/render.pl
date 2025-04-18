:- module(render, [refresh/1]).

:- use_module(screen_wrapper, [refresh_matrix/3, refresh_header/3, clear_screen/0]).
:- use_module('../../States/game_state'). 

cell_height(1).
cell_width(3).

base_bgcolor(0).
base_color(0).
available_bgcolor(1).
available_color(0).
selected_bgcolor(2).
selected_color(0).
cursor_bgcolor(3).
cursor_color(0).

refresh(GameState) :-
    matrix_dims(GameState, BoardRows, BoardCols),
    cell_height(CH), cell_width(CW),
    DisplayHeight is (CH + 1) * BoardRows + 1,
    DisplayWidth is (CW + 1) * BoardCols + 1,

    StartLine is 2,
    StartCol is 45,
    HeaderStartLine is 5,
    HeaderStartCol is 15,

    make_display_matrix(GameState, DisplayHeight, DisplayWidth, DisplayMatrix),

    clear_screen,
    refresh_header(GameState, HeaderStartLine, HeaderStartCol),
    refresh_matrix(DisplayMatrix, StartLine, StartCol).

matrix_dims(GameState, Rows, Cols) :-
    Matrix = GameState.matrix,
    length(Matrix, Rows),
    (Rows > 0 -> nth0(0, Matrix, FirstRow), length(FirstRow, Cols) ; Cols = 0).


make_display_matrix(GameState, Height, Width, DisplayMatrix) :-
    HeightAux is Height - 1,
    WidthAux is Width - 1,
    findall(
        Row,
        (   between(0, HeightAux, I),
            findall( (Char, BgColor, Color),
                     (between(0, WidthAux, J), get_display_char(I, J, Height, Width, GameState, (Char, BgColor, Color))),
                     Row
            )
        ),
        DisplayMatrix
    ).


get_display_char(I, J, Height, Width, GameState, (Char, BgColor, Color)) :-
    cell_height(CH), cell_width(CW),
    LineMod is I mod (CH + 1),
    ColMod is J mod (CW + 1),
    EndLine is Height - 1,
    EndCol is Width - 1,
    base_bgcolor(BaseBg), base_color(BaseColor),
    
    
    (
        I == 0, J == 0                                                          -> Char = '┌', BgColor = BaseBg, Color = BaseColor
    ;   I == 0, J == EndCol                                                     -> Char = '┐', BgColor = BaseBg, Color = BaseColor
    ;   I == EndLine, J == EndCol                                               -> Char = '┘', BgColor = BaseBg, Color = BaseColor
    ;   I == EndLine, J == 0                                                    -> Char = '└', BgColor = BaseBg, Color = BaseColor
    ;   I == 0, ColMod == 0, J \= 0, J \= EndCol                                -> Char = '┬', BgColor = BaseBg, Color = BaseColor
    ;   I == EndLine, ColMod == 0, J \= 0, J \= EndCol                          -> Char = '┴', BgColor = BaseBg, Color = BaseColor
    ;   J == 0, LineMod == 0, I \= 0, I \= EndLine                              -> Char = '├', BgColor = BaseBg, Color = BaseColor
    ;   J == EndCol, LineMod == 0, I \= 0, I \= EndLine                         -> Char = '┤', BgColor = BaseBg, Color = BaseColor
    ;   LineMod == 0, ColMod == 0, I \= 0, I \= EndLine, J \= 0, J \= EndCol    -> Char = '┼', BgColor = BaseBg, Color = BaseColor
    ;   LineMod == 0                                                            -> Char = '─', BgColor = BaseBg, Color = BaseColor
    ;   ColMod == 0                                                             -> Char = '│', BgColor = BaseBg, Color = BaseColor
    ;   
        get_cell_content_char(I, J, GameState, (Char, BgColor, Color))
    ).


get_cell_content_char(I, J, GameState, (Char, BgColor, Color)) :-
    cell_height(CH), cell_width(CW),
    CellLine is I div (CH + 1),
    CellCol is J div (CW + 1),
    nth0(CellLine, GameState.matrix, RowContent),
    nth0(CellCol, RowContent, Cell),

    [CursorLine, CursorCol] = GameState.cursor,

    % Verificar se a célula está selecionada
    IsSelected = (GameState.selected \= none, 
                 GameState.selected = [SelLine, SelCol], 
                 CellLine =:= SelLine, 
                 CellCol =:= SelCol),

    
    (   (CellLine =:= CursorLine, CellCol =:= CursorCol)    -> cursor_bgcolor(BgColor), cursor_color(Color)
    ;   IsSelected                                          -> selected_bgcolor(BgColor), selected_color(Color)
    ;   Cell.is_available == true                           -> available_bgcolor(BgColor), available_color(Color)
    ;   base_bgcolor(BgColor), base_color(Color)
    ),
    
    
    MiddleRowOffset is (CH + 1) // 2,
    MiddleColOffset is (CW + 1) // 2,

    (   
        Cell.player \= none
    ->  (   Cell.is_king == true
            -> (   Cell.player == p1 -> Char = '◉'
                ;   Cell.player == p2 -> Char = '◍'
                )
            ;   (   
                    I mod (CH + 1) =:= MiddleRowOffset,
                    J mod (CW + 1) =:= MiddleColOffset -> (
                        Cell.player == p1 -> Char = '●'
                        ;   Cell.player == p2 -> Char = '○'
                    ) ; 
                    Char = ' '
                )
        )
    ;   Char = ' '
    ).