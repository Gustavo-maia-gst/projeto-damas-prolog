:- module(render, [render_board/1]).

:- use_module(screen_wrapper, [refresh_screen/3]).
:- use_module('../../States/game_state'). % Para acessar a estrutura do game_state, se necessário.

% Constantes para layout da célula (como no Haskell)
cell_height(1).
cell_width(3).

% Códigos de cor (mapeamento do Haskell/screen_wrapper)
% Assumindo: 0=default, 1=available(yellow), 2=selected(blue), 3=cursor(green)
base_bgcolor(0).
base_color(0).
available_bgcolor(1).
available_color(0). % Branco sobre Amarelo
selected_bgcolor(2).
selected_color(0). % Branco sobre Azul
cursor_bgcolor(3).
cursor_color(0).  % Branco sobre Verde

render_board(GameState) :-
    % 1. Obter dimensões do terminal (opcional, por enquanto usar fixo)
    TermLines = 24,
    TermCols = 80,

    % 2. Calcular dimensões da matriz de exibição
    matrix_dims(GameState, BoardRows, BoardCols),
    cell_height(CH), cell_width(CW),
    DisplayHeight is (CH + 1) * BoardRows + 1,
    DisplayWidth is (CW + 1) * BoardCols + 1,

    % 3. Calcular posição inicial para centralizar (aproximado)
    StartLine is max(1, (TermLines - DisplayHeight) // 2),
    StartCol is max(1, (TermCols - DisplayWidth) // 2),
    HeaderCol is max(1, StartCol // 3), % Coluna para os cabeçalhos

    % 5. Criar a matriz de exibição
    make_display_matrix(GameState, DisplayHeight, DisplayWidth, DisplayMatrix),

    % 6. Chamar refresh_screen para desenhar a matriz
    refresh_screen(DisplayMatrix, StartLine, StartCol).

% Predicado para obter dimensões da matriz do jogo
matrix_dims(GameState, Rows, Cols) :-
    Matrix = GameState.matrix,
    length(Matrix, Rows),
    (Rows > 0 -> nth0(0, Matrix, FirstRow), length(FirstRow, Cols) ; Cols = 0).

% Predicado principal para criar a matriz de exibição
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

% Determina o caractere e as cores para uma posição (I, J) na matriz de exibição
get_display_char(I, J, Height, Width, GameState, (Char, BgColor, Color)) :-
    cell_height(CH), cell_width(CW),
    LineMod is I mod (CH + 1),
    ColMod is J mod (CW + 1),
    EndLine is Height - 1,
    EndCol is Width - 1,
    base_bgcolor(BaseBg), base_color(BaseColor),
    
    % Lógica das bordas (similar ao makeChar)
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
    ;   % Dentro de uma célula do tabuleiro
        get_cell_content_char(I, J, GameState, (Char, BgColor, Color))
    ).

% Determina o caractere e as cores para o *conteúdo* de uma célula do tabuleiro
get_cell_content_char(I, J, GameState, (Char, BgColor, Color)) :-
    cell_height(CH), cell_width(CW),
    CellLine is I div (CH + 1),
    CellCol is J div (CW + 1),
    nth0(CellLine, GameState.matrix, RowContent),
    nth0(CellCol, RowContent, Cell),
    
    % Determina a cor baseada no estado da célula (cursor, selecionada, disponível)
    (   Cell.is_under_cursor == true -> cursor_bgcolor(BgColor), cursor_color(Color)
    ;   Cell.is_selected == true     -> selected_bgcolor(BgColor), selected_color(Color)
    ;   Cell.is_available == true    -> available_bgcolor(BgColor), available_color(Color)
    ;   base_bgcolor(BgColor), base_color(Color)
    ),
    
    % Calcula os offsets para o centro da célula
    MiddleRowOffset is (CH + 1) // 2,
    MiddleColOffset is (CW + 1) // 2,
    
    % Verifica se estamos na posição central da célula e se há um jogador
    (   I mod (CH + 1) =:= MiddleRowOffset,
        J mod (CW + 1) =:= MiddleColOffset,
        Cell.player \= none
    ->  (   Cell.is_king == true
            -> (   Cell.player == p1 -> Char = '◉'
                ;   Cell.player == p2 -> Char = '◍'
                ;   Char = 'K'
                )
            ;   (   Cell.player == p1 -> Char = '●'
                ;   Cell.player == p2 -> Char = '○'
                ;   Char = '?'
                )
        )
    ;   Char = ' '
    ).

% Implementação dos próximos passos (make_display_matrix, etc.)
% ... (será adicionado a seguir) 