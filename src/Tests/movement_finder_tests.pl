:- begin_tests(movement_finder).
:- use_module('../movement_finder').
:- use_module('../utils').
:- use_module('../States/game_state').

make_empty_cell(Cell) :-
    Cell = cell{
        is_selected: false,
        is_available: false,
        player: none,
        is_king: false
    }.

make_cell_with_player(Player, Cell) :-
    Cell = cell{
        is_selected: false,
        is_available: false,
        player: Player,
        is_king: false
    }.

make_empty_cell_row(Row) :-
    length(Row, 8),
    maplist(=(cell{
        is_selected: false,
        is_available: false,
        player: none,
        is_king: false
    }), Row).


test(empty_state_returns_same_state) :-
    InitialState = state{selected: none},
    find_valid_moves(InitialState, false, ResultState),
    assertion(ResultState == InitialState).

test('set cell as available') :-
    make_empty_cell(EmptyCell),

    % Cria uma linha com 8 células vazias
    length(Row, 8), maplist(=(EmptyCell), Row),

    % Cria a matriz (8 linhas iguais)
    length(Matrix, 8), maplist(=(Row), Matrix),

    State = game_state{
        cursor: [0, 0],
        selected: none,
        matrix: Matrix,
        p1_count: 12,
        p2_count: 12,
        turn: p1,
        is_locked: false
    },

    set_available(2, 3, State, NewState),

    get_cell(2, 3, NewState, NewCell),

    assertion(NewCell.is_available == true).    

test(state_controlado) :-
    % Criar uma célula com p1
    make_cell_with_player(p1, P1Cell),
    make_empty_cell(Empty),

    % Criar uma linha onde a peça p1 está na posição (2,3)
    Row2 = [Empty, Empty, Empty, P1Cell, Empty, Empty, Empty, Empty],

    % Outras linhas vazias
    make_empty_cell_row(EmptyRow),
    Matrix = [
        EmptyRow,  % 0
        EmptyRow,  % 1
        Row2,      % 2 <- aqui está a peça
        EmptyRow,  % 3
        EmptyRow,  % 4
        EmptyRow,  % 5
        EmptyRow,  % 6
        EmptyRow   % 7
    ],

    GameState = game_state{
        cursor: [2,3],
        selected: [2,3],
        matrix: Matrix,
        turn: p1,
        is_locked: false
    },

    find_valid_moves(GameState, false, NewState),

    % Verificar se uma célula ficou disponível
    (
        member(Row, NewState.matrix),
        member(Cell, Row),
        Cell.is_available == true
    ->
        true
    ;
        fail
    ).


test('marca movimentos simples diagonais para peça de p1') :-
    % Criar uma célula com p1
    make_cell_with_player(p1, P1Cell),
    make_empty_cell(Empty),

    % Criar uma linha onde a peça p1 está na posição (2,3)
    Row2 = [Empty, Empty, Empty, P1Cell, Empty, Empty, Empty, Empty],

    % Outras linhas vazias
    make_empty_cell_row(EmptyRow),
    Matrix = [
        EmptyRow,  % 0
        EmptyRow,  % 1
        Row2,      % 2 <- aqui está a peça
        EmptyRow,  % 3
        EmptyRow,  % 4
        EmptyRow,  % 5
        EmptyRow,  % 6
        EmptyRow   % 7
    ],

    GameState = game_state{
        cursor: [2,3],
        selected: [2,3],
        matrix: Matrix,
        turn: p1,
        is_locked: false
    },


    find_valid_moves(GameState, false, NewState),

    get_cell(1, 2, NewState, CellA),
    get_cell(1, 4, NewState, CellB),
    get_cell(1, 3, NewState, CellC),

    assertion(CellA.is_available == true),
    assertion(CellB.is_available == true),
    assertion(CellC.is_available == false).


test('p1 capturando nas diagonais') :-
    % Criar uma célula com p1
    make_cell_with_player(p1, P1Cell),
    make_cell_with_player(p2, P2Cell),
    make_empty_cell(Empty),

    % Criar uma linha onde a peça p1 está na posição (2,3)
    Row2 = [Empty, Empty, Empty, P1Cell, Empty, Empty, Empty, Empty],

    % Outras linhas vazias
    make_empty_cell_row(EmptyRow),
    Matrix = [
        EmptyRow,  % 0
        [Empty, Empty, P2Cell, Empty, P2Cell, Empty, Empty, Empty],  % 1
        Row2,      % 2 <- aqui está a peça
        EmptyRow,  % 3
        EmptyRow,  % 4
        EmptyRow,  % 5
        EmptyRow,  % 6
        EmptyRow   % 7
    ],

    GameState = game_state{
        cursor: [2,3],
        selected: [2,3],
        matrix: Matrix,
        turn: p1,
        is_locked: false
    },


    find_valid_moves(GameState, false, NewState),

    get_cell(1, 2, NewState, CellA),
    get_cell(1, 4, NewState, CellB),
    get_cell(0, 1, NewState, CellC),
    get_cell(0, 5, NewState, CellD),

    assertion(CellA.is_available == false),
    assertion(CellB.is_available == false),
    assertion(CellC.is_available == true),
    assertion(CellD.is_available == true).

test('p1 captura em uma diagonal e a outra esta livre') :-
    % Criar uma célula com p1
    make_cell_with_player(p1, P1Cell),
    make_cell_with_player(p2, P2Cell),
    make_empty_cell(Empty),

    % Criar uma linha onde a peça p1 está na posição (2,3)
    Row2 = [Empty, Empty, Empty, P1Cell, Empty, Empty, Empty, Empty],

    % Outras linhas vazias
    make_empty_cell_row(EmptyRow),
    Matrix = [
        EmptyRow,  % 0
        [Empty, Empty, P2Cell, Empty, Empty, Empty, Empty, Empty],  % 1
        Row2,      % 2 <- aqui está a peça
        EmptyRow,  % 3
        EmptyRow,  % 4
        EmptyRow,  % 5
        EmptyRow,  % 6
        EmptyRow   % 7
    ],

    GameState = game_state{
        cursor: [2,3],
        selected: [2,3],
        matrix: Matrix,
        turn: p1,
        is_locked: false
    },


    find_valid_moves(GameState, false, NewState),

    get_cell(1, 2, NewState, CellA),
    get_cell(1, 4, NewState, CellB),
    get_cell(0, 1, NewState, CellC),
    get_cell(0, 5, NewState, CellD),

    assertion(CellA.is_available == false),
    assertion(CellB.is_available == true),
    assertion(CellC.is_available == true),
    assertion(CellD.is_available == false).


:- end_tests(movement_finder).



