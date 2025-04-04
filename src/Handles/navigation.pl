:- module(navigation, [moveUp/2, moveDown/2, moveLeft/2, moveRight/2]).

updatePosition(State, Ydiff, Xdiff, NewState) :-
    Cursor = State.cursor,
    Cursor = [Y, X],
    NewY is Y + Ydiff,
    NewX is X + Xdiff,
    Matrix = State.matrix,
    length(Matrix, NumRows),
    (Matrix = [FirstRow|_] -> length(FirstRow, NumCols) ; NumCols = 0),
    (NewY >= 0, NewY < NumRows, NewX >= 0, NewX < NumCols ->
        NewCursor = [NewY, NewX],
        NewState = State.put(cursor, NewCursor)
    ;
        NewState = State
    ).

moveUp(State, NewState) :- updatePosition(State, -1, 0, NewState).

moveDown(State, NewState) :- updatePosition(State, 1, 0, NewState).

moveLeft(State, NewState) :- updatePosition(State, 0, -1, NewState).

moveRight(State, NewState) :- updatePosition(State, 0, 1, NewState).
