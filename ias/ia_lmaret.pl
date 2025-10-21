% ============================================
% IA AVEC HEURISTIQUES
% ============================================

% ============================================
% ADAPTER POUR FORMAT LIGNES
% ============================================

% Point d'entrée : adapter depuis format lignes vers format colonnes
% Board : Liste de 6 lignes (ligne 0=haut, ligne 5=bas), chaque ligne a 7 cellules (0=vide, 1='x', 2='o')
% Joueur : 'x' ou 'o'
% Col : Colonne jouée (1-7)
joue_coup(LinesBoard, Joueur, Col) :-
    % Convertir le board du format lignes au format colonnes
    convert_lines_to_columns(LinesBoard, ColumnsBoard),
    % Appeler l'IA heuristique
    ai_heur_play(ColumnsBoard, Col, Joueur),
    % Afficher la colonne jouée
    format('~w~n', [Col]).

% Convertir de format lignes (6 lignes × 7 colonnes) vers format colonnes (7 colonnes × 6 cellules)
convert_lines_to_columns(Lines, Columns) :-
    Lines = [L0, L1, L2, L3, L4, L5],
    % Extraire chaque colonne en parcourant les lignes de haut en bas
    extract_column(Lines, 1, C1),
    extract_column(Lines, 2, C2),
    extract_column(Lines, 3, C3),
    extract_column(Lines, 4, C4),
    extract_column(Lines, 5, C5),
    extract_column(Lines, 6, C6),
    extract_column(Lines, 7, C7),
    Columns = [C1, C2, C3, C4, C5, C6, C7].

% Extraire une colonne depuis les lignes (du haut vers le bas devient du bas vers le haut)
% Ligne 0 (haut) devient index 6 de la colonne, ligne 5 (bas) devient index 1
extract_column(Lines, ColIndex, Column) :-
    Lines = [L0, L1, L2, L3, L4, L5],
    nth1(ColIndex, L0, V0), convert_cell(V0, C6),
    nth1(ColIndex, L1, V1), convert_cell(V1, C5),
    nth1(ColIndex, L2, V2), convert_cell(V2, C4),
    nth1(ColIndex, L3, V3), convert_cell(V3, C3),
    nth1(ColIndex, L4, V4), convert_cell(V4, C2),
    nth1(ColIndex, L5, V5), convert_cell(V5, C1),
    Column = [C1, C2, C3, C4, C5, C6].

% Convertir une cellule du format entier au format symbole
convert_cell(0, _).  % 0 = cellule vide = variable libre
convert_cell(1, x).  % 1 = joueur x
convert_cell(2, o).  % 2 = joueur o

% IA intelligente pour tous les joueurs
ai_heur_play(Board, MoveCol, Player) :-
    ai_heur_bestMove(Board, Player, MoveCol),
    !.

% Trouve le meilleur coup selon l'ordre de priorité
ai_heur_bestMove(Board, Player, Move) :-
    (   ai_heur_winningMove(Board, Player, Move)
    ;   ai_heur_blockingMove(Board, Player, Move)
    ;   ai_heur_threatMove(Board, Player, Move)
    ;   ai_heur_blockThreatMove(Board, Player, Move)
    ;   ai_heur_centerMove(Board, Move)
    ;   ai_heur_randomValidMove(Board, Move)
    ),
    % Vérifier que ce coup ne donne pas la victoire à l'adversaire
    \+ ai_heur_givesOpponentWin(Board, Player, Move),
    !.

% Si tous les coups donnent la victoire à l'adversaire, on est obligé d'en jouer un
ai_heur_bestMove(Board, _Player, Move) :-
    ai_heur_validMoves(Board, ValidCols),
    member(Move, ValidCols),
    !.

% ============================================
% STRATÉGIES TACTIQUES
% ============================================

% 1. Coup gagnant : Trouver un coup qui permet de gagner immédiatement
ai_heur_winningMove(Board, Player, Col) :-
    ai_heur_validMoves(Board, ValidCols),
    member(Col, ValidCols),
    ai_heur_testMove(Board, Col, Player, TestBoard),
    ai_heur_checkWinner(TestBoard, Player, Col).

% 2. Coup bloquant : Bloquer la victoire adverse
ai_heur_blockingMove(Board, Player, Col) :-
    ai_heur_opponent(Player, Opp),
    ai_heur_winningMove(Board, Opp, Col).

% 3. Créer une menace : Faire un alignement de 3
ai_heur_threatMove(Board, Player, Col) :-
    ai_heur_validMoves(Board, ValidCols),
    member(Col, ValidCols),
    ai_heur_testMove(Board, Col, Player, TestBoard),
    ai_heur_hasThreeInRow(TestBoard, Player, Col).

% 4. Bloquer une menace adverse
ai_heur_blockThreatMove(Board, Player, Col) :-
    ai_heur_opponent(Player, Opp),
    ai_heur_threatMove(Board, Opp, Col).

% 5. Jouer au centre (colonnes 4, 3, 5 par ordre de préférence)
ai_heur_centerMove(Board, Col) :-
    member(Col, [4, 3, 5]),
    ai_heur_validMove(Board, Col).

% 6. Coup aléatoire parmi les coups valides
ai_heur_randomValidMove(Board, Col) :-
    ai_heur_validMoves(Board, ValidCols),
    ValidCols \= [],
    random_member(Col, ValidCols).

% ============================================
% VÉRIFICATION DES COUPS PERDANTS
% ============================================

% Vérifier si jouer en Col donne une victoire à l'adversaire au tour suivant
ai_heur_givesOpponentWin(Board, Player, Col) :-
    ai_heur_opponent(Player, Opp),
    % Simuler notre coup
    ai_heur_testMove(Board, Col, Player, BoardAfterOurMove),
    % Vérifier si l'adversaire peut gagner immédiatement après
    ai_heur_winningMove(BoardAfterOurMove, Opp, _).

% ============================================
% DÉTECTION DE MENACES (alignement de 3)
% ============================================

% Vérifier si un joueur a un alignement de 3 après avoir joué en Col
ai_heur_hasThreeInRow(Board, Player, Col) :-
    % Récupérer la ligne où le pion a été placé
    nth1(Col, Board, Column),
    findall(I, (nth1(I, Column, Cell), nonvar(Cell)), FilledRows),
    (FilledRows = [] -> fail ; max_list(FilledRows, Row)),

    % Vérifier dans toutes les directions
    (   ai_heur_hasThreeHorizontal(Board, Player, Row)
    ;   ai_heur_hasThreeVertical(Board, Player, Col)
    ;   ai_heur_hasThreeDiagonal(Board, Player, Col, Row)
    ).

% Vérifier alignement de 3 horizontal
ai_heur_hasThreeHorizontal(Board, Player, Row) :-
    findall(Cell, (nth1(_, Board, Column), nth1(Row, Column, Cell)), Line),
    ai_heur_hasThreeInSequence(Line, Player).

% Vérifier alignement de 3 vertical
ai_heur_hasThreeVertical(Board, Player, Col) :-
    nth1(Col, Board, Column),
    ai_heur_hasThreeInSequence(Column, Player).

% Vérifier alignement de 3 diagonal
ai_heur_hasThreeDiagonal(Board, Player, Col, Row) :-
    (   ai_heur_getDiagonal(Board, Col, Row, 1, 1, Diag1),
        ai_heur_hasThreeInSequence(Diag1, Player)
    ;   ai_heur_getDiagonal(Board, Col, Row, 1, -1, Diag2),
        ai_heur_hasThreeInSequence(Diag2, Player)
    ).

% Extraire une diagonale autour d'une position
ai_heur_getDiagonal(Board, Col, Row, DCol, DRow, Diagonal) :-
    findall(Cell, (
        between(-3, 3, Offset),
        C is Col + Offset * DCol,
        R is Row + Offset * DRow,
        C >= 1, C =< 7,
        R >= 1, R =< 6,
        nth1(C, Board, Column),
        nth1(R, Column, Cell)
    ), Diagonal).

% Chercher 3 pions consécutifs dans une séquence
ai_heur_hasThreeInSequence([A,B,C|_], Player) :-
    nonvar(A), nonvar(B), nonvar(C),
    A == Player, B == Player, C == Player,
    !.
ai_heur_hasThreeInSequence([_|T], Player) :-
    T = [_,_|_],  % Optimisation: au moins 3 éléments restants
    ai_heur_hasThreeInSequence(T, Player).

% ============================================
% HELPERS
% ============================================

% Obtenir tous les coups valides
ai_heur_validMoves(Board, ValidCols) :-
    findall(C, (between(1, 7, C), ai_heur_validMove(Board, C)), ValidCols).

% Vérifier si un coup est valide
ai_heur_validMove(Board, Col) :-
    integer(Col),
    Col >= 1, Col =< 7,
    nth1(Col, Board, Column),
    \+ ai_heur_full(Column).

% Vérifier si une colonne est pleine
ai_heur_full(Column) :-
    \+ (member(Cell, Column), var(Cell)).

% Simuler un coup sans modifier le plateau réel
ai_heur_testMove(Board, Col, Player, NewBoard) :-
    nth1(Col, Board, Column),
    addToColumn(Column, Player, NewColumn),
    replaceColumn(Board, Col, NewColumn, NewBoard).

% Note: addToColumn/3 et replaceColumn/4 sont définis dans game_logic.pl

% Obtenir l'adversaire
ai_heur_opponent('x', 'o').
ai_heur_opponent('o', 'x').

% Vérifier victoire pour une simulation (sans dépendre de last_move)
ai_heur_checkWinner(Board, Player, Col) :-
    nth1(Col, Board, Column),
    findall(I, (nth1(I, Column, Cell), nonvar(Cell)), FilledRows),
    (FilledRows = [] -> fail ; max_list(FilledRows, Row)),

    % Extraire toutes les lignes à vérifier
    findall(Cell, (nth1(_, Board, C), nth1(Row, C, Cell)), HLine),
    nth1(Col, Board, VLine),
    ai_heur_getDiagonal(Board, Col, Row, 1, 1, Diag1),
    ai_heur_getDiagonal(Board, Col, Row, 1, -1, Diag2),

    % Vérifier si une ligne contient 4 alignés
    (   ai_heur_hasFourInSequence(HLine, Player)
    ;   ai_heur_hasFourInSequence(VLine, Player)
    ;   ai_heur_hasFourInSequence(Diag1, Player)
    ;   ai_heur_hasFourInSequence(Diag2, Player)
    ).

% Chercher 4 pions consécutifs dans une séquence
ai_heur_hasFourInSequence([A,B,C,D|_], Player) :-
    nonvar(A), nonvar(B), nonvar(C), nonvar(D),
    A == Player, B == Player, C == Player, D == Player,
    !.
ai_heur_hasFourInSequence([_|T], Player) :-
    T = [_,_,_|_],  % Au moins 4 éléments restants
    ai_heur_hasFourInSequence(T, Player).
