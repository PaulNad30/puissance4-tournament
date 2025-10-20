% Puissance 4 (Connect Four) en Prolog
% Le plateau est représenté par une liste de 42 éléments (7 colonnes x 6 lignes)
% Les indices vont de 0 à 41, organisés ligne par ligne de bas en haut:

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% INTERFACE POUR LE TOURNOI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% PRÉDICAT PRINCIPAL POUR LE TOURNOI
joue_coup(BoardTournoi, Joueur, Colonne) :-
    convert_tournament_to_internal(BoardTournoi, Joueur, Board1D, Player),
    select_ia_for_tournament(Board1D, Colonne, Player).

%%%% Sélection de l'IA pour le tournoi
select_ia_for_tournament(Board, Column, Player) :-
    %ia_minimax(Board, Column, Player).
    ia_expert_fast(Board, Column, Player).  % Plus fort mais 5-20 sec/coup
    % ia_mcts(Board, Column, Player).         % Moyen, 2-3 sec/coup
    % ia_random(Board, Column, Player).       % Très rapide mais faible

convert_tournament_to_internal(Board2D, Joueur, Board1D, Player) :-
    reverse(Board2D, ReversedBoard2D),
    flatten(ReversedBoard2D, FlatBoardReversed),
    convert_tournament_values(FlatBoardReversed, Board1D),
    (Joueur = 1 -> Player = 'x' ; Player = 'o').

%%%% Convertir les valeurs du tournoi (0/1/2) vers notre format (_/'x'/'o')
convert_tournament_values([], []).
convert_tournament_values([H|T], [H2|T2]) :-
    (H = 0 -> true           % 0 → variable non instanciée (_)
    ; H = 1 -> H2 = 'x'      % 1 → 'x'
    ; H = 2 -> H2 = 'o'      % 2 → 'o'
    ),
    convert_tournament_values(T, T2).

% ========== 5 INTELLIGENCES ARTIFICIELLES DISPONIBLES ==========
% 1. ia_random      : Joue aléatoirement (avec tactiques de base)
% 2. ia_minimax     : Minimax avec Alpha-Beta (profondeur 6) - ~1 seconde/coup
% 3. ia_mcts        : Monte Carlo Tree Search (1000 simulations) - ~2-3 secondes/coup
% 4. ia_expert_fast : IA Expert rapide (profondeur 5-7) - ~5-10 secondes/coup ⭐ PAR DÉFAUT
% 5. ia_expert      : IA Expert complète (profondeur 8-10) - ~30 sec-2 min/coup ⚠️ TRÈS LENT

% STRATÉGIES TACTIQUES IMPLÉMENTÉES pour toutes les IA :
% 1. Victoire immédiate : Gagner si possible
% 2. Blocage défensif : Bloquer une victoire adverse
% 3. FOURCHETTE : Créer 2+ menaces simultanées
% 4. Blocage de fourchette : Empêcher l'adversaire de créer une fourchette
% 5. Filtrage des coups suicidaires : Ne jamais offrir la victoire
% 6. Évaluation heuristique : Patterns, centre, menaces, connectivité

:- dynamic board/1.
:- dynamic nodes_explored/1.

%%%% Test si le jeu est terminé %%%
gameover(Winner) :- board(Board), winner(Board, Winner), !.
gameover('Draw') :- board(Board), isBoardFull(Board).

%%%% Test si un plateau a une configuration gagnante pour le joueur P
% Lignes horizontales (4 consécutifs sur une même ligne)
winner(Board, P) :- 
    checkHorizontal(Board, P).

% Colonnes verticales (4 consécutifs dans une même colonne)
winner(Board, P) :- 
    checkVertical(Board, P).

% Diagonales ascendantes (/)
winner(Board, P) :- 
    checkDiagonalAsc(Board, P).

% Diagonales descendantes (\)
winner(Board, P) :- 
    checkDiagonalDesc(Board, P).

% Vérification horizontale
checkHorizontal(Board, P) :-
    member(Start, [0,7,14,21,28,35]),
    member(Offset, [0,1,2,3]),
    Index is Start + Offset,
    nth0(Index, Board, P), nonvar(P),
    I1 is Index + 1, nth0(I1, Board, Q1), P == Q1,
    I2 is Index + 2, nth0(I2, Board, Q2), P == Q2,
    I3 is Index + 3, nth0(I3, Board, Q3), P == Q3.

% Vérification verticale
checkVertical(Board, P) :-
    member(Col, [0,1,2,3,4,5,6]),
    member(Row, [0,1,2]),
    Index is Row * 7 + Col,
    nth0(Index, Board, P), nonvar(P),
    I1 is Index + 7, nth0(I1, Board, Q1), P == Q1,
    I2 is Index + 14, nth0(I2, Board, Q2), P == Q2,
    I3 is Index + 21, nth0(I3, Board, Q3), P == Q3.

% Vérification diagonale ascendante
checkDiagonalAsc(Board, P) :-
    member(Row, [0,1,2]),
    member(Col, [0,1,2,3]),
    Index is Row * 7 + Col,
    nth0(Index, Board, P), nonvar(P),
    I1 is Index + 8, nth0(I1, Board, Q1), P == Q1,
    I2 is Index + 16, nth0(I2, Board, Q2), P == Q2,
    I3 is Index + 24, nth0(I3, Board, Q3), P == Q3.

% Vérification diagonale descendante
checkDiagonalDesc(Board, P) :-
    member(Row, [0,1,2]),
    member(Col, [3,4,5,6]),
    Index is Row * 7 + Col,
    nth0(Index, Board, P), nonvar(P),
    I1 is Index + 6, nth0(I1, Board, Q1), P == Q1,
    I2 is Index + 12, nth0(I2, Board, Q2), P == Q2,
    I3 is Index + 18, nth0(I3, Board, Q3), P == Q3.

%%%% Vérifie si toutes les cases sont remplies
isBoardFull([]).
isBoardFull([H|T]) :- nonvar(H), isBoardFull(T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% IA Random
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ia_random(Board, Column, Player) :- 
    % 1. Coup gagnant ?
    (findWinningMove(Board, Player, WinMove) ->
        Column = WinMove
    % 2. Bloquer l'adversaire ?
    ; changePlayer(Player, Opponent), findWinningMove(Board, Opponent, BlockMove) ->
        Column = BlockMove
    % 3. Créer une fourchette (double menace) ?
    ; findForkMove(Board, Player, ForkMove) ->
        Column = ForkMove
    % 4. Bloquer une fourchette adverse ?
    ; changePlayer(Player, Opponent), findForkMove(Board, Opponent, BlockFork) ->
        Column = BlockFork
    % 5. Sinon jouer au hasard parmi les coups sûrs
    ;
        getSafeMoves(Board, Player, SafeMoves),
        random_member(Column, SafeMoves)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% IA Minimax Alpha-Beta
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ia_minimax(Board, Column, Player) :-
    MaxDepth = 6,
    % 1. Coup gagnant immédiat ?
    (findWinningMove(Board, Player, WinMove) ->
        Column = WinMove
    % 2. Bloquer une victoire adverse ?
    ; changePlayer(Player, Opponent), findWinningMove(Board, Opponent, BlockMove) ->
        Column = BlockMove
    % 3. Créer une fourchette (double menace) ?
    ; findForkMove(Board, Player, ForkMove) ->
        Column = ForkMove
    % 4. Bloquer une fourchette adverse ?
    ; changePlayer(Player, Opponent), findForkMove(Board, Opponent, BlockFork) ->
        Column = BlockFork
    % 5. Sinon évaluer normalement avec Minimax en filtrant les coups dangereux
    ;
        getSafeMoves(Board, Player, SafeMoves),
        evaluateAllMoves(SafeMoves, Board, Player, MaxDepth, ScoredMoves),
        findBestScore(ScoredMoves, BestScore),
        findall(Col, member(Col-BestScore, ScoredMoves), BestMoves),
        random_member(Column, BestMoves)
    ).

%%%% Trouve un coup gagnant immédiat s'il existe
findWinningMove(Board, Player, Column) :-
    allPossibleColumns(Board, Moves),
    member(Column, Moves),
    simulateMove(Board, Column, Player, NewBoard),
    winner(NewBoard, Player), !.

%%%% Compte le nombre de coups gagnants disponibles sur un plateau
countWinningMoves(Board, Player, Count) :-
    allPossibleColumns(Board, Moves),
    findall(1, (
        member(Move, Moves),
        simulateMove(Board, Move, Player, NewBoard),
        winner(NewBoard, Player)
    ), WinningMoves),
    length(WinningMoves, Count).

%%%% Détecte si un coup crée une double menace (fourchette)
createsFork(Board, Column, Player) :-
    simulateMove(Board, Column, Player, NewBoard),
    countWinningMoves(NewBoard, Player, Count),
    Count >= 2.

%%%% Trouve un coup qui crée une fourchette
findForkMove(Board, Player, Column) :-
    getSafeMoves(Board, Player, Moves),
    member(Column, Moves),
    createsFork(Board, Column, Player), !.

%%%% Vérifie si un coup offre la victoire à l'adversaire
isLosingMove(Board, Column, Player) :-
    simulateMove(Board, Column, Player, NewBoard),
    changePlayer(Player, Opponent),
    findWinningMove(NewBoard, Opponent, _).

%%%% Filtre les coups qui offrent la victoire à l'adversaire
filterSafeMoves([], _, []).
filterSafeMoves([Move|Moves], Board-Player, SafeMoves) :-
    (isLosingMove(Board, Move, Player) ->
        filterSafeMoves(Moves, Board-Player, SafeMoves)
    ;
        filterSafeMoves(Moves, Board-Player, RestSafe),
        SafeMoves = [Move|RestSafe]
    ).

%%%% Obtient les coups sûrs (qui ne donnent pas la victoire à l'adversaire)
getSafeMoves(Board, Player, SafeMoves) :-
    allPossibleColumns(Board, AllMoves),
    filterSafeMoves(AllMoves, Board-Player, Filtered),
    % Si aucun coup n'est sûr, on doit quand même jouer
    (Filtered = [] -> SafeMoves = AllMoves ; SafeMoves = Filtered).

evaluateAllMoves([], _, _, _, []).
evaluateAllMoves([Move|Moves], Board, Player, Depth, [Move-Score|Rest]) :-
    simulateMove(Board, Move, Player, NewBoard),
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    minimax(NewBoard, Opponent, NewDepth, -10000, 10000, _, OpponentScore),
    Score is -OpponentScore,
    evaluateAllMoves(Moves, Board, Player, Depth, Rest).

findBestScore([_-Score|Rest], BestScore) :-
    findBestScoreHelper(Rest, Score, BestScore).

findBestScoreHelper([], Best, Best).
findBestScoreHelper([_-Score|Rest], CurrentBest, BestScore) :-
    (Score > CurrentBest ->
        findBestScoreHelper(Rest, Score, BestScore)
    ;
        findBestScoreHelper(Rest, CurrentBest, BestScore)
    ).

minimax(Board, Player, Depth, Alpha, Beta, BestColumn, BestScore) :-
    allPossibleColumns(Board, Moves),
    Moves \= [],
    alphabeta(Moves, Board, Player, Depth, Alpha, Beta, nil, -10000, BestColumn, BestScore).

alphabeta([], _, _, _, _, _, BestColumn, BestScore, BestColumn, BestScore) :- !.

alphabeta([Move|Moves], Board, Player, Depth, Alpha, Beta, TempBest, TempScore, BestColumn, BestScore) :-
    simulateMove(Board, Move, Player, NewBoard),
    (   (Depth =< 0 ; winner(NewBoard, _) ; isBoardFull(NewBoard)) ->
        evaluate(NewBoard, Player, Score)
    ;
        changePlayer(Player, Opponent),
        NewDepth is Depth - 1,
        NegBeta is -Beta,
        NegAlpha is -Alpha,
        minimax(NewBoard, Opponent, NewDepth, NegBeta, NegAlpha, _, OpponentScore),
        Score is -OpponentScore
    ),
    (   Score > TempScore ->
        NewBest = Move,
        NewScore = Score
    ;
        NewBest = TempBest,
        NewScore = TempScore
    ),
    NewAlpha is max(Alpha, NewScore),
    (   NewAlpha >= Beta ->
        BestColumn = NewBest,
        BestScore = NewScore
    ;
        alphabeta(Moves, Board, Player, Depth, NewAlpha, Beta, NewBest, NewScore, BestColumn, BestScore)
    ).

%%%% Simule un coup sans modifier le plateau original
simulateMove(Board, Column, Player, NewBoard) :-
    findLowestRow(Board, Column, Row),
    Index is Row * 7 + Column,
    copy_term(Board, NewBoard),
    nth0(Index, NewBoard, Player).

%%%% Fonction d'évaluation du plateau
evaluate(Board, Player, Score) :-
    (winner(Board, Player) ->
        Score = 1000
    ; changePlayer(Player, Opponent), winner(Board, Opponent) ->
        Score = -1000
    % Si l'adversaire peut gagner au prochain coup = très mauvais
    ; changePlayer(Player, Opponent), canWinNextMove(Board, Opponent) ->
        Score = -950
    % Si on peut créer une fourchette = excellent
    ; allPossibleColumns(Board, Moves), member(Move, Moves), createsFork(Board, Move, Player) ->
        Score = 850
    % Si l'adversaire peut créer une fourchette = très mauvais
    ; changePlayer(Player, Opponent), 
      allPossibleColumns(Board, Moves), member(Move, Moves), createsFork(Board, Move, Opponent) ->
        Score = -800
    % Vérifier si on peut gagner au prochain coup
    ; canWinNextMove(Board, Player) ->
        Score = 900
    ;
        evaluatePosition(Board, Player, Score)
    ).

%%%% Vérifie si un joueur peut gagner au prochain coup
canWinNextMove(Board, Player) :-
    allPossibleColumns(Board, Moves),
    member(Move, Moves),
    simulateMove(Board, Move, Player, NewBoard),
    winner(NewBoard, Player), !.

%%%% Évaluation heuristique de la position
evaluatePosition(Board, Player, Score) :-
    changePlayer(Player, Opponent),
    countPatterns(Board, Player, PlayerScore),
    countPatterns(Board, Opponent, OpponentScore),
    centerControl(Board, Player, CenterBonus),
    % Détecter les menaces et opportunités
    countThreats(Board, Player, PlayerThreats),
    countThreats(Board, Opponent, OpponentThreats),
    Score is PlayerScore - OpponentScore + CenterBonus + PlayerThreats * 100 - OpponentThreats * 90.

%%%% Bonus pour le contrôle des colonnes centrales
centerControl(Board, Player, Bonus) :-
    countPiecesInColumn(Board, Player, 3, Center),
    countPiecesInColumn(Board, Player, 2, Left),
    countPiecesInColumn(Board, Player, 4, Right),
    Bonus is Center * 3 + Left * 2 + Right * 2.

countPiecesInColumn(Board, Player, Col, Count) :-
    findall(1, (
        between(0, 5, Row),
        Index is Row * 7 + Col,
        nth0(Index, Board, Player)
    ), Pieces),
    length(Pieces, Count).

%%%% Compte le nombre de menaces (3 pions alignés avec case libre jouable)
countThreats(Board, Player, Count) :-
    findall(1, isThreat(Board, Player), Threats),
    length(Threats, Count).

%%%% Vérifie s'il existe une menace (3 pions + 1 case libre jouable)
isThreat(Board, Player) :-
    % Pour chaque colonne possible
    allPossibleColumns(Board, Moves),
    member(Col, Moves),
    % Simuler le coup
    findLowestRow(Board, Col, Row),
    Index is Row * 7 + Col,
    % Vérifier si ce coup complète un alignement de 3
    checkThreatAt(Board, Index, Player).

%%%% Vérifie si placer un pion à Index complète un alignement de 3
checkThreatAt(Board, Index, Player) :-
    % Copier le board et placer le pion
    copy_term(Board, TestBoard),
    nth0(Index, TestBoard, Player),
    % Vérifier s'il y a maintenant 4 alignés
    winner(TestBoard, Player).

%%%% Compte les patterns (alignements de 2, 3 pions avec espaces libres)
countPatterns(Board, Player, TotalScore) :-
    countHorizontalPatterns(Board, Player, H),
    countVerticalPatterns(Board, Player, V),
    countDiagonalPatterns(Board, Player, D),
    TotalScore is H + V + D.

countHorizontalPatterns(Board, Player, Score) :-
    findall(S, (
        member(Row, [0,1,2,3,4,5]),
        member(Col, [0,1,2,3]),
        Index is Row * 7 + Col,
        checkWindowScore(Board, Index, 1, Player, S)
    ), Scores),
    sumlist(Scores, Score).

countVerticalPatterns(Board, Player, Score) :-
    findall(S, (
        member(Col, [0,1,2,3,4,5,6]),
        member(Row, [0,1,2]),
        Index is Row * 7 + Col,
        checkWindowScore(Board, Index, 7, Player, S)
    ), Scores),
    sumlist(Scores, Score).

countDiagonalPatterns(Board, Player, Score) :-
    findall(S, (
        (   (member(Row, [0,1,2]), member(Col, [0,1,2,3]),
             Index is Row * 7 + Col, Delta = 8,
             checkWindowScore(Board, Index, Delta, Player, S))
        ;   (member(Row, [0,1,2]), member(Col, [3,4,5,6]),
             Index is Row * 7 + Col, Delta = 6,
             checkWindowScore(Board, Index, Delta, Player, S))
        )
    ), Scores),
    sumlist(Scores, Score).

checkWindowScore(Board, Index, Delta, Player, Score) :-
    I2 is Index + Delta,
    I3 is Index + Delta * 2,
    I4 is Index + Delta * 3,
    (   (I2 < 42, I3 < 42, I4 < 42) ->
        (   nth0(Index, Board, V1),
            nth0(I2, Board, V2),
            nth0(I3, Board, V3),
            nth0(I4, Board, V4),
            countPieces([V1,V2,V3,V4], Player, Count),
            (   Count = 3 -> Score = 50  % Augmenté de 5 à 50
            ;   Count = 2 -> Score = 10  % Augmenté de 2 à 10
            ;   Score = 0
            )
        )
    ;   Score = 0
    ).

countPieces(List, Player, Count) :-
    changePlayer(Player, Opponent),
    (   member(Opponent, List) ->
        Count = 0
    ;
        include(==(Player), List, PlayerPieces),
        length(PlayerPieces, Count)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% IA MCTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ia_mcts(Board, Column, Player) :-
    % 1. Coup gagnant immédiat ?
    (findWinningMove(Board, Player, WinMove) ->
        Column = WinMove
    % 2. Bloquer une victoire adverse ?
    ; changePlayer(Player, Opponent), findWinningMove(Board, Opponent, BlockMove) ->
        Column = BlockMove
    % 3. Créer une fourchette (double menace) ?
    ; findForkMove(Board, Player, ForkMove) ->
        Column = ForkMove
    % 4. Bloquer une fourchette adverse ?
    ; changePlayer(Player, Opponent), findForkMove(Board, Opponent, BlockFork) ->
        Column = BlockFork
    % 5. Sinon utiliser MCTS avec les coups sûrs
    ;
        NumSimulations = 1000,
        getSafeMoves(Board, Player, SafeMoves),
        mcts_safe(Board, Player, NumSimulations, SafeMoves, Column)
    ).

%%%% MCTS avec coups filtrés
mcts_safe(Board, Player, NumSimulations, Moves, BestColumn) :-
    initializeStats(Moves, Stats),
    runSimulations(Board, Player, Moves, Stats, NumSimulations, FinalStats),
    selectBestMoveRandom(FinalStats, BestColumn).

mcts(Board, Player, NumSimulations, BestColumn) :-
    allPossibleColumns(Board, Moves),
    initializeStats(Moves, Stats),
    runSimulations(Board, Player, Moves, Stats, NumSimulations, FinalStats),
    selectBestMoveRandom(FinalStats, BestColumn).

initializeStats([], []).
initializeStats([Move|Moves], [Move-0-0|Stats]) :-
    initializeStats(Moves, Stats).

runSimulations(_, _, _, Stats, 0, Stats) :- !.
runSimulations(Board, Player, Moves, Stats, N, FinalStats) :-
    N > 0,
    selectMoveUCB(Stats, N, SelectedMove),
    simulateGame(Board, Player, SelectedMove, Result),
    updateStats(Stats, SelectedMove, Result, Player, NewStats),
    N1 is N - 1,
    runSimulations(Board, Player, Moves, NewStats, N1, FinalStats).

selectMoveUCB(Stats, TotalPlays, SelectedMove) :-
    TotalPlaysFloat is float(TotalPlays),
    findBestUCB(Stats, TotalPlaysFloat, nil, -1000000, SelectedMove).

findBestUCB([], _, BestMove, _, BestMove) :- !.
findBestUCB([Move-Wins-Plays|Rest], TotalPlays, CurrentBest, CurrentBestScore, BestMove) :-
    (   Plays = 0 ->
        UCB = 1000000
    ;
        WinRate is Wins / Plays,
        Exploration is sqrt((2 * log(TotalPlays)) / Plays),
        UCB is WinRate + Exploration
    ),
    (   UCB > CurrentBestScore ->
        findBestUCB(Rest, TotalPlays, Move, UCB, BestMove)
    ;
        findBestUCB(Rest, TotalPlays, CurrentBest, CurrentBestScore, BestMove)
    ).

simulateGame(Board, Player, FirstMove, Result) :-
    simulateMove(Board, FirstMove, Player, NewBoard),
    changePlayer(Player, Opponent),
    playoutRandom(NewBoard, Opponent, Player, Result).

playoutRandom(Board, CurrentPlayer, OriginalPlayer, Result) :-
    (   winner(Board, Winner) ->
        (Winner = OriginalPlayer -> Result = win ; Result = loss)
    ;   isBoardFull(Board) ->
        Result = draw
    ;   allPossibleColumns(Board, Moves),
        random_member(Move, Moves),
        simulateMove(Board, Move, CurrentPlayer, NewBoard),
        changePlayer(CurrentPlayer, NextPlayer),
        playoutRandom(NewBoard, NextPlayer, OriginalPlayer, Result)
    ).

updateStats([], _, _, _, []).
updateStats([Move-Wins-Plays|Rest], SelectedMove, Result, Player, [Move-NewWins-NewPlays|NewRest]) :-
    (   Move = SelectedMove ->
        NewPlays is Plays + 1,
        (   Result = win -> NewWins is Wins + 1
        ;   Result = draw -> NewWins is Wins + 0.5
        ;   NewWins = Wins
        )
    ;
        NewWins = Wins,
        NewPlays = Plays
    ),
    updateStats(Rest, SelectedMove, Result, Player, NewRest).

selectBestMoveRandom(Stats, BestColumn) :-
    findBestWinRateValue(Stats, BestRate),
    findall(Move, (member(Move-Wins-Plays, Stats), Plays > 0, Wins / Plays >= BestRate - 0.01), BestMoves),
    (BestMoves = [] -> random_member(BestColumn, [0,1,2,3,4,5,6]) ; random_member(BestColumn, BestMoves)).

findBestWinRateValue(Stats, BestRate) :-
    findall(Rate, (member(_-Wins-Plays, Stats), Plays > 0, Rate is Wins / Plays), Rates),
    (Rates = [] -> BestRate = 0 ; max_list(Rates, BestRate)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% IA EXPERT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ia_expert(Board, Column, Player) :-
    (findWinningMove(Board, Player, WinMove) ->
        Column = WinMove
    ; changePlayer(Player, Opponent), findWinningMove(Board, Opponent, BlockMove) ->
        Column = BlockMove
    ; useOpeningBook(Board, Player, OpeningMove) ->
        Column = OpeningMove
    ; findForkMove(Board, Player, ForkMove) ->
        Column = ForkMove
    ; changePlayer(Player, Opponent), findForkMove(Board, Opponent, BlockFork) ->
        Column = BlockFork
    ;
        expertSearch(Board, Player, Column)
    ).

useOpeningBook(Board, Player, Column) :-
    countTotalMoves(Board, TotalMoves),
    TotalMoves < 4,
    openingMove(Board, Player, Column),
    canPlayInColumn(Board, Column).

openingMove(Board, 'x', 3) :- countTotalMoves(Board, 0).
openingMove(Board, 'o', 3) :- countTotalMoves(Board, 1), canPlayInColumn(Board, 3).
openingMove(Board, 'o', 2) :- countTotalMoves(Board, 1).
openingMove(Board, 'x', 4) :- countTotalMoves(Board, 2), nth0(3, Board, 'x'), canPlayInColumn(Board, 4).
openingMove(Board, 'x', 2) :- countTotalMoves(Board, 2), canPlayInColumn(Board, 2).

countTotalMoves(Board, Count) :-
    findall(1, (member(Piece, Board), nonvar(Piece)), Pieces),
    length(Pieces, Count).

expertSearch(Board, Player, Column) :-
    countTotalMoves(Board, TotalMoves),
    (TotalMoves < 10 -> Depth = 8 ;
     TotalMoves < 25 -> Depth = 9 ;
     Depth = 10),
    getNonPoisonedMoves(Board, Player, SafeMoves),
    evaluateMovesExpert(SafeMoves, Board, Player, Depth, ScoredMoves),
    findBestScore(ScoredMoves, BestScore),
    findall(Col, member(Col-BestScore, ScoredMoves), BestMoves),
    (member(3, BestMoves) -> Column = 3 ;
     member(2, BestMoves) -> Column = 2 ;
     member(4, BestMoves) -> Column = 4 ;
     random_member(Column, BestMoves)).

%%%% Évalue les coups avec la version expert (sans affichage)
evaluateMovesExpert([], _, _, _, []).
evaluateMovesExpert([Move|Moves], Board, Player, Depth, [Move-Score|Rest]) :-
    simulateMove(Board, Move, Player, NewBoard),
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    retractall(nodes_explored(_)),
    assert(nodes_explored(0)),
    minimaxExpertWithProgress(NewBoard, Opponent, NewDepth, -10000, 10000, _, OpponentScore),
    Score is -OpponentScore,
    evaluateMovesExpert(Moves, Board, Player, Depth, Rest).

%%%% Minimax avec compteur de progression (sans affichage)
minimaxExpertWithProgress(Board, Player, Depth, Alpha, Beta, BestColumn, BestScore) :-
    allPossibleColumns(Board, Moves),
    Moves \= [],
    alphabetaExpertWithProgress(Moves, Board, Player, Depth, Alpha, Beta, nil, -10000, BestColumn, BestScore).

alphabetaExpertWithProgress([], _, _, _, _, _, BestColumn, BestScore, BestColumn, BestScore) :- !.
alphabetaExpertWithProgress([Move|Moves], Board, Player, Depth, Alpha, Beta, TempBest, TempScore, BestColumn, BestScore) :-
    % Incrémenter le compteur (sans affichage)
    nodes_explored(N),
    N1 is N + 1,
    retract(nodes_explored(N)),
    assert(nodes_explored(N1)),
    
    simulateMove(Board, Move, Player, NewBoard),
    (   (Depth =< 0 ; winner(NewBoard, _) ; isBoardFull(NewBoard)) ->
        evaluateExpert(NewBoard, Player, Score)
    ;
        changePlayer(Player, Opponent),
        NewDepth is Depth - 1,
        NegBeta is -Beta,
        NegAlpha is -Alpha,
        minimaxExpertWithProgress(NewBoard, Opponent, NewDepth, NegBeta, NegAlpha, _, OpponentScore),
        Score is -OpponentScore
    ),
    (   Score > TempScore ->
        NewBest = Move,
        NewScore = Score
    ;
        NewBest = TempBest,
        NewScore = TempScore
    ),
    NewAlpha is max(Alpha, NewScore),
    (   NewAlpha >= Beta ->
        BestColumn = NewBest,
        BestScore = NewScore
    ;
        alphabetaExpertWithProgress(Moves, Board, Player, Depth, NewAlpha, Beta, NewBest, NewScore, BestColumn, BestScore)
    ).

getNonPoisonedMoves(Board, Player, NonPoisoned) :-
    getSafeMoves(Board, Player, SafeMoves),
    changePlayer(Player, Opponent),
    filterPoisonedMoves(SafeMoves, Board, Player, Opponent, Filtered),
    (Filtered = [] -> NonPoisoned = SafeMoves ; NonPoisoned = Filtered).

filterPoisonedMoves([], _, _, _, []).
filterPoisonedMoves([Move|Moves], Board, Player, Opponent, Result) :-
    (isPoisonedMove(Board, Move, Player, Opponent) ->
        filterPoisonedMoves(Moves, Board, Player, Opponent, Result)
    ;
        filterPoisonedMoves(Moves, Board, Player, Opponent, Rest),
        Result = [Move|Rest]
    ).

isPoisonedMove(Board, Column, Player, Opponent) :-
    simulateMove(Board, Column, Player, NewBoard),
    getSafeMoves(NewBoard, Opponent, OpponentMoves),
    member(OpMove, OpponentMoves),
    createsFork(NewBoard, OpMove, Opponent).

:- discontiguous evaluateMovesExpert/5.
evaluateMovesExpert([], _, _, _, []).
evaluateMovesExpert([Move|Moves], Board, Player, Depth, [Move-Score|Rest]) :-
    simulateMove(Board, Move, Player, NewBoard),
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    minimaxExpert(NewBoard, Opponent, NewDepth, -10000, 10000, _, OpponentScore),
    Score is -OpponentScore,
    evaluateMovesExpert(Moves, Board, Player, Depth, Rest).

minimaxExpert(Board, Player, Depth, Alpha, Beta, BestColumn, BestScore) :-
    allPossibleColumns(Board, Moves),
    Moves \= [],
    alphabetaExpert(Moves, Board, Player, Depth, Alpha, Beta, nil, -10000, BestColumn, BestScore).

alphabetaExpert([], _, _, _, _, _, BestColumn, BestScore, BestColumn, BestScore) :- !.
alphabetaExpert([Move|Moves], Board, Player, Depth, Alpha, Beta, TempBest, TempScore, BestColumn, BestScore) :-
    simulateMove(Board, Move, Player, NewBoard),
    (   (Depth =< 0 ; winner(NewBoard, _) ; isBoardFull(NewBoard)) ->
        evaluateExpert(NewBoard, Player, Score)
    ;
        changePlayer(Player, Opponent),
        NewDepth is Depth - 1,
        NegBeta is -Beta,
        NegAlpha is -Alpha,
        minimaxExpert(NewBoard, Opponent, NewDepth, NegBeta, NegAlpha, _, OpponentScore),
        Score is -OpponentScore
    ),
    (   Score > TempScore ->
        NewBest = Move,
        NewScore = Score
    ;
        NewBest = TempBest,
        NewScore = TempScore
    ),
    NewAlpha is max(Alpha, NewScore),
    (   NewAlpha >= Beta ->
        BestColumn = NewBest,
        BestScore = NewScore
    ;
        alphabetaExpert(Moves, Board, Player, Depth, NewAlpha, Beta, NewBest, NewScore, BestColumn, BestScore)
    ).

evaluateExpert(Board, Player, Score) :-
    (winner(Board, Player) ->
        Score = 10000
    ; changePlayer(Player, Opponent), winner(Board, Opponent) ->
        Score = -10000
    ; changePlayer(Player, Opponent), canWinNextMove(Board, Opponent) ->
        Score = -9500
    ; canWinNextMove(Board, Player) ->
        Score = 9000
    ; allPossibleColumns(Board, Moves), member(Move, Moves), createsFork(Board, Move, Player) ->
        Score = 8500
    ; changePlayer(Player, Opponent), 
      allPossibleColumns(Board, Moves), member(Move, Moves), createsFork(Board, Move, Opponent) ->
        Score = -8000
    ;
        evaluatePositionExpert(Board, Player, Score)
    ).

evaluatePositionExpert(Board, Player, Score) :-
    changePlayer(Player, Opponent),
    countPatterns(Board, Player, PlayerPatterns),
    countPatterns(Board, Opponent, OpponentPatterns),
    centerControl(Board, Player, PlayerCenter),
    centerControl(Board, Opponent, OpponentCenter),
    countThreats(Board, Player, PlayerThreats),
    countThreats(Board, Opponent, OpponentThreats),
    evaluateConnectivity(Board, Player, PlayerConn),
    evaluateConnectivity(Board, Opponent, OpponentConn),
    evaluateWinningStructures(Board, Player, PlayerStruct),
    evaluateWinningStructures(Board, Opponent, OpponentStruct),
    evaluateColumnControl(Board, Player, PlayerColControl),
    evaluateColumnControl(Board, Opponent, OpponentColControl),
    Score is PlayerPatterns - OpponentPatterns 
           + (PlayerCenter - OpponentCenter) * 2
           + PlayerThreats * 150 - OpponentThreats * 140
           + PlayerConn * 3 - OpponentConn * 3
           + PlayerStruct * 80 - OpponentStruct * 80
           + PlayerColControl * 20 - OpponentColControl * 20.

evaluateConnectivity(Board, Player, Score) :-
    findall(Bonus, (
        between(0, 41, Index),
        nth0(Index, Board, Player),
        countAdjacentPieces(Board, Index, Player, Adjacent),
        Bonus = Adjacent
    ), Bonuses),
    sumlist(Bonuses, Score).

countAdjacentPieces(Board, Index, Player, Count) :-
    Row is Index // 7,
    Col is Index mod 7,
    findall(1, (
        member([DR, DC], [[-1,0], [1,0], [0,-1], [0,1], [-1,-1], [-1,1], [1,-1], [1,1]]),
        NRow is Row + DR,
        NCol is Col + DC,
        NRow >= 0, NRow < 6, NCol >= 0, NCol < 7,
        NIndex is NRow * 7 + NCol,
        nth0(NIndex, Board, Player)
    ), Adjacent),
    length(Adjacent, Count).

evaluateWinningStructures(Board, Player, Score) :-
    countSevenShapes(Board, Player, Sevens),
    countDoubleDiagonals(Board, Player, Diagonals),
    Score is Sevens + Diagonals.

countSevenShapes(Board, Player, Count) :-
    findall(1, hasSevenShape(Board, Player), Shapes),
    length(Shapes, Count).

hasSevenShape(Board, Player) :-
    between(0, 34, Index),
    Row is Index // 7,
    Col is Index mod 7,
    Col =< 3,
    Row =< 3,
    nth0(Index, Board, Player),
    I1 is Index + 7, nth0(I1, Board, Player),
    I2 is Index + 1, nth0(I2, Board, Player).

countDoubleDiagonals(Board, Player, Count) :-
    findall(1, hasDoubleDiagonal(Board, Player), Diags),
    length(Diags, Count).

hasDoubleDiagonal(Board, Player) :-
    between(14, 20, Index),
    nth0(Index, Board, Player),
    I1 is Index + 8, I2 is Index - 8,
    I3 is Index + 6, I4 is Index - 6,
    (nth0(I1, Board, Player), nth0(I2, Board, Player) ;
     nth0(I3, Board, Player), nth0(I4, Board, Player)).

evaluateColumnControl(Board, Player, Score) :-
    countPiecesInColumn(Board, Player, 3, Center),
    countPiecesInColumn(Board, Player, 2, Left),
    countPiecesInColumn(Board, Player, 4, Right),
    Score is Center * 3 + Left * 2 + Right * 2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% IA EXPERT FAST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ia_expert_fast(Board, Column, Player) :-
    (findWinningMove(Board, Player, WinMove) ->
        Column = WinMove
    ; changePlayer(Player, Opponent), findWinningMove(Board, Opponent, BlockMove) ->
        Column = BlockMove
    ; useOpeningBook(Board, Player, OpeningMove) ->
        Column = OpeningMove
    ; findForkMove(Board, Player, ForkMove) ->
        Column = ForkMove
    ; changePlayer(Player, Opponent), findForkMove(Board, Opponent, BlockFork) ->
        Column = BlockFork
    ;
        expertSearchFast(Board, Player, Column)
    ).

%%%% Version rapide de expertSearch (profondeur réduite)
expertSearchFast(Board, Player, Column) :-
    countTotalMoves(Board, TotalMoves),
    % Profondeur réduite : 5-7 au lieu de 8-10
    (TotalMoves < 10 -> Depth = 7 ;
     TotalMoves < 25 -> Depth = 8 ;
     Depth = 9),
    getNonPoisonedMoves(Board, Player, SafeMoves),
    evaluateMovesExpertFast(SafeMoves, Board, Player, Depth, ScoredMoves),
    findBestScore(ScoredMoves, BestScore),
    findall(Col, member(Col-BestScore, ScoredMoves), BestMoves),
    (member(3, BestMoves) -> Column = 3 ;
     member(2, BestMoves) -> Column = 2 ;
     member(4, BestMoves) -> Column = 4 ;
     random_member(Column, BestMoves)).

%%%% Évalue les coups (version Fast sans affichage)
evaluateMovesExpertFast([], _, _, _, []).
evaluateMovesExpertFast([Move|Moves], Board, Player, Depth, [Move-Score|Rest]) :-
    retractall(nodes_explored(_)),
    assert(nodes_explored(0)),
    simulateMove(Board, Move, Player, NewBoard),
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    minimaxExpertWithProgress(NewBoard, Opponent, NewDepth, -10000, 10000, _, OpponentScore),
    Score is -OpponentScore,
    evaluateMovesExpertFast(Moves, Board, Player, Depth, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% CONVERSION DE FORMATS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Convertit un tableau 2D en tableau 1D
board2d_to_1d(Board2D, Board1D) :-
    flatten(Board2D, Board1D).

%%%% Convertit un tableau 1D en tableau 2D
board1d_to_2d(Board1D, Board2D) :-
    split_into_rows(Board1D, Board2D).

split_into_rows([], []).
split_into_rows(Board1D, [Row|RestRows]) :-
    length(Row, 7),
    append(Row, Rest, Board1D),
    split_into_rows(Rest, RestRows).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ADAPTATEURS POUR COMPATIBILITÉ ENTRE FORMATS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Convertir board avec variables (_) en board avec espaces (' ')
board_var_to_space([], []).
board_var_to_space([H|T], [H2|T2]) :-
    (var(H) -> H2 = ' ' ; H2 = H),
    board_var_to_space(T, T2).

%%%% Convertir board avec espaces (' ') en board avec variables (_)
board_space_to_var([], []).
board_space_to_var([H|T], [H2|T2]) :-
    (H == ' ' -> true ; H2 = H),
    board_space_to_var(T, T2).

%%%% Vérifie si on peut jouer dans une colonne
canPlayInColumn(Board, Column) :- 
    TopIndex is 35 + Column,
    nth0(TopIndex, Board, Val),
    var(Val).

%%%% Trouve la ligne la plus basse disponible dans une colonne
findLowestRow(Board, Column, Row) :- 
    findLowestRowHelper(Board, Column, 0, Row).

findLowestRowHelper(Board, Column, CurrentRow, Row) :-
    CurrentRow < 6,
    Index is CurrentRow * 7 + Column,
    nth0(Index, Board, Val),
    (var(Val) -> Row = CurrentRow ; 
     NextRow is CurrentRow + 1, 
     findLowestRowHelper(Board, Column, NextRow, Row)).

%%%% Change de joueur
changePlayer('x', 'o').
changePlayer('o', 'x').

%%%% Prédicats pour l'analyse des coups possibles
possibleColumn(Board, Col) :- 
    between(0, 6, Col),
    canPlayInColumn(Board, Col).

allPossibleColumns(Board, L) :- 
    findall(Col, possibleColumn(Board, Col), L).