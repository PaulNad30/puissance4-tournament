% ia_manchot.pl
:- module(ia_manchot, [joue_coup/3]).
:- use_module(engine).
:- use_module(library(random)).

% IA Bandit Manchot :
% Essaie 1000 coups aléatoires simulés et choisit celui qui maximise une heuristique simple :
% +10 pour un coup gagnant immédiat, -10 pour un coup donnant la victoire à l'adversaire, +1 sinon.

joue_coup(Board, Player, BestCol) :-
        valid_moves(Board, Moves),
        findall(Score-Col, (
        member(Col, Moves),
        estimate(Board, Player, Col, Score)
        ), Scores),
        keysort(Scores, Sorted),
        reverse(Sorted, [_-BestCol|_]).

estimate(Board, Player, Col, Score) :-
% Faire 1000 tirages aléatoires pour estimer la qualité du coup
findall(S, (between(1,1000,_), simulate(Board, Player, Col, S)), ListScores),
sum_list(ListScores, Total),
length(ListScores, L),
(L>0 -> Score is Total / L ; Score = 0).

simulate(Board, Player, Col, Score) :-
( apply_move(Board, Col, Player, NewBoard) ->
( winner(NewBoard, Player) -> Score = 10 ;
opponent(Player, Opp),
valid_moves(NewBoard, OppMoves),
random_member(OppCol, OppMoves),
( apply_move(NewBoard, OppCol, Opp, NextBoard), winner(NextBoard, Opp) -> Score = -10 ; Score = 1 )
)
; Score = -10 ).

opponent(1,2).
opponent(2,1).