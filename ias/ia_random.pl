 
% ia_random.pl
:- module(ia_random, [joue_coup/3]).
:- use_module(library(random)).

joue_coup(Board, _Player, Col) :-
    nth0(0, Board, TopRow),
    findall(C, (nth0(C, TopRow, V), V == 0), Cols),
    random_member(Col, Cols).