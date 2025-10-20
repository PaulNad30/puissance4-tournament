% =======================================
% Puissance 4 IA bitboards — tournoi
% Expose: joue_coup/3
% =======================================

:- dynamic tt/4.         % tt(Hash,Depth,Score,Flag)
:- dynamic ttm/2.        % ttm(Hash,BestMove)
:- dynamic killer/2.     % killer(Depth,Move)
:- dynamic win_masks/1.
:- dynamic colmask/2.

% ----- Paramètres -----
max_depth(9).                       % profondeur fixe par défaut
center_order([4,3,5,2,6,1,7]).      % ordonnancement centré
inf(1000000000).

% =======================================
% Entrée tournoi
% LinesBoard : 6 lignes x 7 colonnes, 0/1/2
% Joueur     : 1 ou 2
% Col        : 1..7
% =======================================
joue_coup(LinesBoard, Joueur, Col) :-
    convert_lines_to_bitboards(LinesBoard, MX, MO),
    player_symbol(Joueur, P),
    center_order(O),
    max_depth(D),
    inf(Inf),
    NA is -Inf, NB is Inf,
    negamax(MX,MO,P,D,NA,NB,O,_Val,Cand),
    % sécurité: s'assurer que le coup renvoyé est légal
    ( col_full(MX,MO,Cand) ->
        legal_moves(MX,MO,O,LM),
        ( LM = [Col|_] -> true ; Col = 4 )  % fallback
    ; Col = Cand ).

player_symbol(1,'x').
player_symbol(2,'o').

% =======================================
% Conversion Lines -> Bitboards
% =======================================
% bit_pos: Row 0..5 (haut->bas), Col 0..6 (g->d)
bit_pos(R,C,P) :- P is R*7 + C.

convert_lines_to_bitboards(Lines, MX, MO) :-
    % bits 'x'
    findall(Bx,
        ( between(0,5,R), R1 is R+1, nth1(R1, Lines, Row),
          between(0,6,C), C1 is C+1, nth1(C1, Row, V), V =:= 1,
          bit_pos(R,C,P), Bx is 1<<P ),
        Xbits),
    sum_bits(Xbits, MX),
    % bits 'o'
    findall(Bo,
        ( between(0,5,R), R1 is R+1, nth1(R1, Lines, Row),
          between(0,6,C), C1 is C+1, nth1(C1, Row, V), V =:= 2,
          bit_pos(R,C,P), Bo is 1<<P ),
        Obits),
    sum_bits(Obits, MO).

% =======================================
% Bitboards: coups, états, gagnants
% =======================================
occupied(MX,MO,Occ) :- Occ is MX \/ MO.

% bits d’une colonne C0 (0..6), bas->haut
col_bits(C0, Bits) :-
    findall(B, (between(0,5,R), bit_pos(R,C0,P), B is 1<<P), Bs),
    reverse(Bs, Bits).

% colonne pleine si case du haut occupée
col_full(MX,MO,Col) :-
    C0 is Col-1, bit_pos(0,C0,P), B is 1<<P,
    occupied(MX,MO,Occ), (Occ /\ B) =\= 0.

% coups légaux selon ordre donné
legal_moves(MX,MO,Order,Cols) :-
    findall(C, (member(C,Order), \+ col_full(MX,MO,C)), Cols).

% application d’un coup avec gravité
apply_move(MX,MO,Player,Col,MX2,MO2,LastBit) :-
    C0 is Col-1, col_bits(C0, Bits),
    occupied(MX,MO,Occ),
    member(LastBit, Bits), (Occ /\ LastBit) =:= 0, !,
    ( Player=='x' -> MX2 is MX \/ LastBit, MO2 = MO
    ;                MO2 is MO \/ LastBit, MX2 = MX ).

% --- Masques gagnants pré-calculés ---
:- initialization(init_win_masks).

init_win_masks :-
    findall(M, win_mask(M), Ms),
    retractall(win_masks(_)),
    assertz(win_masks(Ms)).

win_mask(M):-between(0,5,R),between(0,3,C),seq_mask(R,C,0,1,M).  % H
win_mask(M):-between(0,2,R),between(0,6,C),seq_mask(R,C,1,0,M).  % V
win_mask(M):-between(0,2,R),between(0,3,C),seq_mask(R,C,1,1,M).  % /
win_mask(M):-between(3,5,R),between(0,3,C),seq_mask(R,C,-1,1,M). % \

seq_mask(R,C,DR,DC,M):-
    bit_pos(R,C,P0),
    R1 is R+DR, C1 is C+DC, bit_pos(R1,C1,P1),
    R2 is R1+DR, C2 is C1+DC, bit_pos(R2,C2,P2),
    R3 is R2+DR, C3 is C2+DC, bit_pos(R3,C3,P3),
    M is (1<<P0) \/ (1<<P1) \/ (1<<P2) \/ (1<<P3).

has_won(M) :- win_masks(Ms), member(W,Ms), (M /\ W) =:= W, !.

full_board(MX,MO) :-
    occupied(MX,MO,Occ), (1<<42)-1 =:= Occ.

% =======================================
% Évaluation & terminaux
% =======================================
terminal_score(MX,MO,Player,Score) :-
    ( has_won(MX) -> W = x
    ; has_won(MO) -> W = o
    ; W = none ),
    ( W == none ->
        ( full_board(MX,MO) -> Score = 0 ; fail )
    ; ( Player == W -> Score = 1000000 ; Score = -1000000 )
    ).

heuristic(MX,MO,_P,Score) :-
    center_bonus(MX,MO,CB),
    three_two_bonus(MX,MO,T),
    Score is CB + T.

% cache masque colonne
col_mask(C0,Mask) :- colmask(C0,Mask), !.
col_mask(C0,Mask) :- col_bits(C0,Bits), sum_bits(Bits,Mask), assertz(colmask(C0,Mask)).

center_bonus(MX,MO,Score) :-
    col_mask(3,M),
    CX is popcount(MX /\ M),
    CO is popcount(MO /\ M),
    Score is 3 * (CX - CO).

three_two_bonus(MX,MO,Score) :-
    win_masks(Ms),
    findall(S,
        ( member(W,Ms),
          Xw is MX /\ W, Ow is MO /\ W,
          ( Ow =:= 0 -> score_window(popcount(Xw), Sx) ; Sx = 0 ),
          ( Xw =:= 0 -> score_window(popcount(Ow), So) ; So = 0 ),
          S is Sx - So ),
        L),
    sum_list(L, Score).

score_window(0,0). score_window(1,1). score_window(2,5).
score_window(3,25). score_window(4,1000000).

% =======================================
% Table de transposition
% =======================================
tt_lookup(Hash,D,A,B,S) :-
    tt(Hash, D0, S0, Flag), D0 >= D,
    ( Flag=exact -> S=S0
    ; Flag=lower, S0 >= B -> S=S0
    ; Flag=upper, S0 =< A -> S=S0
    ; fail ).

tt_store(Hash,D,V,A0,B0) :-
    ( V =< A0 -> F=upper
    ; V >= B0 -> F=lower
    ; F=exact ),
    retractall(tt(Hash,_,_,_)),
    assertz(tt(Hash,D,V,F)).

ttm_update(Hash,Move) :- retractall(ttm(Hash,_)), assertz(ttm(Hash,Move)).

% =======================================
% Ordonnancement (gains, TT, killer)
% =======================================
order_moves(MX,MO,P,Order,Depth,Hash,Ordered) :-
    legal_moves(MX,MO,Order,LM),
    ( LM = [] -> Ordered=[]
    ; findall(Cw,
         (member(Cw,LM),
          apply_move(MX,MO,P,Cw,MX1,MO1,_),
          (P=='x'->has_won(MX1);has_won(MO1))),
         Win0),
      list_unique(Win0,Win),
      (ttm(Hash,TT), memberchk(TT,LM) -> TTList=[TT] ; TTList=[]),
      (killer(Depth,KM), memberchk(KM,LM) -> KList=[KM] ; KList=[]),
      append(Win, TTList, P1),
      add_if_absent_list(P1, KList, P2),
      subtract_keep_order(LM, P2, Rest),
      append(P2, Rest, Ordered)
    ).

list_unique([],[]).
list_unique([X|Xs],[X|Ys]) :- \+ memberchk(X,Xs), !, list_unique(Xs,Ys).
list_unique([_|Xs],Ys) :- list_unique(Xs,Ys).

add_if_absent_list(L, [], L).
add_if_absent_list(L, [X|Xs], L2) :-
    ( memberchk(X,L) -> L1=L ; append(L,[X],L1) ),
    add_if_absent_list(L1,Xs,L2).

subtract_keep_order([], _, []).
subtract_keep_order([X|Xs], Rm, Ys) :-
    ( memberchk(X,Rm) -> subtract_keep_order(Xs,Rm,Ys)
    ; Ys=[X|T], subtract_keep_order(Xs,Rm,T)
    ).

% =======================================
% Négamax + PVS + LMR
% =======================================
zobrist_hash(MX,MO,P,H) :- term_hash(state(MX,MO,P), H).
switch('x','o'). switch('o','x').

negamax(MX,MO,P,D,A,B,O,Val,Col) :-
    ( terminal_score(MX,MO,P,S) ->
        Val=S, Col=4
    ; D =:= 0 ->
        heuristic(MX,MO,P,H), Val=H, Col=4
    ; zobrist_hash(MX,MO,P,Hh),
      order_moves(MX,MO,P,O,D,Hh,Moves),
      Moves \= [] ->
        ( tt_lookup(Hh,D,A,B,Vt) ->
            Val=Vt, Col=4
        ; A0 = A, B0 = B,
          pvs_search(MX,MO,P,D,A0,B0,O,Moves,BestV,BestC),
          tt_store(Hh,D,BestV,A0,B0),
          ttm_update(Hh,BestC),
          Val=BestV, Col=BestC
        )
    ; Val=0, Col=4 ).

% Première branche pleine, suivantes fenêtre nulle + LMR
pvs_search(MX,MO,P,D,A,B,O,[C|Cs],BestV,BestC) :-
    apply_move(MX,MO,P,C,MX1,MO1,_),
    switch(P,N), D1 is D-1, NA is -B, NB is -A,
    negamax(MX1,MO1,N,D1,NA,NB,O,ChildV,_),
    V1 is -ChildV, A1 is max(A,V1),
    ( A1 >= B ->
        assert_killer(D,C), BestV=V1, BestC=C
    ; pvs_tail(MX,MO,P,D,A1,B,O,Cs,V1,C,BestV,BestC)
    ).

pvs_tail(_,_,_,_,A,_,_,[],CurV,CurC,CurV,CurC).
pvs_tail(MX,MO,P,D,A,B,O,[C|Cs],CurV,CurC,BestV,BestC) :-
    apply_move(MX,MO,P,C,MX1,MO1,_),
    switch(P,N), D1 is D-1,
    ( D >= 3 ->
        negamax(MX1,MO1,N,D1-1, -A-1, -A, O, Vnull1,_),
        Vnull is -Vnull1,
        ( Vnull > A, Vnull < B ->
            negamax(MX1,MO1,N,D1, -B, -A, O, Vfull1,_),
            V is -Vfull1
        ; V = Vnull )
    ; negamax(MX1,MO1,N,D1, -A-1, -A, O, Vn1,_),
      V is -Vn1
    ),
    ( V > CurV ->
        A1 is max(A,V),
        ( A1 >= B ->
            assert_killer(D,C), BestV=V, BestC=C
        ; pvs_tail(MX,MO,P,D,A1,B,O,Cs,V,C,BestV,BestC)
        )
    ; pvs_tail(MX,MO,P,D,A,B,O,Cs,CurV,CurC,BestV,BestC)
    ).

assert_killer(D,C) :- retractall(killer(D,_)), assertz(killer(D,C)).

% =======================================
% Utilitaires
% =======================================
sum_bits(List,B) :- sum_bits_(List,0,B).
sum_bits_([],Acc,Acc).
sum_bits_([X|Xs],Acc,B) :- Acc1 is Acc \/ X, sum_bits_(Xs,Acc1,B).

popcount(X,N) :- popcount_(X,0,N).
popcount_(0,C,C) :- !.
popcount_(X,C0,C) :- X1 is X /\ (X-1), C1 is C0+1, popcount_(X1,C1,C). 
