% =======================================
% Puissance 4 IA bitboards — tournoi
% Expose : jouer_coup/3
% =======================================
:- use_module(library(lists), [sum_list/2]).
:- dynamic table_transpo/4.       % table_transpo(Hash,Profondeur,Score,Type)
:- dynamic table_transpo_mouvement/2.  % table_transpo_mouvement(Hash,MeilleurCoup)
:- dynamic coup_tueur/2.          % coup_tueur(Profondeur,Colonne)
:- dynamic masques_gagnants/1.
:- dynamic masque_colonne/2.

% ----- Paramètres -----
profondeur_max(12).                       
ordre_centre([4,3,5,2,6,1,7]).      
infini(1000000000).

% =======================================
% Entrée tournoi
% GrilleLignes : 6 lignes x 7 colonnes, 0/1/2
% Joueur       : 1 ou 2
% ColonneSortie: 0..6  (EXIGÉ PAR LE TOURNOI)
% =======================================
jouer_coup(GrilleLignes, Joueur, ColonneSortie) :-
    convertir_grille_bitboards(GrilleLignes, BitsX, BitsO),
    retractall(coup_tueur(_,_)),                     
    symbole_joueur(Joueur, Symbole),
    ordre_centre(Ordre),
    profondeur_max(ProfMax), profondeur_racine(BitsX,BitsO,ProfMax,Profondeur),
    infini(Inf), NegInf is -Inf, PosInf is Inf,
    negamax(BitsX,BitsO,Symbole,Profondeur,NegInf,PosInf,Ordre,_Valeur,Candidat1a7),
    choisir_coup_valide(BitsX,BitsO,Ordre,Candidat1a7,CoupChoisi),
    (   getenv('P4_INDEXING', Mode), Mode == 'one_based'
    ->  ColonneSortie = CoupChoisi
    ;   ColonneSortie is CoupChoisi - 1).

choisir_coup_valide(BitsX,BitsO,Ordre,Candidat,CoupChoisi) :-
    ( colonne_pleine(BitsX,BitsO,Candidat) ->
        coups_legaux(BitsX,BitsO,Ordre,Legaux),
        ( Legaux = [CoupChoisi|_] -> true ; CoupChoisi = 4 )
    ; CoupChoisi = Candidat ).

symbole_joueur(1,'x').
symbole_joueur(2,'o').

% =======================================
% Conversion Grille -> Bitboards
% =======================================
position_bit(Ligne,Colonne,Pos) :- Pos is Ligne*7 + Colonne.

convertir_grille_bitboards(Grille, BitsX, BitsO) :-
    % bits 'x'
    findall(Bx,
        ( between(0,5,Ligne), L1 is Ligne+1, nth1(L1, Grille, Row),
          between(0,6,Colonne), C1 is Colonne+1, nth1(C1, Row, V), V =:= 1,
          position_bit(Ligne,Colonne,Pos), Bx is 1<<Pos ),
        ListeX),
    somme_bits(ListeX, BitsX),
    % bits 'o'
    findall(Bo,
        ( between(0,5,Ligne), L1 is Ligne+1, nth1(L1, Grille, Row),
          between(0,6,Colonne), C1 is Colonne+1, nth1(C1, Row, V), V =:= 2,
          position_bit(Ligne,Colonne,Pos), Bo is 1<<Pos ),
        ListeO),
    somme_bits(ListeO, BitsO).

% =======================================
% Bitboards : coups, états, gagnants
% =======================================
cases_occupees(BitsX,BitsO,Occ) :- Occ is BitsX \/ BitsO.

% bits d’une colonne Col (0..6)
bits_colonne(Col, Bits) :-
    findall(B, (between(0,5,Ligne), position_bit(Ligne,Col,Pos), B is 1<<Pos), Bs),
    reverse(Bs, Bits).

colonne_pleine(BitsX,BitsO,Colonne) :-
    C0 is Colonne-1, position_bit(0,C0,Pos), B is 1<<Pos,
    cases_occupees(BitsX,BitsO,Occ), (Occ /\ B) =\= 0.

coups_legaux(BitsX,BitsO,Ordre,Colonnes) :-
    findall(C, (member(C,Ordre), \+ colonne_pleine(BitsX,BitsO,C)), Colonnes).

appliquer_coup(BitsX,BitsO,Symbole,Colonne,BitsX2,BitsO2,DernierBit) :-
    C0 is Colonne-1, bits_colonne(C0, Bits),
    cases_occupees(BitsX,BitsO,Occ),
    member(DernierBit, Bits), (Occ /\ DernierBit) =:= 0, !,
    ( Symbole=='x' -> BitsX2 is BitsX \/ DernierBit, BitsO2 = BitsO
    ;                  BitsO2 is BitsO \/ DernierBit, BitsX2 = BitsX ).

% --- Masques gagnants ---
:- initialization(init_masques_gagnants).

init_masques_gagnants :-
    findall(M, masque_gagnant(M), ListeMasques),
    retractall(masques_gagnants(_)),
    assertz(masques_gagnants(ListeMasques)).

masque_gagnant(M):-between(0,5,L),between(0,3,C),masque_sequence(L,C,0,1,M).  % Horizontal
masque_gagnant(M):-between(0,2,L),between(0,6,C),masque_sequence(L,C,1,0,M).  % Vertical
masque_gagnant(M):-between(0,2,L),between(0,3,C),masque_sequence(L,C,1,1,M).  % Diagonale /
masque_gagnant(M):-between(3,5,L),between(0,3,C),masque_sequence(L,C,-1,1,M). % Diagonale \

masque_sequence(L,C,DL,DC,M):-
    position_bit(L,C,P0),
    L1 is L+DL, C1 is C+DC, position_bit(L1,C1,P1),
    L2 is L1+DL, C2 is C1+DC, position_bit(L2,C2,P2),
    L3 is L2+DL, C3 is C2+DC, position_bit(L3,C3,P3),
    M is (1<<P0) \/ (1<<P1) \/ (1<<P2) \/ (1<<P3).

a_gagne(M) :- masques_gagnants(Ms), member(W,Ms), (M /\ W) =:= W, !.

grille_pleine(BitsX,BitsO) :-
    cases_occupees(BitsX,BitsO,Occ), (1<<42)-1 =:= Occ.

% =======================================
% Évaluation et états terminaux
% =======================================
score_terminal(BitsX,BitsO,Symbole,Score) :-
    ( a_gagne(BitsX) -> Gagnant = x
    ; a_gagne(BitsO) -> Gagnant = o
    ; Gagnant = aucun ),
    ( Gagnant == aucun ->
        ( grille_pleine(BitsX,BitsO) -> Score = 0 ; fail )
    ; ( Symbole == Gagnant ->
          score_victoire(BitsX,BitsO,1000000,Score)
      ;   score_victoire(BitsX,BitsO,1000000,S), Score is -S )
    ).

heuristique(BitsX,BitsO,_S,Score) :-
    bonus_centre(BitsX,BitsO,Bc),
    bonus_alignes(BitsX,BitsO,Ba),
    Score is Bc + Ba.

masque_colonne_cache(C0,Masque) :- masque_colonne(C0,Masque), !.
masque_colonne_cache(C0,Masque) :- bits_colonne(C0,Bits), somme_bits(Bits,Masque), assertz(masque_colonne(C0,Masque)).

bonus_centre(BitsX,BitsO,Score) :-
    masque_colonne_cache(3,M),
    Cx is compteur_bits(BitsX /\ M),
    Co is compteur_bits(BitsO /\ M),
    Score is 3 * (Cx - Co).

bonus_alignes(BitsX,BitsO,Score) :-
    masques_gagnants(Ms),
    findall(S,
        ( member(W,Ms),
          Xw is BitsX /\ W, Ow is BitsO /\ W,
          ( Ow =:= 0 -> score_fenetre(compteur_bits(Xw), Sx) ; Sx = 0 ),
          ( Xw =:= 0 -> score_fenetre(compteur_bits(Ow), So) ; So = 0 ),
          S is Sx - So ),
        Liste),
    sum_list(Liste, Score).

score_fenetre(0,0). score_fenetre(1,1). score_fenetre(2,5).
score_fenetre(3,25). score_fenetre(4,1000000).

% =======================================
% Table de transposition
% =======================================
tt_rechercher(Hash,D,A,B,S) :-
    table_transpo(Hash, D0, S0, Type), D0 >= D,
    ( Type=exact -> S=S0
    ; Type=borne_inf, S0 >= B -> S=S0
    ; Type=borne_sup, S0 =< A -> S=S0
    ; fail ).

tt_enregistrer(Hash,D,V,A0,B0) :-
    ( V =< A0 -> Type=borne_sup
    ; V >= B0 -> Type=borne_inf
    ; Type=exact ),
    retractall(table_transpo(Hash,_,_,_)),
    assertz(table_transpo(Hash,D,V,Type)).

ttm_maj(Hash,Coup) :- retractall(table_transpo_mouvement(Hash,_)), assertz(table_transpo_mouvement(Hash,Coup)).

% =======================================
% Ordonnancement (gains, TT, coup tueur)
% =======================================
ordonner_coups(BitsX,BitsO,S,Ordre,Profondeur,Hash,Ordonne) :-
    coups_legaux(BitsX,BitsO,Ordre,Legaux),
    ( Legaux = [] -> Ordonne=[]
    ; findall(G,
         (member(G,Legaux),
          appliquer_coup(BitsX,BitsO,S,G,Bx1,Bo1,_),
          (S=='x'->a_gagne(Bx1);a_gagne(Bo1))),
         Gagnants0),
      liste_unique(Gagnants0,Gagnants),
      (table_transpo_mouvement(Hash,TT), memberchk(TT,Legaux) -> ListeTT=[TT] ; ListeTT=[]),
      (coup_tueur(Profondeur,CT), memberchk(CT,Legaux) -> ListeCT=[CT] ; ListeCT=[]),
      append(Gagnants, ListeTT, P1),
      ajouter_si_absent(P1, ListeCT, P2),
      soustraire_garder_ordre(Legaux, P2, Reste),
      append(P2, Reste, Ordonne)
    ).

liste_unique([],[]).
liste_unique([X|Xs],[X|Ys]) :- \+ memberchk(X,Xs), !, liste_unique(Xs,Ys).
liste_unique([_|Xs],Ys) :- liste_unique(Xs,Ys).

ajouter_si_absent(L, [], L).
ajouter_si_absent(L, [X|Xs], L2) :-
    ( memberchk(X,L) -> L1=L ; append(L,[X],L1) ),
    ajouter_si_absent(L1,Xs,L2).

soustraire_garder_ordre([], _, []).
soustraire_garder_ordre([X|Xs], Rm, Ys) :-
    ( memberchk(X,Rm) -> soustraire_garder_ordre(Xs,Rm,Ys)
    ; Ys=[X|T], soustraire_garder_ordre(Xs,Rm,T)
    ).

% =======================================
% Négamax + PVS + LMR
% =======================================
hash_zobrist(BitsX,BitsO,S,H) :- term_hash(etat(BitsX,BitsO,S), H).
echanger('x','o'). echanger('o','x').

negamax(BitsX,BitsO,S,Prof,A,B,Ordre,Valeur,Colonne) :-
    ( score_terminal(BitsX,BitsO,S,Sval) ->
        Valeur=Sval, Colonne=4
    ; Prof =:= 0 ->
        heuristique(BitsX,BitsO,S,H), Valeur=H, Colonne=4
    ; hash_zobrist(BitsX,BitsO,S,Hash),
      ordonner_coups(BitsX,BitsO,S,Ordre,Prof,Hash,Coups),
      Coups \= [] ->
        ( tt_rechercher(Hash,Prof,A,B,Vt) ->
            Valeur=Vt, Colonne=4
        ; A0 = A, B0 = B,
          recherche_pvs(BitsX,BitsO,S,Prof,A0,B0,Ordre,Coups,MeilleurV,MeilleurC),
          tt_enregistrer(Hash,Prof,MeilleurV,A0,B0),
          ttm_maj(Hash,MeilleurC),
          Valeur=MeilleurV, Colonne=MeilleurC
        )
    ; Valeur=0, Colonne=4 ).

recherche_pvs(BitsX,BitsO,S,Prof,A,B,Ordre,[C|Cs],BestV,BestC) :-
    appliquer_coup(BitsX,BitsO,S,C,Bx1,Bo1,_),
    echanger(S,Suivant), P1 is Prof-1, NA is -B, NB is -A,
    negamax(Bx1,Bo1,Suivant,P1,NA,NB,Ordre,Vfils1,_),
    V1 is -Vfils1, A1 is max(A,V1),
    ( A1 >= B ->
        assert_coup_tueur(Prof,C), BestV=V1, BestC=C
    ; recherche_pvs_suite(BitsX,BitsO,S,Prof,A1,B,Ordre,Cs,V1,C,BestV,BestC)
    ).

recherche_pvs_suite(_,_,_,_,_,_,_,[],CurV,CurC,CurV,CurC).
recherche_pvs_suite(BitsX,BitsO,S,Prof,A,B,Ordre,[C|Cs],CurV,CurC,BestV,BestC) :-
    appliquer_coup(BitsX,BitsO,S,C,Bx1,Bo1,_),
    echanger(S,Suivant), P1 is Prof-1,
    ( Prof >= 3 ->
        negamax(Bx1,Bo1,Suivant,P1-1, -A-1, -A, Ordre,Vn1,_),
        Vn is -Vn1,
        ( Vn > A, Vn < B ->
            negamax(Bx1,Bo1,Suivant,P1, -B, -A, Ordre,Vf1,_),
            V is -Vf1
        ; V = Vn )
    ; negamax(Bx1,Bo1,Suivant,P1, -A-1, -A, Ordre,Vtmp1,_),
      V is -Vtmp1
    ),
    ( V > CurV ->
        A1 is max(A,V),
        ( A1 >= B ->
            assert_coup_tueur(Prof,C), BestV=V, BestC=C
        ; recherche_pvs_suite(BitsX,BitsO,S,Prof,A1,B,Ordre,Cs,V,C,BestV,BestC)
        )
    ; recherche_pvs_suite(BitsX,BitsO,S,Prof,A,B,Ordre,Cs,CurV,CurC,BestV,BestC)
    ).

assert_coup_tueur(Prof,C) :- retractall(coup_tueur(Prof,_)), assertz(coup_tueur(Prof,C)).

% =======================================
% Utilitaires
% =======================================
somme_bits(Liste,B) :- somme_bits_(Liste,0,B).
somme_bits_([],Acc,Acc).
somme_bits_([X|Xs],Acc,B) :- Acc1 is Acc \/ X, somme_bits_(Xs,Acc1,B).

compteur_bits(X,N) :- compteur_bits_(X,0,N).
compteur_bits_(0,C,C) :- !.
compteur_bits_(X,C0,C) :- X1 is X /\ (X-1), C1 is C0+1, compteur_bits_(X1,C1,C).

cases_vides(BitsX,BitsO,N) :- cases_occupees(BitsX,BitsO,Occ), compteur_bits(Occ,K), N is 42-K.
profondeur_racine(BitsX,BitsO,Prof0,Prof) :- cases_vides(BitsX,BitsO,V), (V < Prof0 -> Prof=V ; Prof=Prof0).
score_victoire(BitsX,BitsO,Base,Score) :- cases_vides(BitsX,BitsO,V), Score is Base - V.
