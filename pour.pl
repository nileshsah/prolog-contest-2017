%% pour.pl (SWI-Prolog)
% swipl -g "main,halt" pour.pl ins.pl

:- dynamic(explored/2).

%% TODO: Fix solution logic for calculating the number of valid beer glasses

modify_jug_quantity([jug(Idx,K,C)|T], Idx, X, [jug(Idx,K,X)|T]).
modify_jug_quantity([jug(I,K,C)|T], Idx, X, [jug(I,K,C)|R]) :-
    I \= Idx,
    modify_jug_quantity(T, Idx, X, R).

verify_soln(_, _, 0, _).
verify_soln([jug(I,K,C)|Jugs], Visited, M, Req) :-
    verify_soln(Jugs, Visited, M, Req).
verify_soln([jug(I,K,C)|Jugs], Visited, M, Req) :-
    \+ member(jug(I,K,C), Visited),
    C == Req,
    M1 is M-1,
    verify_soln(Jugs, Visited, M1, Req).
verify_soln([jug(I,K,C)|Jugs], Visited, M, Req) :-
    \+ member(jug(I,K,C), Visited),
    C < Req,
    Left is Req - C,
    member(jug(I2, K2, C2), Jugs),
    C2 == Left,
    M1 is M-1,
    verify_soln(Jugs, [jug(I2,K2,C2)|Visited], M1, Req).

solve(Jugs, Iter, M, Req) :-
    Iter > 0,
    verify_soln(Jugs, [], M, Req),
    write(Jugs), nl.
solve(Jugs, Iter, M, Req) :-
    Iter > 0,
    \+ verify_soln(Jugs, [], M, Req),
    \+ explored(Jugs, Iter),
    assert(explored(Jugs, Iter)),
    member(jug(I1,K1,C1), Jugs),
    member(jug(I2,K2,C2), Jugs),
    jug(I1,K1,C1) \= jug(I2,K2,C2),
    C1 > 0,
    C2 < K2,
    ToExch is min(C1, K2-C2),
    NewC1 is C1 - ToExch,
    NewC2 is C2 + ToExch,
    modify_jug_quantity(Jugs, I1, NewC1, P1),
    modify_jug_quantity(P1, I2, NewC2, P2),
    NewIter is Iter - 1,
    solve(P2, NewIter, M, Req).

process(R, L, K, M, Req) :-
    Modval is mod(K, M),
    Modval == 0,
    solve(R, L, M, Req),
    write('split(yes)').
process(R, L, K, M, Req) :-
    write('split(no)').

main :-
    vessels(N),
    source(V),
    people(M),
    findall(jug(I,K,0),capacity(I,K),Jugs),
    horizon(L),
    member(jug(Idx,K,C), Jugs),
    Idx == V,
    modify_jug_quantity(Jugs, V, K, R),
    Req is K / M,
    process(R, L, K, M, Req).
