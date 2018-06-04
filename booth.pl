%% booth.pl (SWI-Prolog)
% swipl -g "main,halt" booth.pl ins.pl

modify_position([position(Idx,X,Y)|T], Idx, NewX, NewY, [position(Idx,NewX,NewY)|T]).
modify_position([position(I,X,Y)|T], Idx, NewX, NewY, [position(I,X,Y)|R]) :-
    I \= Idx,
    modify_position(T, Idx, NewX, NewY, R).

collides(Positions, Booths) :-
    member(position(I1, X1, Y1), Positions),
    member(dimension(I1,W1,H1), Booths),
    member(position(I2,X2,Y2), Positions),
    member(dimension(I2,W2,H2), Booths),
    I1 \= I2,
    EX1 is X1 + W1,
    EY1 is Y1 + H1,
    EX2 is X2 + W2,
    EY2 is Y2 + H2,
    \+ Y1 >= EY2, 
    \+ Y2 >= EY1, 
    \+ EX1 =< X2, 
    \+ EX2 =< X1.

valid(Idx, X, Y, Booths) :-
    room(W,H),
    member(dimension(Idx,Wt,Ht), Booths),
    Xf is X + Wt,
    Yf is Y + Ht,
    X >= 0, X =< W,
    Y >= 0, Y =< H,
    Xf >= 0, Xf =< W,
    Yf >= 0, Yf =< H.

is_diff(L1, L2) :-
    member(X, L1),
    \+ member(X, L2).

solve(G, [(Cur, Idx, D)|_], _, _) :-
    \+ is_diff(G, Cur),
    write('moves('), write(D), write(')').
solve(G, [(Cur, Idx, D)|Rest], Visited, Booths) :-
    booths(N),
    Idx > N,
    solve(G, Rest, Visited, Booths).
solve(G, [(Cur, Idx, D)|Rest], Visited, Booths) :-
    member(position(I,X,Y), Cur),
    \+ valid(I, X, Y, Booths),
    %write("NAN: "), write(Cur), nl,
    solve(G, Rest, Visited, Booths).
solve(G, [(Cur, Idx, D)|Rest], Visited, Booths) :-
    collides(Cur, Booths),
    %write("Collides: "), write(Cur), nl,
    solve(G, Rest, Visited, Booths).
solve(G, [(Cur, Idx, D)|Rest], Visited, Booths) :-
    member((X, T), Visited),
    Idx == T,
    \+ is_diff(X, Cur),
    %write("Duplicate: "), write(Cur), nl,
    solve(G, Rest, Visited, Booths).
solve(G, [(Cur, Idx, D)|Rest], Visited, Booths) :-
    booths(N),
    NewIdx is Idx + 1,
    member(position(Idx, X, Y), Cur),
    %write(Cur), write((Idx,D)), nl,
    NewD is D + 1,
    IncX is X + 1,
    DecX is X - 1,
    IncY is Y + 1,
    DecY is Y - 1,
    modify_position(Cur, Idx, IncX, Y, R1),
    modify_position(Cur, Idx, DecX, Y, R2),
    modify_position(Cur, Idx, X, IncY, R3),
    modify_position(Cur, Idx, X, DecY, R4),
    append([(Cur, NewIdx, D)|Rest], [(R1, 1, NewD), (R2, 1, NewD), (R3, 1, NewD), (R4, 1, NewD)], Q),
    solve(G, Q, [(Cur, Idx)|Visited], Booths).

main :-
    room(W,H),
    booths(N),
    findall(dimension(I, Wi, He), dimension(I, Wi, He), Booths),
    findall(position(I, X, Y), position(I, X, Y), Positions),
    target(Idx, Xf, Yf),
    member(position(Idx, Xs, Ys), Positions),
    modify_position(Positions, Idx, Xf, Yf, G),
    solve(G, [(Positions, 1, 0)], [], Booths).
