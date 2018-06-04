%% tourism1.pl (SWI-Prolog)
% swipl -g "main,halt" tourism1.pl ins.pl

modify_arr([(Idx,K)|T], Idx, X, [(Idx,X)|T]).
modify_arr([(I,K)|T], Idx, X, [(I,K)|R]) :-
    I \= Idx,
    modify_arr(T, Idx, X, R).

violates(P, X, EndY, Preferences) :-
    member(order(P,X,EndY), Preferences).
violates(P, X, EndY, Preferences) :-
    member(order(P,X,Z), Preferences),
    violates(P, Z, EndY, Preferences).

counterP(I, (X,Y), Preferences, Cur, Total) :-
    people(N),
    I > N,
    Total = Cur.
counterP(I, (X,Y), Preferences, Cur, Total) :-
    people(N),
    I =< N,
    violates(I, Y, X, Preferences),
    % write((I,X,Y)), nl,
    NewI is I + 1,
    NewCur is Cur + 1,
    counterP(NewI, (X,Y), Preferences, NewCur, Total).
counterP(I, (X,Y), Preferences, Cur, Total) :-
    people(N),
    I =< N,
    \+ violates(I, Y, X, Preferences),
    NewI is I + 1,
    counterP(NewI, (X,Y), Preferences, Cur, Total).

loop(_, [], Cur, Total) :-
    Total = Cur.
loop(X, [(Id,Y)|P], Cur, Total) :-
    findall(order(P1, X1, Y1), order(P1, X1, Y1), Preferences),
    I is 1,
    counterP(1, (X,Y), Preferences, 0, T1),
    NewCur is Cur + T1,
    loop(X, P, NewCur, Total).

countViolations([], Cur, Total) :-
    Total = Cur.
countViolations([(Id,X)|P], Cur, Total) :-
    loop(X, P, 0, T1),
    NewCur is Cur + T1,
    countViolations(P, NewCur, Total).

readN([X|Arr], I, N, Value) :-
    I == N,
    Value = X.
readN([X|Arr], I, N, Value) :-
    I \= N,
    NewI is I + 1,
    readN(Arr, NewI, N, Value).

permute(L, I, R, Arr, Soln) :-
    L == R,
    countViolations(Arr, 0, T),
    % write(Arr), write(T), nl,
    Soln = T.
permute(_, I, R, _, Soln) :-
    I > R,
    Soln = R * R.
permute(L, I, R, Arr, Soln) :-
    readN(Arr, 1, L, (IL, XL)),
    readN(Arr, 1, I, (IR, XR)),
    modify_arr(Arr, IL, XR, T1),
    modify_arr(T1, IR, XL, NewArr),
    NewL is L + 1,
    NewI is I + 1,
    permute(NewL, NewL, R, NewArr, S1),
    permute(L, NewI, R, Arr, S2),
    Soln is min(S1, S2).

createArr(I, M, R) :-
    I > M,
    R = [].
createArr(I, M, [(I,I)|R]) :-
    NewI is I + 1,
    createArr(NewI, M, R).

main :-
    people(N),
    locations(M),
    preferences(K),
    createArr(1, M, Arr),
    permute(1, 1, M, Arr, Soln),
    write('violations('), write(Soln), write(').').
