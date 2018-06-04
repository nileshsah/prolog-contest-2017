%% tourism2.pl (SWI-Prolog)
% swipl -g "main,halt" tourism2.pl ins.pl

inc_prefer([(Idx,V)|T], Idx, R) :-
    NewV is V + 1,
    R = [(Idx,NewV)|T].
inc_prefer([(I,V)|T], Idx, [(I,V)|R]) :-
    I \= Idx,
    inc_prefer(T, Idx, R).

list_length([], I, SZ) :-
    SZ = I.
list_length([X|R], I, SZ) :-
    NewI is I + 1,
    list_length(R, NewI, SZ).

getMinVal([], MinVal) :-
    MinVal is 10000.
getMinVal([(Id,X)|P], MinVal) :-
    findall(prefer(Id, L), prefer(Id, L), P1),
    list_length(P1, 0, SZ),
    SZ == X,
    locations(M),
    getMinVal(P, MV),
    MinVal is min(M, MV).
getMinVal([(Id,X)|P], MinVal) :-
    getMinVal(P, MV),
    MinVal is min(X, MV).

updatePrefer(_, [], Pref, NewPref) :-
    NewPref = Pref.
updatePrefer(Loc, [prefer(P, L)|R], Pref, NewPref) :-
    Loc \= L,
    updatePrefer(Loc, R, Pref, NewPref).
updatePrefer(L, [prefer(P, L)|R], Pref, NewPref) :-
    inc_prefer(Pref, P, T),
    updatePrefer(L, R, T, NewPref).

solve([], _, Pref, Soln) :-
    getMinVal(Pref, MV),
    %write(Pref), write(MV), nl,
    Soln is MV.
solve([location(L, D, O, C)|Loc], T, Pref, Soln) :-
    Till is T + D,
    Till =< C,
    NewTime is max(O + D, T + D),
    findall(prefer(P, L), prefer(P, L), Prefer),
    updatePrefer(L, Prefer, Pref, NewPref),
    solve(Loc, NewTime, NewPref, S1),
    solve(Loc, T, Pref, S2),
    Soln is max(S1, S2).
solve([location(L, D, O, C)|Loc], T, Pref, Soln) :-
    Till is T + D,
    Till > C,  
    solve(Loc, T, Pref, S1),
    Soln is S1.

createArr(I, M, R) :-
    I > M,
    R = [].
createArr(I, M, [(I,0)|R]) :-
    NewI is I + 1,
    createArr(NewI, M, R).

%% Assuming that the location list is sorted based on closing time

main :-
    people(N),
    locations(M),
    preferences(K),
    findall(location(L, D, O, C), location(L, D, O, C), L1),
    createArr(1, N, P1),
    solve(L1, 0, P1, Soln),
    write('satisfaction('), write(Soln), write(').').
