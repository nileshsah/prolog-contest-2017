%% buffet.pl (SWI-Prolog)
% swipl -g "main,halt" buffet.pl ins.pl

calcWidth([], [], CurW, TotalW) :-
    TotalW = CurW.
calcWidth([dish_width(D,W)|Width], [demand(D,Q)|Demand], CurW, TotalW) :-
    NewCurW is CurW + W * Q,
    calcWidth(Width, Demand, NewCurW, TotalW).

solve(Width, Demand) :-
    dishes(N),
    separation(S),
    hot(H),
    table_width(L),
    H < N,
    calcWidth(Width, Demand, 0, W),
    Soln is ceil((W + S)/L),
    write('tables('), write(Soln), write(')').
solve(Width, Demand) :-
    dishes(N),
    hot(H),
    table_width(L),
    H >= N,
    calcWidth(Width, Demand, 0, W),
    Soln is ceil(W/L),
    write('tables('), write(Soln), write(')').

main :-
    dishes(N),
    separation(S),
    hot(H),
    table_width(L),
    findall(dish_width(D, W), dish_width(D, W), Width),
    findall(demand(D, Q), demand(D, Q), Demand),
    solve(Width, Demand).

