% ПІБ: Yermolovych Zakhar Maksymovych
% tsp_clpfd_Yermolovych.pl
% Модуль TSP через CLP(FD): circuit/1, element/3, labeling з min(Cost).
% Точний оптимум через обмеження. Tour у форматі [1,...,1] будується з successor-подання.

:- module(tsp_clpfd_Yermolovych, [tsp_clpfd/3]).

:- use_module(library(clpfd)).                  % ins/2, (#=)/2, circuit/1, element/3, labeling/2, sum/3
:- use_module(tsp_common_Yermolovych, [matrix_to_flat/2]).

% Призначення / мультипризначенність:
% - tsp_clpfd(++Matrix, --Tour, --Cost): основне призначення — знайти оптимальний тур і його вартість.
% - Інші комбінації індикаторів (наприклад, --Matrix, ?Tour, ?Cost) не є змістовними,
%   оскільки задача потребує заданої матриці відстаней і виконує оптимізацію через пошук.
%
% Пояснення CLP(FD) (детально): див. docs/clpfd_explained.md

/* ------------------------------------------------------------------------
   tsp_clpfd/3
   ------------------------------------------------------------------------ */

%% tsp_clpfd(++Matrix, --Tour, --Cost) is semidet.
%  Purpose:
%    Розв'язує TSP як задачу обмежень CLP(FD):
%      1) вводиться successor-представлення Succs довжини N (Succs[I] = місто-послідовник);
%      2) Succs ins 1..N і circuit(Succs) задають один гамільтонів цикл;
%      3) матриця Matrix сплющується у Flat (рядок за рядком);
%      4) для кожного I зв'язується вартість ребра I->Succs[I] через element/3:
%         Idx #= (I-1)*N + Succs[I], element(Idx, Flat, Di);
%      5) Cost #= sum(Ds) і виконується labeling([ffc, min(Cost)], Succs);
%      6) з Succs відновлюється Tour у форматі [1,...,1].
%
%  Інші комбінації (пояснення відсутності змістовних призначень):
%    - (++Matrix, -Tour, +Cost)  Перевірка “чи існує тур із заданою вартістю Cost” не є метою;
%                               предикат використовується для знаходження оптимуму.
%
%  Неочевидні built-ins (CLP(FD)):
%    - once/1: залишає лише перше рішення (після мінімізації це оптимум).
%
/** <examples>
?- tsp_clpfd([[0,2,9],[2,0,6],[9,6,0]], Tour, Cost).
% Tour = [1,2,3,1],
% Cost = 17.

?- tsp_clpfd([[0,1],[1,0]], T, C).
% T = [1,2,1],
% C = 2.

?- tsp_clpfd(M, T, C).
% ERROR: Type error: `list' expected, found _G... (an unbound variable)
*/
% tsp_clpfd(++Matrix, --Tour, --Cost).
tsp_clpfd(Matrix, Tour, Cost) :-
    must_be(list, Matrix),
    length(Matrix, N),
    N >= 1,
    (   N =:= 1
    ->  Tour = [1, 1],
        Cost = 0
    ;   length(Succs, N),
        Succs ins 1..N,
        circuit(Succs),
        matrix_to_flat(Matrix, Flat),
        build_cost_constraints(N, Flat, Succs, Cost),
        once(labeling([ffc, min(Cost)], Succs)),
        succs_to_tour(Succs, Tour)
    ).

/* ------------------------------------------------------------------------
   succs_to_tour/2
   ------------------------------------------------------------------------ */

%% succs_to_tour(++Succs, --Tour) is det.
%  Purpose:
%    Перетворює successor-подання Succs у замкнутий тур Tour = [1,...,1] довжини N+1.
%    Обхід починається з міста 1 і робиться рівно N кроків:
%      Next = Succs[Current], потім Current := Next.
%    circuit/1 гарантує повернення у 1 після N кроків.
%
%  Індикатори / призначення (змістовні):
%    1) (++Succs, --Tour)  Succs заданий, Tour повертається.
%
%  Інші комбінації (пояснення відсутності змістовних призначень):
%    - (-Succs, ?)  Генерація Succs не є задачею цього предикату.
%
%  Неочевидні built-ins:
%    - nth1/3: доступ до елемента списку за індексом (1-based).
%
/** <examples>
?- succs_to_tour([2,3,1], Tour).
% Tour = [1,2,3,1].

?- succs_to_tour([2,1], Tour).
% Tour = [1,2,1].
*/
% succs_to_tour(++Succs, --Tour).
succs_to_tour(Succs, Tour) :-
    must_be(list, Succs),
    length(Succs, N),
    walk(1, N, Succs, Visited),
    Tour = [1 | Visited].

/* ------------------------------------------------------------------------
   walk/4
   ------------------------------------------------------------------------ */

%% walk(++Current, ++StepsLeft, ++Succs, --Visited) is det.
%  Purpose:
%    Виконує StepsLeft кроків по successor-ребрах:
%      Next = Succs[Current],
%    і накопичує послідовність відвіданих вершин у Visited.
%
%  Індикатори / призначення (змістовні):
%    1) (++Current, ++StepsLeft, ++Succs, --Visited)  Обхід для побудови туру.
%
%  Інші комбінації (пояснення відсутності змістовних призначень):
%    - (?, ?, -Succs, ?)  Succs має бути заданий (це “граф”).
%
%  Неочевидні built-ins:
%    - is/2: звичайна арифметика (очевидний) для K1 is K - 1.
%
/** <examples>
?- walk(1, 3, [2,3,1], Vs).
% Vs = [2,3,1].

?- walk(1, 0, [2,3,1], Vs).
% Vs = [].
*/
% walk(++Current, ++StepsLeft, ++Succs, --Visited).
walk(_Current, 0, _Succs, []).
walk(Current, K, Succs, [Next | Rest]) :-
    K > 0,
    nth1(Current, Succs, Next),
    K1 is K - 1,
    walk(Next, K1, Succs, Rest).

/* ------------------------------------------------------------------------
   build_cost_constraints/4
   ------------------------------------------------------------------------ */

%% build_cost_constraints(++N, ++Flat, ++Succs, --Cost) is det.
%  Purpose:
%    Зв'язує successor-подання Succs з сумарною вартістю Cost через element/3:
%      для кожного I (1..N):
%        Si = Succs[I],
%        Idx #= (I-1)*N + Si,
%        element(Idx, Flat, Di),
%      Cost #= sum(Di).
%
%  Індикатори / призначення (змістовні):
%    1) (++N, ++Flat, ++Succs, --Cost)  N, Flat, Succs задані, Cost — FD-змінна.
%
%  Інші комбінації (пояснення відсутності змістовних призначень):
%    - (-N, ?, ?, ?)  Без N неможливо адресувати Flat.
%
%  Неочевидні built-ins (CLP(FD)):
%    - element/3: Di = Flat[Idx] (Idx 1-based, може бути FD-змінною).
%    - (#=)/2: обмеження для арифметики над FD.
%    - sum/3: сумування FD-значень.
%    - numlist/3: формує список індексів 1..N.
%    - maplist/3: застосовує edge_cost_i/5 до кожного індексу та збирає Ds.
%
/** <examples>
?- N=3,
   Flat=[0,2,9, 2,0,6, 9,6,0],
   Succs=[2,3,1],
   build_cost_constraints(N, Flat, Succs, Cost),
   labeling([min(Cost)], [Cost]).
% Cost = 17.

?- build_cost_constraints(2, [0,1,1,0], [2,1], Cost),
   labeling([min(Cost)], [Cost]).
% Cost = 2.
*/
% build_cost_constraints(++N, ++Flat, ++Succs, --Cost).
build_cost_constraints(N, Flat, Succs, Cost) :-
    must_be(integer, N),
    must_be(list, Flat),
    must_be(list, Succs),
    numlist(1, N, Indices),
    maplist(edge_cost_i(N, Flat, Succs), Indices, Ds),
    sum(Ds, #=, Cost).

/* ------------------------------------------------------------------------
   edge_cost_i/5
   ------------------------------------------------------------------------ */

%% edge_cost_i(++N, ++Flat, ++Succs, ++I, --Di) is det.
%  Purpose:
%    Для фіксованого I (1..N) задає Di як вартість ребра I -> Succs[I]:
%      Idx #= (I-1)*N + Succs[I],
%      element(Idx, Flat, Di).
%
%  Індикатори / призначення (змістовні):
%    1) (++N, ++Flat, ++Succs, ++I, --Di)  Обмеження для одного ребра.
%
%  Інші комбінації (пояснення відсутності змістовних призначень):
%    - (?, ?, -Succs, ?, ?)  Succs має бути заданий як список FD-змінних.
%
%  Неочевидні built-ins (CLP(FD)):
%    - element/3 і (#=)/2: дозволяють зв'язати індекс і значення у Flat.
%
/** <examples>
?- N=3,
   Flat=[0,2,9, 2,0,6, 9,6,0],
   Succs=[2,3,1],
   edge_cost_i(N, Flat, Succs, 1, D1),
   edge_cost_i(N, Flat, Succs, 2, D2),
   edge_cost_i(N, Flat, Succs, 3, D3),
   labeling([], [D1,D2,D3]).
% D1 = 2,
% D2 = 6,
% D3 = 9.
*/
% edge_cost_i(++N, ++Flat, ++Succs, ++I, --Di).
edge_cost_i(N, Flat, Succs, I, Di) :-
    nth1(I, Succs, Si),
    Idx #= (I - 1) * N + Si,
    element(Idx, Flat, Di).
