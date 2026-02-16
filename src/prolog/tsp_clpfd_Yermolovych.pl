% tsp_clpfd_Yermolovych.pl
% Модуль TSP через CLP(FD): circuit/1, element/3, labeling з min(Cost).
% Точний оптимум через обмеження. Tour у форматі [1,...,1] через succs_to_tour/2.

:- module(tsp_clpfd_Yermolovych, [tsp_clpfd/3]).

:- use_module(library(clpfd)).
:- use_module(tsp_common_Yermolovych, [matrix_to_flat/2]).

%% tsp_clpfd(++Matrix, --Tour, --Cost) is semidet.
%  Purpose:
%    Точний оптимум TSP через обмеження CLP(FD). Succs ins 1..N, circuit(Succs),
%    вартість через element/3 по сплющеній матриці. once(labeling([ffc, min(Cost)], Succs)).
%    Tour будується обходом Succs починаючи з 1 — формат [1,...,1].
%
%  Modes (meaningful):
%    1) (++Matrix, -Tour, -Cost)  Matrix задана, Tour і Cost — результат.
%
%  Modes (not meaningful):
%    (-Matrix, ?, ?)  Matrix обов'язкова.
%    (++Matrix, ++Tour, ++Cost)  Предикат генерує рішення.
%
%  Notes / built-ins:
%    - circuit/1: Succs[i] = місто, куди йдемо з i; гарантує один Hamiltonian cycle.
%    - labeling/2: перебір варіантів для FD-змінних; min(Cost) — мінімізація.
%    - once/1: повертає лише перше (оптимальне) рішення.
%
/** <examples>
?- tsp_clpfd([[0,2,9],[2,0,6],[9,6,0]], Tour, Cost).
% очікується: Tour = [1,2,3,1], Cost = 17 (Cost = brute force).

?- tsp_clpfd([[0,1],[1,0]], T, C).
% очікується: T = [1,2,1], C = 2.

?- tsp_clpfd(M, T, C).
% очікується: instantiation_error.

% Перевірка збігу з brute force:
% n5_demo.tsp: Cost = 17 (tsp_bruteforce і tsp_clpfd однакові).
% n8_01.tsp: Cost однаковий для обох алгоритмів.
*/
tsp_clpfd(Matrix, Tour, Cost) :-
    must_be(list, Matrix),
    length(Matrix, N),
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

%% succs_to_tour(++Succs, --Tour) is det.
%  Purpose:
%    Перетворює successor-представлення в Tour = [1,...,1] довжини N+1.
%    Обхід починається з 1: N кроків Next = Succs[Current], додаємо Next. circuit гарантує
%    повернення в 1 після N кроків.
%
%  Modes (meaningful):
%    1) (++Succs, -Tour)  Succs — список наступників, Tour — замкнений тур.
%
%  Modes (not meaningful):
%    (-Succs, ?)  Succs визначається tsp_clpfd, не генерується тут.
%
/** <examples>
?- succs_to_tour([2,3,1], Tour).
% очікується: Tour = [1,2,3,1].

?- succs_to_tour([2,1], Tour).
% очікується: Tour = [1,2,1].
*/
succs_to_tour(Succs, Tour) :-
    must_be(list, Succs),
    length(Succs, N),
    walk(1, N, Succs, Visited),
    Tour = [1 | Visited].

%  walk(+Current, +StepsLeft, +Succs, -Visited) is det.
%  Обхід: StepsLeft кроків, додаємо Succs[Current] до Visited.
walk(_Current, 0, _Succs, []).
walk(Current, K, Succs, [Next | Rest]) :-
    K > 0,
    nth1(Current, Succs, Next),
    K1 is K - 1,
    walk(Next, K1, Succs, Rest).

%% build_cost_constraints(++N, ++Flat, ++Succs, --Cost) is det.
%  Purpose:
%    Створює зв'язок між Succs і Cost через element/3.
%    Для кожного i (1..N): Idx #= (i-1)*N + Succs[i], element(Idx, Flat, Di).
%    Cost #= sum(Di).
%
%  Modes (meaningful):
%    1) (++N, ++Flat, ++Succs, -Cost)  N, Flat, Succs задані, Cost — FD-змінна.
%
%  Modes (not meaningful):
%    (-N, ?, ?, ?)  N обов'язковий.
%
%  Notes / built-ins:
%    - element/3: element(Idx, Flat, Di) — Di = Flat[Idx] (1-based); працює з FD Idx.
%    - (#=)/2: арифметична рівність в CLP(FD).
%    - ins/2: домен FD-змінних (Succs ins 1..N).
%    - sum/3: sum(Ds, #=, Cost) — Cost = сума елементів Ds.
%
/** <examples>
?- N=3, Flat=[0,2,9,2,0,6,9,6,0], length(Succs,3), Succs=[2,3,1],
   Succs ins 1..3, build_cost_constraints(N, Flat, Succs, Cost), label([Cost]).
% очікується: Cost = 17.

?- build_cost_constraints(2, [0,1,1,0], [2,1], Cost), label([Cost]).
% очікується: Cost = 2.
*/
build_cost_constraints(N, Flat, Succs, Cost) :-
    must_be(integer, N),
    must_be(list, Flat),
    must_be(list, Succs),
    numlist(1, N, Indices),
    maplist(edge_cost_i(N, Flat, Succs), Indices, Ds),
    sum(Ds, #=, Cost).

%  edge_cost_i(++N, ++Flat, ++Succs, ++I, -Di) is det.
%  Di = Flat[(I-1)*N + Succs[I]] — відстань від міста I до Succs[I].
edge_cost_i(N, Flat, Succs, I, Di) :-
    nth1(I, Succs, Si),
    Idx #= (I - 1) * N + Si,
    element(Idx, Flat, Di).
