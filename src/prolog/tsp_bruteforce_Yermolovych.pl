% tsp_bruteforce_Yermolovych.pl
% Модуль brute-force TSP: точний перебір усіх перестановок міст 2..N через permutation/2.
% Тур замкнутий: [1,...,1]. Використовує aggregate_all/3 для ефективного пошуку мінімуму.

:- module(tsp_bruteforce_Yermolovych, [tsp_bruteforce/3]).

:- use_module(library(lists)).
:- use_module(library(aggregate)).
:- use_module(tsp_common_Yermolovych, [tour_cost/3]).

%% tsp_bruteforce(++Matrix, --Tour, --Cost) is semidet.
%  Purpose:
%    Знаходить оптимальний замкнутий тур (початок і кінець у місті 1) і його мінімальну вартість.
%    Перебирає всі перестановки міст 2..N, замикає цикл через 1, обчислює вартість і повертає
%    одне найкраще рішення. Не зберігає всі кандидати — використовує aggregate_all(min/2).
%
%  Modes (meaningful):
%    1) (++Matrix, -Tour, -Cost)  Matrix задана, Tour і Cost — результат (оптимум).
%
%  Modes (not meaningful):
%    (-Matrix, ?, ?)  Matrix обов'язкова (вхідні дані).
%    (++Matrix, ++Tour, ++Cost)  Предикат генерує рішення, не перевіряє.
%
%  Notes / built-ins:
%    - aggregate_all/3: збирає всі розв'язки Goal і агрегує за шаблоном min(Cost, Witness),
%      повертаючи мінімальну вартість і відповідний тур без збереження всіх кандидатів.
%
/** <examples>
?- tsp_bruteforce([[0,2,9],[2,0,6],[9,6,0]], Tour, Cost).
% очікується: Tour = [1,2,3,1], Cost = 17 (або інший оптимум).

?- tsp_bruteforce([[0,1],[1,0]], T, C).
% очікується: T = [1,2,1], C = 2.

?- tsp_bruteforce(M, T, C).
% очікується: instantiation_error або must_be.
*/
tsp_bruteforce(Matrix, Tour, Cost) :-
    must_be(list, Matrix),
    length(Matrix, N),
    (   N =:= 1
    ->  Tour = [1, 1],
        Cost = 0
    ;   aggregate_all(
            min(Cost0, Tour0),
            candidate_tour(Matrix, N, Tour0, Cost0),
            min(Cost, Tour)
        )
    ).

%% candidate_tour(++Matrix, ++N, --Tour, --Cost) is nondet.
%  Purpose:
%    Генерує кандидатні замкнені тури і їхню вартість (для мінімізації).
%    Для кожної перестановки міст [2..N] будує тур [1|Perm]++[1] і обчислює вартість.
%
%  Modes (meaningful):
%    1) (++Matrix, ++N, -Tour, -Cost)  Matrix і N задані, Tour і Cost — вихідні (nondet).
%
%  Modes (not meaningful):
%    (-Matrix, ?, ?, ?)  Matrix обов'язкова.
%    (++, -N, ?, ?)  N обчислюється з Matrix зовні, не генерується тут.
%
%  Notes / built-ins:
%    - permutation/2: генерує перестановки списку (nondet). Використовується для перебору
%      порядку відвідування міст 2..N.
%
/** <examples>
?- candidate_tour([[0,2,9],[2,0,6],[9,6,0]], 3, Tour, Cost).
% очікується: множина розв'язків, напр. Tour=[1,2,3,1], Cost=17; Tour=[1,3,2,1], Cost=17; ...

?- findall(C, candidate_tour([[0,1],[1,0]], 2, _, C), Cs).
% очікується: Cs = [2] (єдина перестановка [2]).
*/
candidate_tour(Matrix, N, Tour, Cost) :-
    must_be(list, Matrix),
    must_be(integer, N),
    numlist(2, N, Cities),
    permutation(Cities, Perm),
    build_tour(Perm, Tour),
    tour_cost(Matrix, Tour, Cost).

%% build_tour(+Perm, --Tour) is det.
%  Purpose:
%    Будує замкнутий тур: додає 1 на початок і 1 в кінець.
%    Tour = [1|Perm] ++ [1], тобто [1, P1, P2, ..., Pk, 1].
%
%  Modes (meaningful):
%    1) (++Perm, -Tour)  Perm — порядок міст 2..N, Tour — замкнений тур.
%
%  Modes (not meaningful):
%    (-Perm, ?)  Perm генерується candidate_tour, не цією функцією.
%
%  Notes / built-ins:
%    - append/3: append([1], Perm, Temp), append(Temp, [1], Tour) — конкатенує списки.
%
/** <examples>
?- build_tour([2,3], Tour).
% очікується: Tour = [1,2,3,1].

?- build_tour([], Tour).
% очікується: Tour = [1,1] (N=1, лише місто 1).
*/
build_tour(Perm, Tour) :-
    must_be(list, Perm),
    append([1], Perm, Temp),
    append(Temp, [1], Tour).
