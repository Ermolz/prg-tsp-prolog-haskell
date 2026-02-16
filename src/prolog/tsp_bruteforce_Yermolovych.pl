% ПІБ: Yermolovych Zakhar Maksymovych
% tsp_bruteforce_Yermolovych.pl
% Модуль brute-force TSP: точний перебір усіх перестановок міст 2..N через permutation/2.
% Тур замкнутий: [1,...,1]. Для вибору мінімуму використовується aggregate_all/3.

:- module(tsp_bruteforce_Yermolovych, [tsp_bruteforce/3]).

:- use_module(library(lists)).        % permutation/2, numlist/3, append/3
:- use_module(library(aggregate)).    % aggregate_all/3
:- use_module(tsp_common_Yermolovych, [tour_cost/3]).

/* ------------------------------------------------------------------------
   tsp_bruteforce/3
   ------------------------------------------------------------------------ */

%% tsp_bruteforce(++Matrix, --Tour, --Cost) is semidet.
%  Purpose:
%    Обчислює оптимальний замкнутий TSP-тур, який починається і закінчується у місті 1,
%    та його мінімальну вартість.
%
%  Ідея:
%    1) Фіксується старт/фініш у місті 1.
%    2) Перебираються всі перестановки міст 2..N (факторіальна кількість).
%    3) Для кожного кандидата рахується tour_cost/3.
%    4) aggregate_all/3 вибирає розв'язок з мінімальною вартістю.
%
%  Індикатори / призначення (змістовні):
%    1) (++Matrix, --Tour, --Cost)  Matrix задана, Tour і Cost повертаються як оптимум.
%
%  Інші комбінації (пояснення відсутності змістовних призначень):
%    - (-Matrix, ?, ?)        Немає сенсу генерувати матрицю для перебору.
%    - (++Matrix, ++Tour, ?)  Не використовується як “перевірка туру”, бо задача — знайти оптимум.
%    - (++Matrix, ?, ++Cost)  Перевірка на заданий Cost не є ціллю цього предикату (оптимізація).
%
%  Неочевидні built-ins:
%    - must_be/2: перевіряє тип/конкретизацію аргументів; при порушенні кидає помилку.
%    - aggregate_all/3: агрегує всі розв'язки Goal і повертає мінімум без збереження всіх кандидатів.
%    - min/2 у aggregate_all: шаблон min(Cost, Witness) для пошуку мінімальної вартості.
%
/** <examples>
?- tsp_bruteforce([[0,2,9],[2,0,6],[9,6,0]], Tour, Cost).
% Tour = [1,2,3,1],
% Cost = 17.

?- tsp_bruteforce([[0,1],[1,0]], T, C).
% T = [1,2,1],
% C = 2.

?- tsp_bruteforce(M, T, C).
% ERROR: Type error: `list' expected, found _G... (an unbound variable)
*/
% tsp_bruteforce(++Matrix, --Tour, --Cost).
tsp_bruteforce(Matrix, Tour, Cost) :-
    must_be(list, Matrix),
    length(Matrix, N),
    N >= 1,
    (   N =:= 1
    ->  Tour = [1, 1],
        Cost = 0
    ;   aggregate_all(
            min(Cost0, Tour0),
            candidate_tour(Matrix, N, Tour0, Cost0),
            min(Cost, Tour)
        )
    ).

/* ------------------------------------------------------------------------
   candidate_tour/4
   ------------------------------------------------------------------------ */

%% candidate_tour(++Matrix, ++N, --Tour, --Cost) is nondet.
%  Purpose:
%    Генерує всі кандидатні замкнені тури (через перестановки міст 2..N)
%    та їхню вартість. Використовується як Goal для мінімізації у tsp_bruteforce/3.
%
%  Індикатори / призначення (змістовні):
%    1) (++Matrix, ++N, --Tour, --Cost)  Генератор (nondet) турів і вартостей.
%
%  Інші комбінації (пояснення відсутності змістовних призначень):
%    - (++, -N, ?, ?)     N має бути відомим (розмірність), не генерується тут.
%    - (-Matrix, ?, ?, ?) Немає сенсу генерувати матрицю.
%
%  Неочевидні built-ins:
%    - permutation/2: недетерміновано генерує всі перестановки списку.
%    - numlist/3: будує список цілих [2..N] (список міст без 1).
%
/** <examples>
?- candidate_tour([[0,2,9],[2,0,6],[9,6,0]], 3, Tour, Cost).
% Tour = [1,2,3,1],
% Cost = 17 ;
% Tour = [1,3,2,1],
% Cost = 17 ;
% false.

?- findall(C, candidate_tour([[0,1],[1,0]], 2, _, C), Cs).
% Cs = [2].
*/
% candidate_tour(++Matrix, ++N, --Tour, --Cost).
candidate_tour(Matrix, N, Tour, Cost) :-
    must_be(list, Matrix),
    must_be(integer, N),
    N >= 2,
    numlist(2, N, Cities),
    permutation(Cities, Perm),
    build_tour(Perm, Tour),
    tour_cost(Matrix, Tour, Cost).

/* ------------------------------------------------------------------------
   build_tour/2
   ------------------------------------------------------------------------ */

%% build_tour(++Perm, --Tour) is det.
%  Purpose:
%    Будує замкнутий тур шляхом додавання 1 на початок і 1 в кінець:
%    Tour = [1|Perm] ++ [1].
%
%  Індикатори / призначення (змістовні):
%    1) (++Perm, --Tour)  Perm заданий, Tour повертається.
%
%  Інші комбінації (пояснення відсутності змістовних призначень):
%    - (-Perm, ?)  Генерація Perm не є задачею цього предикату.
%
%  Неочевидні built-ins:
%    - append/3: конкатенація списків.
%
/** <examples>
?- build_tour([2,3], Tour).
% Tour = [1,2,3,1].

?- build_tour([], Tour).
% Tour = [1,1].
*/
% build_tour(++Perm, --Tour).
build_tour(Perm, Tour) :-
    must_be(list, Perm),
    append([1], Perm, Temp),
    append(Temp, [1], Tour).
