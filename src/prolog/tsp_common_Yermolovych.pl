% ПІБ: Yermolovych Zakhar Maksymovych
% tsp_common_Yermolovych.pl
% Загальні утиліти для TSP:
% - обчислення вартості туру;
% - перевірка валідності туру;
% - сплющення матриці (для CLP(FD) element/3);
% - форматування результату (рядок cost=... tour=[...]).

:- module(tsp_common_Yermolovych,
          [ tour_cost/3
          , is_valid_tour/2
          , matrix_to_flat/2
          , format_result/3
          ]).

:- use_module(library(lists)).  % append/2, append/3, sum_list/2, msort/2

/* ------------------------------------------------------------------------
   tour_cost/3
   ------------------------------------------------------------------------ */

%% tour_cost(++Matrix, +Tour, --Cost) is semidet.
%  Purpose:
%    Обчислює загальну вартість замкнутого туру Tour за матрицею відстаней Matrix.
%    Вартість = сума Matrix[From][To] для сусідніх пар (From,To) у Tour.
%    Якщо Tour невалідний відносно N = length(Matrix) — fail.
%
%  Індикатори (змістовні):
%    1) (++Matrix, +Tour, -Cost)  обчислення вартості за заданими Matrix і Tour.
%    2) (++Matrix, +Tour, +Cost)  перевірка: чи дорівнює вартість заданому Cost.
%
%  Відсутність інших змістовних призначень:
%    - (-Matrix, ?, ?)  немає сенсу без матриці.
%    - (++, -Tour, ?)   породження Tour не належить цьому предикату (це роблять алгоритми).
%
%  Неочевидні вбудовані:
%    - nth1/3: доступ до елемента за 1-based індексацією.
%    - sum_list/2: сума елементів списку.
%
/** <examples>
?- tour_cost([[0,1],[1,0]], [1,2,1], Cost).
% очікується: Cost = 2.

?- tour_cost([[0,2,9],[2,0,6],[9,6,0]], [1,2,3,1], Cost).
% очікується: Cost = 17.

?- tour_cost([[0,1],[1,0]], [1,2,3,1], Cost).
% очікується: false (невалідний тур).

?- tour_cost([[0,1],[1,0]], [1,2,1], 2).
% очікується: true.
*/
tour_cost(Matrix, Tour, Cost) :-
    must_be(list, Matrix),
    must_be(list, Tour),
    length(Matrix, N),
    is_valid_tour(N, Tour),
    tour_pairs(Tour, Pairs),
    maplist(pair_cost(Matrix), Pairs, Costs),
    sum_list(Costs, Cost).

/* ------------------------------------------------------------------------
   tour_pairs/2
   ------------------------------------------------------------------------ */

%% tour_pairs(+Tour, --Pairs) is semidet.
%  Purpose:
%    Перетворює список міст Tour у список сусідніх пар (From,To).
%    Напр.: [1,2,3,1] -> [(1,2),(2,3),(3,1)].
%
%  Індикатори (змістовні):
%    1) (+Tour, -Pairs)  побудова пар із заданого туру.
%
%  Відсутність інших змістовних призначень:
%    - (-Tour, ?)  породження Tour не є метою.
%
/** <examples>
?- tour_pairs([1,2,3,1], Pairs).
% очікується: Pairs = [(1,2),(2,3),(3,1)].
*/
tour_pairs([A, B | Rest], [(A, B) | Pairs]) :-
    tour_pairs([B | Rest], Pairs).
tour_pairs([_], []).

/* ------------------------------------------------------------------------
   pair_cost/3
   ------------------------------------------------------------------------ */

%% pair_cost(++Matrix, +Pair, --Dist) is det.
%  Purpose:
%    Dist = Matrix[From][To] для Pair = (From,To).
%
%  Індикатори (змістовні):
%    1) (++Matrix, +Pair, -Dist)  отримання відстані.
%    2) (++Matrix, +Pair, +Dist)  перевірка: чи збігається Dist з Matrix[From][To].
%
%  Неочевидні вбудовані:
%    - nth1/3: індексація з 1.
%
/** <examples>
?- pair_cost([[0,1],[1,0]], (1,2), D).
% очікується: D = 1.

?- pair_cost([[0,1],[1,0]], (2,1), 1).
% очікується: true.
*/
pair_cost(Matrix, (From, To), Dist) :-
    nth1(From, Matrix, Row),
    nth1(To, Row, Dist).

/* ------------------------------------------------------------------------
   is_valid_tour/2
   ------------------------------------------------------------------------ */

%% is_valid_tour(++N, +Tour) is semidet.
%  Purpose:
%    Перевіряє валідність замкнутого туру:
%    - length(Tour) = N+1
%    - перший і останній елемент = 1
%    - середина містить рівно один раз кожне місто з 2..N
%      (без повторів і пропусків).
%
%  Індикатори (змістовні):
%    1) (++N, +Tour)  перевірка валідності.
%
%  Відсутність інших змістовних призначень:
%    - (-N, ?)  N потрібен для перевірки довжини та множини міст.
%    - (++, -Tour)  породження туру не є метою цього предикату.
%
%  Неочевидні вбудовані:
%    - msort/2: сортує список, ЗБЕРІГАЄ дублікати; корисно для перевірки,
%      що Middle є перестановкою Expected без повторів.
%    - append/3: відокремлення останнього елемента (1) від решти.
%
/** <examples>
?- is_valid_tour(5, [1,2,3,4,5,1]).
% очікується: true.

?- is_valid_tour(5, [1,2,4,5,3,1]).
% очікується: true.

?- is_valid_tour(5, [1,2,2,4,5,1]).
% очікується: false (повтор 2).

?- is_valid_tour(5, [1,2,4,5,1]).
% очікується: false (довжина не N+1).
*/
is_valid_tour(N, Tour) :-
    must_be(integer, N),
    must_be(list, Tour),
    length(Tour, Len),
    Len =:= N + 1,
    Tour = [1 | Rest],
    append(Middle, [1], Rest),
    numlist(2, N, Expected),
    msort(Middle, Sorted),
    Sorted == Expected.

/* ------------------------------------------------------------------------
   matrix_to_flat/2
   ------------------------------------------------------------------------ */

%% matrix_to_flat(++Matrix, --Flat) is det.
%  Purpose:
%    Сплющує матрицю (список рядків) у один список (рядок за рядком).
%    Використовується у CLP(FD) разом з element/3.
%
%  Індикатори (змістовні):
%    1) (++Matrix, -Flat)  сплющення заданої матриці.
%    2) (++Matrix, +Flat)  перевірка: чи є Flat сплющенням Matrix.
%
%  Відсутність інших змістовних призначень:
%    - (-Matrix, ?)  породження матриці не є метою.
%
%  Неочевидні вбудовані:
%    - append/2: конкатенує список списків у список.
%
/** <examples>
?- matrix_to_flat([[0,1],[2,3]], Flat).
% очікується: Flat = [0,1,2,3].

?- matrix_to_flat([[0]], F).
% очікується: F = [0].
*/
matrix_to_flat(Matrix, Flat) :-
    must_be(list, Matrix),
    append(Matrix, Flat).

/* ------------------------------------------------------------------------
   format_result/3
   ------------------------------------------------------------------------ */

%% format_result(+Tour, +Cost, --Line) is det.
%  Purpose:
%    Формує рядок результату у форматі:
%      "cost=<int> tour=[1,2,...,1]"
%    Без пробілів у списку tour.
%
%  Індикатори (змістовні):
%    1) (+Tour, +Cost, -Line)  побудова рядка.
%    2) (+Tour, +Cost, +Line)  перевірка: чи збігається Line з очікуваним форматом.
%
%  Відсутність інших змістовних призначень:
%    - (?, ?, -Line)  породження Tour/Cost не належить цьому предикату.
%    - (?, ?, +Line)  парсинг рядка назад не реалізовано.
%
%  Неочевидні вбудовані:
%    - atomics_to_string/3: об'єднує елементи списку у рядок через роздільник.
%    - format/3: форматує рядок (string(Line)).
%
/** <examples>
?- format_result([1,2,4,5,3,1], 21, Line).
% очікується: Line = "cost=21 tour=[1,2,4,5,3,1]".

?- format_result([1,2,1], 2, L).
% очікується: L = "cost=2 tour=[1,2,1]".
*/
format_result(Tour, Cost, Line) :-
    must_be(list, Tour),
    must_be(integer, Cost),
    atomics_to_string(Tour, ',', TourInner),
    string_concat('[', TourInner, S1),
    string_concat(S1, ']', TourStr),
    format(string(Line), 'cost=~d tour=~s', [Cost, TourStr]).
