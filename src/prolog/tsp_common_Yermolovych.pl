% tsp_common_Yermolovych.pl
% Модуль загальних утиліт TSP: обчислення вартості туру, перетворення матриць,
% перевірка валідності туру, форматування результату.
% Використовується tsp_bruteforce_Yermolovych, tsp_clpfd_Yermolovych і main_Yermolovych.

:- module(tsp_common_Yermolovych, [tour_cost/3, is_valid_tour/2, matrix_to_flat/2, format_result/3]).

%% tour_cost(++Matrix, +Tour, --Cost) is semidet.
%  Purpose:
%    Обчислює загальну вартість замкнутого туру Tour за матрицею відстаней Matrix.
%    Вартість = сума Matrix[From][To] по сусідніх парах у Tour. Якщо Tour невалідний — fail.
%
%  Modes (meaningful):
%    1) (++Matrix, +Tour, -Cost)  Matrix і Tour задані, Cost — результат.
%
%  Modes (not meaningful):
%    (-Matrix, ?, ?)  Matrix обов'язкова для обчислення.
%    (?, -Tour, -Cost)  Tour визначається алгоритмом, не цим предикатом.
%
%  Notes / built-ins:
%    - nth1/3: доступ до елемента матриці — nth1(From, Matrix, Row), nth1(To, Row, Dist).
%    - sum_list/2: підсумовування списку вартостей.
%
/** <examples>
?- tour_cost([[0,1],[1,0]], [1,2,1], Cost).
% очікується: Cost = 2.

?- tour_cost([[0,2,9],[2,0,6],[9,6,0]], [1,2,3,1], Cost).
% очікується: Cost = 17.

?- tour_cost([[0,1],[1,0]], [1,2,3,1], Cost).
% очікується: false (невалідний тур).
*/
tour_cost(Matrix, Tour, Cost) :-
    must_be(list, Matrix),
    must_be(list, Tour),
    length(Matrix, N),
    is_valid_tour(N, Tour),
    tour_pairs(Tour, Pairs),
    maplist(pair_cost(Matrix), Pairs, Costs),
    sum_list(Costs, Cost).

%  tour_pairs(+Tour, -Pairs) is semidet.
%  Pairs — список пар (From,To) для сусідніх міст у замкненому турі.
tour_pairs([A, B | Rest], [(A, B) | Pairs]) :-
    tour_pairs([B | Rest], Pairs).
tour_pairs([_], []).

%  pair_cost(++Matrix, +Pair, -Dist) is det.
%  Dist = Matrix[From][To] для Pair = (From, To). nth1/3: індексація з 1.
pair_cost(Matrix, (From, To), Dist) :-
    nth1(From, Matrix, Row),
    nth1(To, Row, Dist).

%% is_valid_tour(++N, +Tour) is semidet.
%  Purpose:
%    Перевіряє валідність замкнутого туру: length = N+1, починається і закінчується 1,
%    усі міста 2..N зустрічаються рівно один раз у середині (без повторів і пропусків).
%    Не генерує Tour, лише перевірка.
%
%  Modes (meaningful):
%    1) (++N, +Tour)  Обидва задані, перевірка валідності.
%
%  Modes (not meaningful):
%    (-N, ?)  N обов'язковий для перевірки довжини.
%    (++, -Tour)  Предикат лише перевіряє, не генерує.
%
/** <examples>
?- is_valid_tour(5, [1,2,3,4,5,1]).
% очікується: true.

?- is_valid_tour(5, [1,2,4,5,3,1]).
% очікується: true.

?- is_valid_tour(5, [1,2,2,4,5,1]).
% очікується: false (повтор 2).

?- is_valid_tour(5, [1,2,4,5,1]).
% очікується: false (пропущено 3, length /= 6).
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

%  Notes: msort/2 — сортує список, зберігаючи дублікати; для перевірки множини
%  {2..N} без повторів msort дає [2,3,...,N] тільки якщо Middle — перестановка.

%% matrix_to_flat(++Matrix, --Flat) is det.
%  Purpose:
%    Сплющує матрицю N×N у список довжини N*N (рядок за рядком).
%    Для використання з CLP(FD) element/3.
%
%  Modes (meaningful):
%    1) (++Matrix, -Flat)  Matrix — список рядків, Flat — сплющений список.
%
%  Modes (not meaningful):
%    (-Matrix, ?)  Matrix є вхідним даним.
%
%  Notes / built-ins:
%    - append/2: конкатенує список списків у один список.
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

%% format_result(+Tour, +Cost, --Line) is det.
%  Purpose:
%    Повертає рядок формату: cost=<int> tour=[1,...,1].
%    У списку tour немає пробілів між числами (приклад: [1,2,3,1]).
%
%  Modes (meaningful):
%    1) (++Tour, ++Cost, -Line)  Tour і Cost задані, Line — результат.
%
%  Modes (not meaningful):
%    (?, ?, ++Line)  Парсинг рядка не реалізовано.
%
%  Notes / built-ins:
%    - atomics_to_string/3: об'єднує атоми/числа в рядок з роздільником.
%    - format/3: форматує рядок.
%
/** <examples>
?- format_result([1,2,4,5,3,1], 17, Line).
% очікується: Line = "cost=17 tour=[1,2,4,5,3,1]".

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
