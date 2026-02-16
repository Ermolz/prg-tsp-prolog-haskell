% ПІБ: Yermolovych Zakhar Maksymovych
% io_Yermolovych.pl
% Модуль I/O: читання інстансів *.tsp, валідація матриць відстаней.
% Формат *.tsp: рядки з # — коментарі, перша значуща строка — N, далі N рядків по N цілих.

:- module(io_Yermolovych, [read_instance_file/3, validate_matrix/2]).

%% read_instance_file(+Path, -N, -Matrix) is semidet.
%  Індикатори: (++Path, --N, --Matrix)
%  Призначення (змістовні):
%    1) (++Path, --N, --Matrix) — зчитати інстанс із файлу.
%  Відсутні призначення:
%    (-Path, ?, ?) — немає сенсу без заданого шляху.
%    (++Path, ++N, ++Matrix) — предикат не призначений для “перевірки” готових значень.
%
%  Неочевидні built-ins:
%    - setup_call_cleanup/3: гарантує закриття потоку.
%    - must_be/2: перевірка типу/зв’язку аргументів.
%
/** <examples>
?- read_instance_file('data/instances/n5_demo.tsp', N, M).
% очікується: N=5, M = [[0,2,9,10,7],[2,0,6,4,3],...].

?- read_instance_file('nonexistent.tsp', N, M).
% очікується: false.

?- read_instance_file(X, N, M).
% очікується: type_error(ground, X).
*/
read_instance_file(Path, N, Matrix) :-
    must_be(ground, Path),
    catch(
        setup_call_cleanup(
            open(Path, read, Stream),
            read_instance_stream(Stream, N, Matrix),
            close(Stream)
        ),
        _Error,
        fail
    ).

% read_instance_stream(+Stream, -N, -Matrix) is semidet.
% Індикатори: (++Stream, --N, --Matrix)
% Призначення (змістовні):
%   1) (++Stream, --N, --Matrix) — зчитати N і матрицю з потоку.
% Відсутні призначення:
%   (-Stream, ?, ?) — без потоку робота неможлива.
read_instance_stream(Stream, N, Matrix) :-
    read_first_significant(Stream, NLine),
    number_string(N, NLine),
    N > 0,
    read_n_matrix_rows(Stream, N, Matrix).

% read_first_significant(+Stream, -Line) is semidet.
% Індикатори: (++Stream, --Line)
% Призначення:
%   1) (++Stream, --Line) — повернути перший значущий (не коментар/порожній) рядок.
read_first_significant(Stream, Line) :-
    read_line_to_string(Stream, Raw),
    (   Raw == end_of_file
    ->  fail
    ;   normalize_space(string(Norm), Raw),
        skip_line(Norm)
    ->  read_first_significant(Stream, Line)
    ;   normalize_space(string(Line), Raw)
    ).

% skip_line(+Line) is semidet.
% Індикатори: (++Line)
% Призначення:
%   1) (++Line) — істина, якщо рядок порожній або коментар (починається з #).
% Відсутні призначення:
%   (-Line) — предикат не генерує рядки.
skip_line(Line) :-
    (   Line == end_of_file
    ->  true
    ;   Line == ""
    ->  true
    ;   sub_string(Line, 0, 1, _, "#")
    ->  true
    ;   false
    ).

% read_n_matrix_rows(+Stream, +N, -Matrix) is det.
% Індикатори: (++Stream, ++N, --Matrix)
% Призначення:
%   1) (++Stream, ++N, --Matrix) — зчитати N рядків матриці.
read_n_matrix_rows(Stream, N, Matrix) :-
    length(Matrix, N),
    maplist(read_matrix_row(Stream, N), Matrix).

% read_matrix_row(+Stream, +N, -Row) is det.
% Індикатори: (++Stream, ++N, --Row)
% Призначення:
%   1) (++Stream, ++N, --Row) — зчитати один рядок матриці з N чисел.
% Неочевидні built-ins:
%   - split_string/4: розбиття рядка на токени.
read_matrix_row(Stream, N, Row) :-
    read_next_significant(Stream, Line),
    split_string(Line, " \t", " \t", Parts),
    maplist(number_string, Row, Parts),
    length(Row, N).

% read_next_significant(+Stream, -Line) is semidet.
% Індикатори: (++Stream, --Line)
% Призначення:
%   1) (++Stream, --Line) — повернути наступний значущий рядок.
read_next_significant(Stream, Line) :-
    read_line_to_string(Stream, Raw),
    (   Raw == end_of_file
    ->  fail
    ;   normalize_space(string(Norm), Raw),
        skip_line(Norm)
    ->  read_next_significant(Stream, Line)
    ;   normalize_space(string(Line), Raw)
    ).

%% validate_matrix(+N, +Matrix) is semidet.
%  Індикатори: (++N, ++Matrix)
%  Призначення (змістовні):
%    1) (++N, ++Matrix) — перевірити, що Matrix є коректною матрицею N×N:
%       невід’ємні цілі, діагональ 0.
%  Відсутні призначення:
%    (-N, ?) — без N не можна перевірити розмірність.
%
/** <examples>
?- validate_matrix(2, [[0,1],[1,0]]).
% очікується: true.

?- validate_matrix(2, [[0,1],[1]]).
% очікується: false.

?- validate_matrix(2, [[0,-1],[1,0]]).
% очікується: false.

?- validate_matrix(2, [[1,1],[1,0]]).
% очікується: false.
*/
validate_matrix(N, Matrix) :-
    must_be(integer, N),
    must_be(list, Matrix),
    length(Matrix, N),
    maplist(validate_row(N), Matrix),
    check_diagonal_zero(Matrix).

% validate_row(+N, +Row) is semidet.
% Індикатори: (++N, ++Row)
% Призначення:
%   1) (++N, ++Row) — перевірити, що рядок має N елементів і всі невід’ємні цілі.
validate_row(N, Row) :-
    must_be(list, Row),
    length(Row, N),
    maplist(nonneg_int, Row).

% nonneg_int(+X) is semidet.
% Індикатори: (++X)
% Призначення:
%   1) (++X) — істина, якщо X — ціле число >= 0.
nonneg_int(X) :-
    integer(X),
    X >= 0.

% check_diagonal_zero(+Matrix) is semidet.
% Індикатори: (++Matrix)
% Призначення:
%   1) (++Matrix) — істина, якщо всі діагональні елементи матриці дорівнюють 0.
% Неочевидні built-ins:
%   - numlist/3: формує список індексів 1..N.
check_diagonal_zero(Matrix) :-
    length(Matrix, N),
    numlist(1, N, Indices),
    maplist(diag_zero_i(Matrix), Indices).

% diag_zero_i(+Matrix, +I) is semidet.
% Індикатори: (++Matrix, ++I)
% Призначення:
%   1) (++Matrix, ++I) — істина, якщо елемент (I,I) дорівнює 0.
% Неочевидні built-ins:
%   - nth1/3: доступ до елементів списку з індексацією з 1.
diag_zero_i(Matrix, I) :-
    nth1(I, Matrix, Row),
    nth1(I, Row, 0).
