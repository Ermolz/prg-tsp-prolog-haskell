% io_Yermolovych.pl
% Модуль I/O: читання інстансів *.tsp, валідація матриць відстаней.
% Відповідає за парсинг файлів і перевірку коректності даних.

:- module(io_Yermolovych, [read_instance_file/3, validate_matrix/2]).

%% read_instance_file(+Path, -N, -Matrix) is semidet.
%  Purpose:
%    Зчитує інстанс TSP з файлу Path і повертає розмір N і матрицю відстаней Matrix.
%    Ігнорує рядки з #, перша значуща строка — N, далі N рядків по N цілих чисел.
%
%  Modes (meaningful):
%    1) (++Path, -N, -Matrix)  Path — шлях до *.tsp; N, Matrix — вихідні.
%
%  Modes (not meaningful):
%    (-Path, ?, ?)  Немає сенсу без заданого шляху.
%    (++Path, ++N, ++Matrix)  Не перезаписує вхідні змінні.
%
%  Notes / built-ins:
%    - setup_call_cleanup/3: гарантує закриття потоку.
%    - read_line_to_string/2: читання рядків.
%    - split_string/4: розбиття на числа.
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

%  read_instance_stream(+Stream, -N, -Matrix) is semidet.
%  Читає N з першого значущого рядка, далі N рядків матриці.
read_instance_stream(Stream, N, Matrix) :-
    read_first_significant(Stream, NStr),
    number_string(N, NStr),
    N > 0,
    read_n_matrix_rows(Stream, N, Matrix).

%  read_first_significant(+Stream, -Line) is semidet.
%  Пропускає коментарі (#) і порожні рядки, повертає перший значущий.
read_first_significant(Stream, Line) :-
    read_line_to_string(Stream, L),
    (   L == end_of_file
    ->  fail
    ;   skip_line(L)
    ->  read_first_significant(Stream, Line)
    ;   Line = L
    ).

%  skip_line(+Line) is semidet.
%  Істина, якщо Line — коментар, порожній або end_of_file.
skip_line(Line) :-
    (   Line == end_of_file
    ->  true
    ;   Line == ""
    ->  true
    ;   sub_string(Line, 0, 1, _, "#")
    ->  true
    ;   false
    ).

%  read_n_matrix_rows(+Stream, +N, -Matrix) is det.
%  Читає N рядків, кожен — N цілих чисел.
read_n_matrix_rows(Stream, N, Matrix) :-
    length(Matrix, N),
    maplist(read_matrix_row(Stream, N), Matrix).

%  read_matrix_row(+Stream, +N, -Row) is det.
%  Читає один значущий рядок і парсить як N цілих.
read_matrix_row(Stream, N, Row) :-
    read_next_significant(Stream, Line),
    split_string(Line, " \t", " \t", Parts),
    maplist(number_string, Row, Parts),
    length(Row, N).

%  read_next_significant(+Stream, -Line) is semidet.
%  Як read_first_significant — повертає наступний значущий рядок.
read_next_significant(Stream, Line) :-
    read_line_to_string(Stream, L),
    (   L == end_of_file
    ->  fail
    ;   skip_line(L)
    ->  read_next_significant(Stream, Line)
    ;   Line = L
    ).

%% validate_matrix(+N, +Matrix) is semidet.
%  Purpose:
%    Перевіряє, що Matrix є коректною матрицею відстаней N×N:
%    N рядків по N цілих невід'ємних чисел, діагональ 0.
%
%  Modes (meaningful):
%    1) (++N, ++Matrix)  N — ціле, Matrix — список рядків.
%
%  Modes (not meaningful):
%    (-N, ?)  Без N не можна перевірити розмірність.
%
/** <examples>
?- validate_matrix(2, [[0,1],[1,0]]).
% очікується: true.

?- validate_matrix(2, [[0,1],[1]]).
% очікується: false (неправильна структура).

?- validate_matrix(2, [[0,-1],[1,0]]).
% очікується: false (від'ємна відстань).

?- validate_matrix(2, [[1,1],[1,0]]).
% очікується: false (діагональ не 0).
*/
validate_matrix(N, Matrix) :-
    must_be(integer, N),
    must_be(list, Matrix),
    length(Matrix, N),
    maplist(validate_row(N), Matrix),
    check_diagonal_zero(Matrix).

%  validate_row(+N, +Row) is semidet.
%  Рядок має N цілих невід'ємних елементів.
validate_row(N, Row) :-
    must_be(list, Row),
    length(Row, N),
    maplist(nonneg_int, Row).

%  nonneg_int(+X) is semidet.
%  X — ціле число >= 0.
nonneg_int(X) :-
    integer(X),
    X >= 0.

%  check_diagonal_zero(+Matrix) is semidet.
%  Усі діагональні елементи дорівнюють 0.
check_diagonal_zero(Matrix) :-
    length(Matrix, N),
    numlist(1, N, Indices),
    maplist(diag_zero_i(Matrix), Indices).

%  diag_zero_i(+Matrix, +I) is semidet.
%  Елемент (I,I) матриці дорівнює 0.
diag_zero_i(Matrix, I) :-
    nth1(I, Matrix, Row),
    nth1(I, Row, 0).
