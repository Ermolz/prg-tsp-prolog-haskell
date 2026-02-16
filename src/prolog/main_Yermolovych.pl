% ПІБ: Yermolovych Zakhar Maksymovych
% main_Yermolovych.pl
% CLI для задачі TSP: парсинг аргументів, читання інстансу, запуск алгоритму, вивід результату.
% Формат успішного виводу: cost=<int> tour=[1,...,1]

:- module(main_Yermolovych, [main/0]).

:- use_module(io_Yermolovych,            [read_instance_file/3, validate_matrix/2]).
:- use_module(tsp_common_Yermolovych,    [format_result/3]).
:- use_module(tsp_bruteforce_Yermolovych,[tsp_bruteforce/3]).
:- use_module(tsp_clpfd_Yermolovych,     [tsp_clpfd/3]).

% ------------------------------------------------------------
% 1) Точка входу CLI
% ------------------------------------------------------------

%% main is det.
%  Індикатори: ( )
%  Призначення (змістовні):
%    1) main — точка входу CLI: читає argv, парсить --algo і --file, запускає run_file/2.
%  Відсутні призначення:
%    Інших немає (аргументи беруться з current_prolog_flag(argv,...)).
%
%  Неочевидні built-ins:
%    - current_prolog_flag(argv, Argv): повертає список аргументів командного рядка (після `--`).
%    - halt/1: завершення процесу з кодом (0 — успіх, 1 — помилка).
%
/** <examples>
% Запуск з консолі (WSL/Linux):
% $ swipl -q -s src/prolog/main_Yermolovych.pl -g main -t halt -- --algo bruteforce --file data/instances/n5_demo.tsp
% очікується: cost=<int> tour=[...]
%
% $ swipl -q -s src/prolog/main_Yermolovych.pl -g main -t halt -- --algo clpfd --file data/instances/n8_01.tsp
% очікується: cost=<int> tour=[...]
*/
main :-
    current_prolog_flag(argv, Argv),
    (   parse_args(Argv, Algo, File)
    ->  run_file(Algo, File)
    ;   usage_msg(Msg),
        writeln(Msg),
        halt(1)
    ).

% ------------------------------------------------------------
% 2) Повідомлення usage
% ------------------------------------------------------------

%% usage_msg(-Msg) is det.
%  Індикатори: (--Msg)
%  Призначення (змістовні):
%    1) (--Msg) — повернути рядок інструкції використання CLI.
%  Відсутні призначення:
%    (++Msg) — не має сенсу як “перевірка”, бо Msg константний.
usage_msg('Usage: swipl -q -s src/prolog/main_Yermolovych.pl -g main -t halt -- --algo bruteforce|clpfd --file <path>.').

% ------------------------------------------------------------
% 3) Парсинг аргументів
% ------------------------------------------------------------

%% parse_args(+Argv, -Algo, -File) is semidet.
%  Індикатори: (++Argv, --Algo, --File)
%  Призначення (змістовні):
%    1) (++Argv, --Algo, --File) — витягнути --algo (bruteforce|clpfd) і --file <path>.
%  Відсутні призначення:
%    (-Argv, ?, ?) — без списку аргументів парсити нічого.
%    (++Argv, ++Algo, ++File) — не призначено для перевірки довільних значень Algo/File.
%
%  Пояснення:
%    Argv у SWI-Prolog зазвичай список атомів; File допускається атом або рядок.
%
/** <examples>
?- parse_args(['--algo', bruteforce, '--file', 'data/n5.tsp'], Algo, File).
% очікується: Algo = bruteforce, File = 'data/n5.tsp'.

?- parse_args(['--file', x, '--algo', clpfd], Algo, File).
% очікується: Algo = clpfd, File = x.

?- parse_args(['--algo', unknown], A, F).
% очікується: false.
*/
parse_args(Argv, Algo, File) :-
    must_be(list, Argv),
    arg_value(Argv, '--algo', AlgoRaw),
    arg_value(Argv, '--file', FileRaw),
    algo_atom(AlgoRaw, Algo),
    must_be(ground, FileRaw),
    \+ is_option(FileRaw),
    File = FileRaw.

%% algo_atom(+Raw, -Algo) is semidet.
%  Індикатори: (++Raw, --Algo)
%  Призначення (змістовні):
%    1) (++Raw, --Algo) — нормалізувати значення algo до атома bruteforce або clpfd.
%  Відсутні призначення:
%    (-Raw, ?) — не генерує можливі Raw.
algo_atom(Raw, bruteforce) :-
    raw_equals(Raw, bruteforce).
algo_atom(Raw, clpfd) :-
    raw_equals(Raw, clpfd).

%% raw_equals(+Raw, +Atom) is semidet.
%  Індикатори: (++Raw, ++Atom)
%  Призначення (змістовні):
%    1) (++Raw, ++Atom) — істина, якщо Raw (атом або рядок) відповідає Atom.
%  Неочевидні built-ins:
%    - atom_string/2: перетворення між атомом і рядком.
raw_equals(Raw, Atom) :-
    (   atom(Raw)
    ->  Raw == Atom
    ;   string(Raw)
    ->  atom_string(Atom, Raw)
    ).

%% arg_value(+Argv, +Opt, -Val) is semidet.
%  Індикатори: (++Argv, ++Opt, --Val)
%  Призначення (змістовні):
%    1) (++Argv, ++Opt, --Val) — знайти Val одразу після Opt у списку Argv.
%  Відсутні призначення:
%    (?, ?, ++Val) — не використовується для пошуку “яка опція перед Val”.
%  Неочевидні built-ins:
%    - append/3: використовується для пошуку підсписку [Opt, Val | _].
arg_value(Argv, Opt, Val) :-
    append(_, [Opt, Val | _], Argv).

%% is_option(+X) is semidet.
%  Індикатори: (++X)
%  Призначення (змістовні):
%    1) (++X) — істина, якщо X виглядає як опція '--...'.
%  Відсутні призначення:
%    (-X) — не генерує опції.
%  Неочевидні built-ins:
%    - sub_string/5: перевірка префікса у рядка.
%    - atom_concat/3: перевірка префікса у атома.
is_option(X) :-
    (   atom(X)
    ->  atom_concat('--', _, X)
    ;   string(X)
    ->  sub_string(X, 0, 2, _, "--")
    ).

% ------------------------------------------------------------
% 4) Вибір алгоритму
% ------------------------------------------------------------

%% solve(+Algo, +Matrix, -Tour, -Cost) is semidet.
%  Індикатори: (++Algo, ++Matrix, --Tour, --Cost)
%  Призначення (змістовні):
%    1) (++Algo, ++Matrix, --Tour, --Cost) — викликати алгоритм TSP відповідно до Algo.
%  Відсутні призначення:
%    (-Algo, ?, ?, ?) — Algo має бути відомий (bruteforce|clpfd).
solve(bruteforce, Matrix, Tour, Cost) :-
    tsp_bruteforce(Matrix, Tour, Cost).
solve(clpfd, Matrix, Tour, Cost) :-
    tsp_clpfd(Matrix, Tour, Cost).

% ------------------------------------------------------------
% 5) Повний прогін: read -> validate -> solve -> print
% ------------------------------------------------------------

%% run_file(+Algo, +File) is det.
%  Індикатори: (++Algo, ++File)
%  Призначення (змістовні):
%    1) (++Algo, ++File) — зчитати інстанс, перевірити, розв’язати, вивести 1 рядок результату.
%  Відсутні призначення:
%    (-Algo, ?) — Algo не генерується, а передається з parse_args/3.
%
%  Умови:
%    - На успіх: виводиться рівно 1 рядок формату cost=<int> tour=[...].
%    - На помилку: виводиться повідомлення і виконується halt(1).
%
/** <examples>
?- run_file(bruteforce, 'data/instances/n5_demo.tsp').
% очікується: cost=<int> tour=[1,...,1].

?- run_file(clpfd, 'data/instances/n8_01.tsp').
% очікується: cost=<int> tour=[1,...,1].
*/
run_file(Algo, File) :-
    must_be(atom, Algo),
    must_be(ground, File),
    (   read_instance_file(File, N, Matrix)
    ->  (   validate_matrix(N, Matrix)
        ->  (   solve(Algo, Matrix, Tour, Cost)
            ->  format_result(Tour, Cost, Line),
                writeln(Line),
                halt(0)
            ;   writeln('Error: failed to solve instance.'),
                halt(1)
            )
        ;   writeln('Error: invalid matrix in instance file.'),
            halt(1)
        )
    ;   writeln('Error: failed to read instance file.'),
        halt(1)
    ).
