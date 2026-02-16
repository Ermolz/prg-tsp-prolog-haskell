% main_Yermolovych.pl
% Модуль CLI: парсинг аргументів --algo bruteforce|clpfd, --file <path>,
% запуск відповідного алгоритму, вивід cost=... tour=[...].

:- module(main_Yermolovych, [main/0]).

:- use_module(io_Yermolovych, [read_instance_file/3, validate_matrix/2]).
:- use_module(tsp_common_Yermolovych, [format_result/3]).
:- use_module(tsp_bruteforce_Yermolovych, [tsp_bruteforce/3]).
:- use_module(tsp_clpfd_Yermolovych, [tsp_clpfd/3]).

%  Note: Модулі io_Yermolovych, tsp_common_Yermolovych, tsp_bruteforce_Yermolovych, tsp_clpfd_Yermolovych
%  мають бути в тому ж каталозі (src/prolog/) або в пошуковому шляху.

%% main is det.
%  Purpose:
%    Точка входу CLI: зчитує argv, парсить --algo і --file, викликає run_file/2.
%    Якщо аргументів бракує або algo невідомий — виводить usage і halt(1).
%
%  Modes (meaningful):
%    1) main  Викликається без аргументів (argv з current_prolog_flag).
%
%  Modes (not meaningful):
%    Немає аргументів.
%
%  Notes / built-ins:
%    - current_prolog_flag(argv, Argv): Argv — список аргументів командного рядка
%      (без імені скрипта; SWI-Prolog передає лише аргументи після імені файлу).
%    - halt(1): завершення процесу з кодом помилки 1.
%
/** <examples>
?- main.
% при argv = [--algo, bruteforce, --file, 'data/instances/n5_demo.tsp']:
%   очікується: cost=17 tour=[1,2,4,5,3,1] (або інший оптимум).

% при argv = [--algo, unknown] або відсутній --file:
%   очікується: Usage: ..., потім halt(1).
*/
main :-
    current_prolog_flag(argv, Argv),
    (   parse_args(Argv, Algo, File)
    ->  run_file(Algo, File)
    ;   usage_msg(Msg),
        writeln(Msg),
        halt(1)
    ).

%  usage_msg(-Msg) is det.
%  Msg — рядок інструкції використання CLI.
usage_msg('Usage: swipl main_Yermolovych.pl --algo bruteforce|clpfd --file <path>').

%% parse_args(+Argv, -Algo, -File) is semidet.
%  Purpose:
%    Парсить список аргументів Argv, витягує --algo (bruteforce|clpfd) і --file <path>.
%    Простий ручний парсер без сторонніх бібліотек.
%
%  Modes (meaningful):
%    1) (++Argv, -Algo, -File)  Argv — список атомів/рядків, Algo і File — вихідні.
%
%  Modes (not meaningful):
%    (-Argv, ?, ?)  Argv обов'язковий (вхід).
%    (++, ++Algo, ++File)  Предикат лише парсить, не перевіряє задані значення.
%
%  Notes:
%    - Argv: список атомів (SWI-Prolog argv). Шлях File — атом або рядок.
%    - Алго має бути bruteforce або clpfd; інакше parse_args fail.
%
/** <examples>
?- parse_args([--algo, bruteforce, --file, 'data/n5.tsp'], Algo, File).
% очікується: Algo = bruteforce, File = 'data/n5.tsp'.

?- parse_args([--file, x.tsp, --algo, clpfd], Algo, File).
% очікується: Algo = clpfd, File = x.tsp.

?- parse_args([--algo, unknown], A, F).
% очікується: false.
*/
parse_args(Argv, Algo, File) :-
    must_be(list, Argv),
    arg_value(Argv, '--algo', AlgoRaw),
    arg_value(Argv, '--file', File),
    algo_atom(AlgoRaw, Algo),
    ground(File),
    \+ is_option(File).

%  algo_atom(+Raw, -Algo) is semidet.
%  Raw — атом або рядок з argv; Algo — канонічний атом bruteforce або clpfd.
algo_atom(Raw, bruteforce) :-
    (   Raw = bruteforce
    ;   Raw = 'bruteforce'
    ;   string(Raw), Raw == "bruteforce"
    ).
algo_atom(Raw, clpfd) :-
    (   Raw = clpfd
    ;   Raw = 'clpfd'
    ;   string(Raw), Raw == "clpfd"
    ).

%  arg_value(+Argv, +Opt, -Val) is semidet.
%  Val — елемент списку Argv, що йде одразу після Opt.
arg_value(Argv, Opt, Val) :-
    append(_, [Opt, Val | _], Argv).

%  is_option(+X) is semidet.
%  Істина, якщо X — опція (починається з --).
is_option(X) :-
    atom(X),
    atom_concat('--', _, X).

%% solve(+Algo, +Matrix, -Tour, -Cost) is semidet.
%  Purpose:
%    Викликає відповідний TSP-алгоритм за Algo: bruteforce -> tsp_bruteforce/3,
%    clpfd -> tsp_clpfd/3.
%
%  Modes (meaningful):
%    1) (++Algo, ++Matrix, -Tour, -Cost)  Algo і Matrix задані, Tour і Cost — результат.
%
%  Modes (not meaningful):
%    (-Algo, ?, ?, ?)  Algo обов'язковий.
%    (++, -Matrix, ?, ?)  Matrix задається run_file, не генерується тут.
%
/** <examples>
?- solve(bruteforce, [[0,1],[1,0]], Tour, Cost).
% очікується: Tour = [1,2,1], Cost = 2.

?- solve(clpfd, [[0,1],[1,0]], Tour, Cost).
% очікується: Tour = [1,2,1], Cost = 2.
*/
solve(bruteforce, Matrix, Tour, Cost) :-
    tsp_bruteforce(Matrix, Tour, Cost).
solve(clpfd, Matrix, Tour, Cost) :-
    tsp_clpfd(Matrix, Tour, Cost).

%% run_file(+Algo, +File) is det.
%  Purpose:
%    Зчитує інстанс з File, валідує матрицю, вирішує TSP через solve/4,
%    форматує результат через format_result/3 і виводить один рядок на stdout.
%    У разі помилки (read fail, validate fail, solve fail) — halt(1).
%
%  Modes (meaningful):
%    1) (++Algo, ++File)  Algo — bruteforce|clpfd, File — шлях до *.tsp.
%
%  Modes (not meaningful):
%    (-Algo, ?)  Algo задається main, не генерується тут.
%
%  Notes:
%    - Вивід рівно 1 рядок: cost=<int> tour=[1,...,1].
%    - Якщо read_instance_file або validate_matrix або solve fail — writeln помилки, halt(1).
%
/** <examples>
?- run_file(bruteforce, 'data/instances/n5_demo.tsp').
% очікується: cost=17 tour=[1,2,4,5,3,1] (або інший оптимум).

?- run_file(clpfd, 'data/instances/n8_01.tsp').
% очікується: cost=<int> tour=[1,...,1].
*/
run_file(Algo, File) :-
    must_be(atom, Algo),
    must_be(ground, File),
    (   read_instance_file(File, N, Matrix),
        validate_matrix(N, Matrix),
        solve(Algo, Matrix, Tour, Cost)
    ->  format_result(Tour, Cost, Line),
        writeln(Line)
    ;   writeln('Error: failed to read, validate or solve instance.'),
        halt(1)
    ).
