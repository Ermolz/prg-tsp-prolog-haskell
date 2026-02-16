# TSP: Prolog ↔ Haskell

## Постановка задачі

Знайти найкоротший замкнутий маршрут, що проходить через N міст рівно один раз і повертається до старту. Дано матрицю відстаней N*N. Потрібно знайти мінімальну вартість та відповідний тур.

## Формат вводу (*.tsp)

- Рядки з `#` — коментарі (ігноруються).
- Перша значуща строка: `N` (ціле число).
- Далі `N` рядків по `N` цілих чисел (відстані між містами).
- Діагональ 0, міста індексуються 1..N.

## Формат виводу

Одна строка (однаковий для Prolog і Haskell):

```
cost=<ціле> tour=[1,2,...,1]
```

Без пробілів у списку туру.

## Алгоритми

- **Prolog bruteforce** — generate & test: `permutation/2`, `tour_cost/3`, `aggregate_all(min/2)`.
- **Prolog CLP(FD)** — декларативні обмеження: `Succs`, `circuit/1`, `element/3`, `labeling` з `min(Cost)`.
- **Haskell bruteforce** — явний перебір: `permutations`, `tourCost`, `minimumBy`.

## Запуск Prolog

Потрібен **SWI-Prolog**. Команди з кореня репозиторію:

```bash
swipl src/prolog/main_Yermolovych.pl --algo bruteforce --file data/instances/n5_demo.tsp -g main -t halt
```

```bash
swipl src/prolog/main_Yermolovych.pl --algo clpfd --file data/instances/n5_demo.tsp -g main -t halt
```

## Запуск Haskell

Потрібен **Stack**:

```bash
cd src/haskell && stack build && cd ../..
```

```bash
cd src/haskell && stack run -- --algo bruteforce --file ../../data/instances/n5_demo.tsp && cd ../..
```

## Benchmarks

Команди відтворення замірів:

```bash
./scripts/bench_prolog.sh
./scripts/bench_haskell.sh
```

Агрегована таблиця (час у мс, мін. з 3 прогонів):

| N  | Prolog brute | Prolog CLP(FD) | Haskell brute | cost |
|----|--------------|----------------|---------------|------|
| 5  | 320          | 400            | 1750          | 21   |
| 8  | 370          | 570            | 1170          | 37   |
| 10 | 6270         | 2100           | 1340          | 53   |
| 12 | —            | 60540          | —             | 78   |

Bruteforce для N=12 не вимірювався через тривалість виконання.

Детально: [docs/benchmarks.md](docs/benchmarks.md)

## Документація

- [docs/benchmarks.md](docs/benchmarks.md) — методика та спостереження
- [docs/compare_prolog_haskell.md](docs/compare_prolog_haskell.md) — порівняння підходів