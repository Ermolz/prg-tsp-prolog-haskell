# Матеріали та джерела

Цей файл містить перелік використаних джерел і пояснення, **для чого саме** вони застосовані в роботі (теорія, бібліотеки, інструменти), а також **де саме** це відображено у репозиторії.

---

## A) Теорія TSP (визначення, складність)

### A1. Визначення задачі та загальна довідка
- **Wikipedia — Travelling salesman problem**
  - Використано для: короткого визначення TSP, огляду типових підходів (точні методи, евристики).
  - Де відображено: `README.md` (Постановка задачі), `docs/compare_prolog_haskell.md` (контекст задачі).

### A2. NP-складність / NP-повнота
- **MIT 18.404 / матеріали з теорії складності**
  - Використано для: формулювання decision-версії TSP і твердження про NP-повноту (як теоретичне обґрунтування “факторіальний вибух” для повного перебору).
  - Де відображено: `README.md` (коротка теорія), `docs/benchmarks.md` (пояснення росту часу).

- **Стаття/публікація про NP-complete для Euclidean TSP (ScienceDirect або інше рецензоване джерело)**
  - Використано для: підтвердження складності саме евклідових інстансів (актуально, якщо згадується формат з координатами або евклідова інтерпретація).
  - Де відображено: (опційно) `docs/compare_prolog_haskell.md` або `README.md` (якщо згадується Euclidean TSP).

---

## B) Prolog / SWI-Prolog (CLP(FD), предикати)

### B1. CLP(FD) та глобальні обмеження
- **SWI-Prolog: `library(clpfd)` (офіційна документація)**
  - Використано для:
    - `circuit/1` — модель гамільтонового циклу через successor-представлення;
    - `element/3` — зв’язок “індекс → елемент” для підрахунку вартості по сплющеній матриці;
    - `labeling/2` — пошук з оптимізацією через `min(Expr)` та опціями вибору змінних.
  - Де відображено:
    - код: `src/prolog/tsp_clpfd_<SURNAME>.pl`
    - пояснення: `docs/compare_prolog_haskell.md`

### B2. Перестановки (brute force)
- **SWI-Prolog: `library(lists)` — `permutation/2`**
  - Використано для: генерації всіх перестановок у brute force підході та пояснення факторіального росту.
  - Де відображено:
    - код: `src/prolog/tsp_bruteforce_<SURNAME>.pl`
    - пояснення: `docs/benchmarks.md`

### B3. Додаткове про CLP(FD) (опційно, “авторитетно”)
- **Markus Triska — матеріали про CLP(FD) у SWI-Prolog**
  - Використано для: більш формального пояснення принципів constraint propagation і ролі `labeling/2`.
  - Де відображено: `docs/compare_prolog_haskell.md` (секція про CLP(FD)).

---

## C) Haskell (перестановки, бенчмарки, збірка)

### C1. Перестановки у стандартній бібліотеці
- **Hackage: `Data.List.permutations`**
  - Використано для: реалізації brute force в Haskell як “спільного базового підходу” для чесного порівняння.
  - Де відображено:
    - код: `src/haskell/src/TSP/BruteForce.hs`
    - пояснення: `docs/compare_prolog_haskell.md`

### C2. Бенчмаркінг (опційно)
- **Hackage: `criterion`**
  - Може бути використано для: стабільніших вимірювань часу (якщо додано окремий bench-модуль).
  - Де відображено: (опційно) `src/haskell/bench/`.

### C3. Відтворюваність збірки
- **Stack documentation (resolver / snapshots)**
  - Використано для: пояснення, чому обрано Stack і чому важливо фіксувати resolver в `stack.yaml`.
  - Де відображено: `README.md` (збірка/запуск Haskell).

### C4. Опційно: метрики виконання
- **GHC RTS `+RTS -s`**
  - Може бути використано для: пояснення метрик алокацій/GC (якщо метрики додані в аналіз).
  - Де відображено: (опційно) `docs/benchmarks.md`.

---

## D) Евристики (опційний додатковий підхід)

> Евристики не є обов’язковими для цього репозиторію, якщо реалізовано 2 підходи в Prolog (brute force + CLP(FD)).
> Водночас вони корисні для демонстрації масштабування на великих N (без гарантії оптимуму).

- **Nearest Neighbor heuristic**
  - Використано/може бути використано для: побудови стартового туру.
- **2-opt**
  - Використано/може бути використано для: локального покращення туру.
- **PAAL / довідка про 2-opt та оцінку O(n²) на ітерацію**
  - Використано/може бути використано для: формальнішого пояснення складності кроку покращення.

Де відображено: (опційно) `docs/compare_prolog_haskell.md` або окремий `docs/heuristics.md`.

---

## E) Датасети / інстанси

### E1. Локальні інстанси
- **`data/instances/*.tsp`**
  - Використано для: демонстрації та бенчмарків.
  - Інстанси є фіксованими і зберігаються у репозиторії для відтворюваності.

### E2. TSPLIB (опційно)
- **TSPLIB**
  - Може бути використано для: порівняння з “класичними” інстансами TSP.
  - Зауваження: формат TSPLIB не збігається з форматом `*.tsp` у цьому репозиторії; потрібен конвертер.

---

## Примітка про посилання
Усі посилання на джерела зберігаються у вигляді URL у кінці цього файлу або у відповідних документах (`README.md`, `docs/*.md`).
Рекомендація: при оформленні фінальної версії додати прямі посилання (URL) у секцію “Посилання” нижче.

---

## Посилання (URL)
- Wikipedia: Travelling salesman problem — https://en.wikipedia.org/wiki/Travelling_salesman_problem  
- SWI-Prolog: library(clpfd) — https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/clpfd.html%27)   ???
- SWI-Prolog: library(lists), permutation/2 — https://www.swi-prolog.org/pldoc/doc_for?object=permutation/2  
- Hackage: Data.List.permutations — https://hackage.haskell.org/package/base/docs/Data-List.html#v:permutations  
- Stack documentation — https://docs.haskellstack.org/  
- Criterion — https://hackage.haskell.org/package/criterion  
- TSPLIB — http://comopt.ifi.uni-heidelberg.de/software/TSPLIB/


- (опційно) MIT 18.404 матеріали — <додати URL>
- (опційно) Euclidean TSP NP-complete (ScienceDirect/ін.) — <додати URL>
