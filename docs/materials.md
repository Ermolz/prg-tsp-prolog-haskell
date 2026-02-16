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

- **Стаття/публікація про NP-complete для Euclidean TSP**
  - Використано для: підтвердження складності саме евклідових інстансів (актуально, якщо згадується формат з координатами або евклідова інтерпретація).
  - Де відображено: `docs/compare_prolog_haskell.md` або `README.md` (якщо згадується Euclidean TSP).

---

## B) Prolog / SWI-Prolog (CLP(FD), предикати)

### B1. CLP(FD) та глобальні обмеження
- **SWI-Prolog: `library(clpfd)` (офіційна документація)**
  - Використано для:
    - `circuit/1` — модель гамільтонового циклу через successor-представлення;
    - `element/3` — зв’язок “індекс → елемент” для підрахунку вартості по сплющеній матриці;
    - `labeling/2` — пошук з оптимізацією через `min(Expr)` та опціями вибору змінних.
  - Де відображено:
    - код: `src/prolog/tsp_clpfd_Yermolovych.pl`
    - пояснення: `docs/compare_prolog_haskell.md`

### B2. Перестановки (brute force)
- **SWI-Prolog: `library(lists)` — `permutation/2`**
  - Використано для: генерації всіх перестановок у brute force підході та пояснення факторіального росту.
  - Де відображено:
    - код: `src/prolog/tsp_bruteforce_Yermolovych.pl`
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

### C2. Відтворюваність збірки
- **Stack documentation (resolver / snapshots)**
  - Використано для: пояснення, чому обрано Stack і чому важливо фіксувати resolver в `stack.yaml`.
  - Де відображено: `README.md` (збірка/запуск Haskell).

### C3. Метрики виконання
- **GHC RTS `+RTS -s`**
  - Може бути використано для: пояснення метрик алокацій/GC.
  - Де відображено: `docs/benchmarks.md`.

---

## D) Евристики (опційний додатковий підхід)

- **Nearest Neighbor heuristic**
  - Використано/може бути використано для: побудови стартового туру.
- **2-opt**
  - Використано/може бути використано для: локального покращення туру.
- **PAAL / довідка про 2-opt та оцінку O(n²) на ітерацію**
  - Використано/може бути використано для: формальнішого пояснення складності кроку покращення.

Де відображено: `docs/compare_prolog_haskell.md`.

---

## E) Датасети / інстанси

### E1. Локальні інстанси
- **`data/instances/*.tsp`**
  - Використано для: демонстрації та бенчмарків.
  - Інстанси є фіксованими і зберігаються у репозиторії для відтворюваності.
  
---

## Посилання (URL)
- Wikipedia: Travelling salesman problem — https://en.wikipedia.org/wiki/Travelling_salesman_problem  
- MIT 18.404 матеріали — https://ocw.mit.edu/courses/18-404j-theory-of-computation-fall-2020/pages/lecture-notes/
- SWI-Prolog: library(clpfd) — https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/clpfd.html%27)
- SWI-Prolog: library(lists), permutation/2 — https://www.swi-prolog.org/pldoc/doc_for?object=permutation/2  
- Hackage: Data.List.permutations — https://hackage.haskell.org/package/base/docs/Data-List.html#v:permutations  
- Stack documentation — https://docs.haskellstack.org/  
- Criterion — https://hackage.haskell.org/package/criterion  
- TSPLIB — http://comopt.ifi.uni-heidelberg.de/software/TSPLIB/
