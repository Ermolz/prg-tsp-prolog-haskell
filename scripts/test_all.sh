#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

echo "== 1) Перевірка, що модулі Prolog завантажуються без warning/error =="

check_module() {
  local m="$1"
  local out
  out="$(swipl -q -g "use_module('$m'), halt" 2>&1 || true)"
  if [[ -n "$out" ]]; then
    echo "[FAIL] $m"
    echo "$out"
    exit 1
  fi
  echo "[OK] $m"
}

check_module "src/prolog/io_Yermolovych"
check_module "src/prolog/tsp_common_Yermolovych"
check_module "src/prolog/tsp_bruteforce_Yermolovych"
check_module "src/prolog/tsp_clpfd_Yermolovych"
check_module "src/prolog/main_Yermolovych"

echo
echo "== 2) Прогін main для всіх інстансів (bruteforce і clpfd де адекватно) =="

run_main() {
  local algo="$1"
  local file="$2"
  local line
  line="$(swipl -q -s src/prolog/main_Yermolovych.pl -g main -t halt -- --algo "$algo" --file "$file")"
  echo "$algo $file -> $line"
  # формат: cost=число tour=[...]
  if ! echo "$line" | grep -Eq '^cost=[0-9]+ tour=\[[0-9,]+\]$'; then
    echo "[FAIL] Unexpected output format"
    exit 1
  fi
}

# bruteforce: n5, n8 (n10/n12 можуть бути дуже повільні)
run_main bruteforce "data/instances/n5_demo.tsp"
run_main bruteforce "data/instances/n8_01.tsp"

# clpfd: всі
run_main clpfd "data/instances/n5_demo.tsp"
run_main clpfd "data/instances/n8_01.tsp"
run_main clpfd "data/instances/n10_01.tsp"
run_main clpfd "data/instances/n12_01.tsp"

echo
echo "== 3) Перевірка збігу вартості bruteforce vs clpfd (n5, n8) =="

get_cost() {
  local algo="$1"
  local file="$2"
  swipl -q -s src/prolog/main_Yermolovych.pl -g main -t halt -- --algo "$algo" --file "$file" \
    | sed -n 's/^cost=\([0-9]\+\) .*/\1/p'
}

for f in "data/instances/n5_demo.tsp" "data/instances/n8_01.tsp"; do
  c1="$(get_cost bruteforce "$f")"
  c2="$(get_cost clpfd "$f")"
  echo "$f: bruteforce=$c1 clpfd=$c2"
  if [[ "$c1" != "$c2" ]]; then
    echo "[FAIL] Costs differ for $f"
    exit 1
  fi
done

echo
echo "== 4) Швидка перевірка доків: перша особа / російські літери (грубо) =="

# перша особа (укр+рус)
if rg -n --hidden --no-ignore -S '\b(я|ми|мені|мене|мною|нам|наш|мої|сделал|сделано|я сделал)\b' README.md docs 2>/dev/null; then
  echo "[WARN] Знайдено можливу першу особу — перевірити вручну."
else
  echo "[OK] Перша особа (грубий пошук) не знайдена."
fi

# типові рос. літери (не гарантує 100%, але ловить частину)
if rg -n --hidden --no-ignore -S '[ёыэъ]' README.md docs 2>/dev/null; then
  echo "[WARN] Знайдено символи, не типові для укр — перевірити вручну."
else
  echo "[OK] Символи ё/ы/э/ъ не знайдені."
fi

echo
echo "[ALL OK] Базова перевірка пройдена."
