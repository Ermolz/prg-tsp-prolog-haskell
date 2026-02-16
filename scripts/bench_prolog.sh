#!/bin/bash
# Run from repository root. Measures Prolog bruteforce (small N) and clpfd (bigger N).
# Writes results to results/benchmarks.csv

set -e
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

CSV="$ROOT/results/benchmarks.csv"
echo "algo,lang,instance,n,cost,time_ms,tour" > "$CSV"

# Bruteforce is factorial -> keep only small instances here
INSTANCES_BRUTE="n5_demo:5 n8_01:8 n10_01:10"
# CLP(FD) can be tried on larger instances
INSTANCES_CLP="n5_demo:5 n8_01:8 n10_01:10 n12_01:12"

run_one() {
  local algo="$1"
  local inst="$2"
  local n="$3"
  local file="data/instances/${inst}.tsp"

  echo "[prolog] algo=${algo} inst=${inst} n=${n} ..."

  output=$(/usr/bin/time -f "%e" swipl -q -s src/prolog/main_Yermolovych.pl -g main -t halt -- --algo "$algo" --file "$file" 2>&1)

  result_line=$(echo "$output" | grep 'cost=' | head -1)
  time_sec=$(echo "$output" | grep -E '^[0-9]+(\.[0-9]+)?$' | tail -1)

  cost=$(echo "$result_line" | sed -n 's/.*cost=\([0-9]*\).*/\1/p')
  tour=$(echo "$result_line" | sed -n 's/.*tour=\(\[.*\]\)/\1/p')
  time_ms=$(echo "$time_sec" | awk '{printf "%.0f", $1*1000}')

  echo "${algo},prolog,${inst},${n},${cost},${time_ms},${tour}" >> "$CSV"
}

# bruteforce (small only)
for entry in $INSTANCES_BRUTE; do
  inst="${entry%%:*}"
  n="${entry##*:}"
  run_one "bruteforce" "$inst" "$n"
done

# clpfd (try bigger)
for entry in $INSTANCES_CLP; do
  inst="${entry%%:*}"
  n="${entry##*:}"
  run_one "clpfd" "$inst" "$n"
done

echo "Prolog benchmarks written to $CSV"
