#!/bin/bash
# Run from repository root. Measures Haskell bruteforce on small instances.
# Appends to results/benchmarks.csv (creates header if missing)

set -e
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

CSV="$ROOT/results/benchmarks.csv"
if [ ! -f "$CSV" ]; then
  echo "algo,lang,instance,n,cost,time_ms,tour" > "$CSV"
fi

# Haskell bruteforce is also factorial -> keep small/medium only
INSTANCES="n5_demo:5 n8_01:8 n10_01:10"

cd src/haskell
for entry in $INSTANCES; do
  inst="${entry%%:*}"
  n="${entry##*:}"
  file="../../data/instances/${inst}.tsp"

  echo "[haskell] algo=bruteforce inst=${inst} n=${n} ..."

  output=$(/usr/bin/time -f "%e" stack run -- --algo bruteforce --file "$file" 2>&1)

  result_line=$(echo "$output" | grep 'cost=' | head -1)
  time_sec=$(echo "$output" | grep -E '^[0-9]+(\.[0-9]+)?$' | tail -1)

  cost=$(echo "$result_line" | sed -n 's/.*cost=\([0-9]*\).*/\1/p')
  tour=$(echo "$result_line" | sed -n 's/.*tour=\(\[.*\]\)/\1/p')
  time_ms=$(echo "$time_sec" | awk '{printf "%.0f", $1*1000}')

  echo "bruteforce,haskell,${inst},${n},${cost},${time_ms},${tour}" >> "$ROOT/results/benchmarks.csv"
done
cd "$ROOT"

echo "Haskell benchmarks written to $CSV"
