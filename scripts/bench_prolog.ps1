# Run from repository root in PowerShell:
#   powershell -ExecutionPolicy Bypass -File scripts/bench_prolog.ps1
# Writes results to results/benchmarks.csv

$ErrorActionPreference = "Stop"

$Root = (Resolve-Path (Join-Path $PSScriptRoot "..")).Path
Set-Location $Root

$Csv = Join-Path $Root "results\benchmarks.csv"
New-Item -ItemType Directory -Force (Join-Path $Root "results") | Out-Null

"algo,lang,instance,n,cost,time_ms,tour" | Set-Content -Encoding UTF8 $Csv

# Bruteforce is factorial -> keep small only
$InstancesBrute = @(
  @{ inst="n5_demo"; n=5 },
  @{ inst="n8_01";  n=8 },
  @{ inst="n10_01"; n=10 }
)

# CLP(FD) can be tried on larger
$InstancesClp = @(
  @{ inst="n5_demo"; n=5 },
  @{ inst="n8_01";  n=8 },
  @{ inst="n10_01"; n=10 },
  @{ inst="n12_01"; n=12 }
)

$MainPl = "src/prolog/main_Yermolovych.pl"

function Run-One($algo, $inst, $n) {
  $file = "data/instances/$inst.tsp"

  Write-Host "[prolog] algo=$algo inst=$inst n=$n ..."

  $sw = [System.Diagnostics.Stopwatch]::StartNew()
  # NOTE: -- separates SWI-Prolog args from your program args in your main/0
  $output = & swipl -q -s $MainPl -g main -t halt -- --algo $algo --file $file 2>&1
  $sw.Stop()

  $resultLine = ($output | Select-String -Pattern "cost=" | Select-Object -First 1).Line
  if (-not $resultLine) { throw "No result line found for $algo $inst" }

  if ($resultLine -match "cost=([0-9]+)\s+tour=(\[[0-9,\s]+\])") {
    $cost = $matches[1]
    $tour = ($matches[2] -replace "\s","") # прибрати пробіли
  } else {
    throw "Unexpected output format: $resultLine"
  }

  $timeMs = [math]::Round($sw.Elapsed.TotalMilliseconds)
  "$algo,prolog,$inst,$n,$cost,$timeMs,$tour" | Add-Content -Encoding UTF8 $Csv
}

foreach ($x in $InstancesBrute) { Run-One "bruteforce" $x.inst $x.n }
foreach ($x in $InstancesClp)   { Run-One "clpfd"      $x.inst $x.n }

Write-Host "Prolog benchmarks written to $Csv"