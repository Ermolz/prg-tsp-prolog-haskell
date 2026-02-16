# Run from repository root in PowerShell:
#   powershell -ExecutionPolicy Bypass -File scripts/bench_haskell.ps1
# Appends Haskell results to results/benchmarks.csv (creates header if missing)

$ErrorActionPreference = "Stop"

$Root = (Resolve-Path (Join-Path $PSScriptRoot "..")).Path
Set-Location $Root

$Csv = Join-Path $Root "results\benchmarks.csv"
New-Item -ItemType Directory -Force (Join-Path $Root "results") | Out-Null

if (-not (Test-Path $Csv)) {
  "algo,lang,instance,n,cost,time_ms,tour" | Set-Content -Encoding UTF8 $Csv
}

# Haskell bruteforce is factorial -> keep small/medium only
$Instances = @(
  @{ inst="n5_demo"; n=5 },
  @{ inst="n8_01";  n=8 },
  @{ inst="n10_01"; n=10 }
)

Push-Location "src/haskell"

foreach ($x in $Instances) {
  $inst = $x.inst
  $n    = $x.n
  $file = "../../data/instances/$inst.tsp"

  Write-Host "[haskell] algo=bruteforce inst=$inst n=$n ..."

  $sw = [System.Diagnostics.Stopwatch]::StartNew()
  $output = & stack run -- --algo bruteforce --file $file 2>&1
  $sw.Stop()

  $resultLine = ($output | Select-String -Pattern "cost=" | Select-Object -First 1).Line
  if (-not $resultLine) { throw "No result line found for haskell $inst" }

  if ($resultLine -match "cost=([0-9]+)\s+tour=(\[[0-9,\s]+\])") {
    $cost = $matches[1]
    $tour = ($matches[2] -replace "\s","")
  } else {
    throw "Unexpected output format: $resultLine"
  }

  $timeMs = [math]::Round($sw.Elapsed.TotalMilliseconds)
  "bruteforce,haskell,$inst,$n,$cost,$timeMs,$tour" | Add-Content -Encoding UTF8 $Csv
}

Pop-Location

Write-Host "Haskell benchmarks written to $Csv"
