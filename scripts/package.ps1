param (
    [Parameter(Mandatory=$true)]
    [string]$archiveName
)

$ErrorActionPreference = 'Stop'

if (-not (Test-Path "./target/release/scc.exe")) {
    Write-Error "scc.exe not found in target/release. Please ensure you have built it."
    exit 1
}

$workingDir = (Get-Location).Path
if ($env:RUNNER_TEMP) {
    $tempDir = $env:RUNNER_TEMP
} else {
    $tempDir = $env:TEMP
}
$stagingDir = Join-Path $tempDir "redscript-archive"
$toolsDir = @($stagingDir, 'engine', 'tools') -join [IO.Path]::DirectorySeparatorChar

if (Test-Path $stagingDir) {
    Remove-Item -Recurse -Force $stagingDir
}
New-Item -ItemType Directory -Path $toolsDir | Out-Null

Copy-Item -Path "./assets/windows/archive/*" -Destination $stagingDir -Recurse -Force
Copy-Item -Path "./target/release/scc.exe" -Destination $toolsDir
Copy-Item -Path "./target/release/scc_lib.dll" -Destination $toolsDir
Copy-Item -Path "./target/release/redscript-cli.exe" -Destination $workingDir

Push-Location $stagingDir
try {
    Compress-Archive -Path "./*" -DestinationPath "$workingDir/$archiveName" -Force
} finally {
    Pop-Location
}
