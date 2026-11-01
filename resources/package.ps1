<#
    Build the executable, copy required files and create a Nexus-style mod archive.
#>
param (
    [Parameter(Mandatory=$true)]
    [string]$stagingDir,

    [Parameter(Mandatory=$true)]
    [string]$archiveName
)

$ErrorActionPreference = 'Stop'
$workingDir = (Get-Location).Path

if ($archiveName -cnotmatch '\A[A-Za-z0-9][A-Za-z0-9._-]*\.zip\z') {
    throw 'Archive name must be a simple .zip filename.'
}
New-Item -ItemType Directory -Path $stagingDir -Force | Out-Null
if (Get-ChildItem -LiteralPath $stagingDir -Force) {
    throw 'Staging directory must be empty.'
}

$targetDir = Join-Path $workingDir 'target'
$releaseDir = Join-Path $targetDir 'x86_64-pc-windows-msvc/release'
$toolsDir = Join-Path $stagingDir 'engine/tools'
$archivePath = Join-Path $workingDir $archiveName

cargo build --release --locked --target x86_64-pc-windows-msvc --target-dir $targetDir -p scc -p scc-lib -p redscript-cli --features mmap,popup
if ($LASTEXITCODE -ne 0) {
    throw "cargo build failed with exit code $LASTEXITCODE"
}

Copy-Item -Recurse -Path './resources/mod/windows/*' -Destination $stagingDir
New-Item -ItemType Directory -Path $toolsDir -Force | Out-Null
Copy-Item -LiteralPath "$releaseDir/scc.exe", "$releaseDir/scc_lib.dll" -Destination $toolsDir
Copy-Item -LiteralPath "$releaseDir/redscript-cli.exe" -Destination $workingDir

if (Test-Path -LiteralPath $archivePath) {
    [IO.File]::Delete($archivePath)
}
Push-Location -LiteralPath $stagingDir
try {
    7z a -mx=5 -r $archivePath '*'
    if ($LASTEXITCODE -ne 0) {
        throw "7z failed with exit code $LASTEXITCODE"
    }
} finally {
    Pop-Location
}
