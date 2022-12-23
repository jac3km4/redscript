<#
    Build the executable, copy required files and create a Nexus-style mod archive.
#>
param (
    [Parameter(Mandatory=$true)]
    [string]$stagingDir,
    
    [Parameter(Mandatory=$true)]
    [string]$archiveName
)

cargo build --release --features mmap,popup

$toolsDir = @($stagingDir, 'engine', 'tools') -join [IO.Path]::DirectorySeparatorChar

mkdir $stagingDir
cp -r ./resources/mod/* $stagingDir
mkdir -p $toolsDir
cp ./target/release/scc.exe $toolsDir

cd $stagingDir
7z a -mx=9 -r $archiveName *

# this variable is exported for the CI job to upload artifacts
if (Test-Path env:GITHUB_ENV) {
    echo "MOD_ARTIFACT_PATH=$($(Resolve-Path $archiveName) -replace '\\', '\\')" >> $env:GITHUB_ENV
}
