<#
    Build the executable, copy required files and create a Nexus-style mod archive.
#>
param (
    [Parameter(Mandatory=$true)]
    [string]$stagingDir,

    [Parameter(Mandatory=$true)]
    [string]$archiveName
)

$workingDir = (Get-Location).Path

cargo build --release --features mmap,popup

$toolsDir = @($stagingDir, 'engine', 'tools') -join [IO.Path]::DirectorySeparatorChar

mkdir $stagingDir
cp -r ./resources/mod/* $stagingDir
mkdir -p $toolsDir
cp ./target/release/scc.exe $toolsDir
cp ./target/release/scc_lib.dll $toolsDir
cp ./target/release/redscript-cli.exe $workingDir

cd $stagingDir
7z a -mx=9 -r "$workingDir/$archiveName" *
