#!/usr/bin/env bash
# Build the executable, copy required files and create a Nexus-style mod archive.
set -e

if [ $# -ne 2 ]; then
    echo "Usage: $0 <staging_dir> <archive_name>"
    exit 1
fi

staging_dir="$1"
archive_name="$2"

working_dir=$(pwd)

cargo build --release --features mmap,popup

tools_dir="$staging_dir/engine/tools"

mkdir -p "$tools_dir"

cp ./target/release/scc "$tools_dir"
if [[ "$OSTYPE" == "darwin"* ]]; then
    cp -r ./resources/mod/macos/* "$staging_dir"
    cp ./target/release/libscc_lib.dylib "$tools_dir"
    cp ./target/release/redscript-cli "$working_dir/redscript-cli-aarch64-darwin"
elif [[ "$OSTYPE" = "linux-gnu"* ]]; then
    cp ./target/release/libscc_lib.so "$tools_dir"
    cp ./target/release/redscript-cli "$working_dir/redscript-cli-x86_64-linux-gnu"
fi

cd "$staging_dir"
zip -r "$working_dir/$archive_name" *
