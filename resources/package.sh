#!/bin/bash
# Build the executable, copy required files and create a Nexus-style mod archive.
set -e

if [ $# -ne 2 ]; then
    echo "Usage: $0 <staging_dir> <archive_name>"
    exit 1
fi

staging_dir="$1"
archive_name="$2"

cargo build --release --features mmap,popup

tools_dir="$staging_dir/engine/tools"

mkdir -p "$staging_dir"
cp -r ./resources/mod/* "$staging_dir"

mkdir -p "$tools_dir"

cp ./target/release/scc "$tools_dir"
if [ -f ./target/release/libscc_lib.dylib ]; then
    cp ./target/release/libscc_lib.dylib "$tools_dir"
fi
if [ -f ./target/release/libscc_lib.so ]; then
    cp ./target/release/libscc_lib.so "$tools_dir"
fi

cd "$staging_dir"
zip -r "$archive_name" *

# Export variable for CI job to upload artifacts
if [ -n "$GITHUB_ENV" ]; then
    echo "MOD_ARTIFACT_PATH=$(realpath "$archive_name")" >> "$GITHUB_ENV"
fi
