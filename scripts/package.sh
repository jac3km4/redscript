#!/usr/bin/env bash
# Build the executable, copy required files and create a Nexus-style mod archive.
set -euo pipefail

if [ $# -ne 1 ]; then
    echo "Usage: $0 <archive_name>"
    exit 1
fi

if [ ! -f "target/release/scc" ]; then
    echo "Error: scc not found in target/release. Please ensure you have built it." >&2
    exit 1
fi

archive_name="$1"
working_dir=$(pwd)
if [ -n "${RUNNER_TEMP:-}" ]; then
    temp_dir="$RUNNER_TEMP"
else
    temp_dir="${TMPDIR:-/tmp}"
fi
staging_dir="$temp_dir/redscript-archive"
tools_dir="$staging_dir/engine/tools"

if [ -d "$staging_dir" ]; then
    rm -rf "$staging_dir"
fi
mkdir -p "$tools_dir"

cp "./target/release/scc" "$tools_dir"
if [[ "$OSTYPE" == "darwin"* ]]; then
    cp -r ./assets/macos/archive/* "$staging_dir"
    cp "./target/release/libscc_lib.dylib" "$tools_dir"
    cp "./target/release/redscript-cli" "$working_dir/redscript-cli-aarch64-darwin"
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    cp "./target/release/libscc_lib.so" "$tools_dir"
    cp "./target/release/redscript-cli" "$working_dir/redscript-cli-x86_64-linux-gnu"
fi

rm -f "$working_dir/$archive_name"
(
    cd "$staging_dir"
    zip -r "$working_dir/$archive_name" *
)
