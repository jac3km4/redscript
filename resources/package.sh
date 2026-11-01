#!/usr/bin/env bash
# Build the executable, copy required files and create a Nexus-style mod archive.
set -euo pipefail

if [ $# -ne 2 ]; then
    echo "Usage: $0 <staging_dir> <archive_name>" >&2
    exit 1
fi

staging_dir="$1"
archive_name="$2"

if [[ ! "$archive_name" =~ ^[a-zA-Z0-9][a-zA-Z0-9._-]*\.zip$ ]]; then
    echo 'Archive name must be a simple .zip filename.' >&2
    exit 1
fi
mkdir -p -- "$staging_dir"
shopt -s nullglob dotglob
staging_entries=("$staging_dir"/*)
shopt -u nullglob dotglob
if (( ${#staging_entries[@]} != 0 )); then
    echo 'Staging directory must be empty.' >&2
    exit 1
fi

case "$(uname -s):$(uname -m)" in
    Darwin:arm64)
        host_target=aarch64-apple-darwin
        library=libscc_lib.dylib
        cli_name=redscript-cli-aarch64-darwin
        ;;
    Linux:x86_64)
        host_target=x86_64-unknown-linux-gnu
        library=libscc_lib.so
        cli_name=redscript-cli-x86_64-linux-gnu
        ;;
    *)
        echo 'Unsupported platform or architecture: expected Darwin arm64 or Linux x86_64.' >&2
        exit 1
        ;;
esac

working_dir=$(pwd)
target_dir="$working_dir/target"
release_dir="$target_dir/$host_target/release"
tools_dir="$staging_dir/engine/tools"
archive_path="$working_dir/$archive_name"

cargo build --release --locked --target "$host_target" --target-dir "$target_dir" -p scc -p scc-lib -p redscript-cli --features mmap,popup

mkdir -p -- "$tools_dir"
cp -- "$release_dir/scc" "$release_dir/$library" "$tools_dir"
cp -- "$release_dir/redscript-cli" "$working_dir/$cli_name"
if [[ "$host_target" == aarch64-apple-darwin ]]; then
    cp -r ./resources/mod/macos/. "$staging_dir"
fi

rm -f -- "$archive_path"
(
    cd -- "$staging_dir"
    zip -r "$archive_path" .
)
