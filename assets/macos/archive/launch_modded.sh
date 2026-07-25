#!/usr/bin/env bash
# Compiles REDscript and launches the game.
set -euo pipefail

game_dir=$(dirname "$(readlink -f "$0")")

if ! "$game_dir/engine/tools/scc" -compile "$game_dir/r6/scripts"; then
    message="REDscript compilation failed, the game was not launched. Check r6/logs/redscript_rCURRENT.log for details."
    echo "$message" >&2
    osascript -e "display alert \"REDscript\" message \"$message\" as critical" >/dev/null 2>&1 || true
    exit 1
fi

open "$game_dir/Cyberpunk2077.app"
