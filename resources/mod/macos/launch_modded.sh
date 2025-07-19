#!/usr/bin/env bash
# Compiles REDscript and launches the game.

game_dir=$(dirname "$(readlink -f "$0")")

"$game_dir/engine/tools/scc" -compile "$game_dir/r6/scripts"
"$game_dir/Cyberpunk2077.app/Contents/MacOS/Cyberpunk2077"
