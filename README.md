# redscript
Toolkit for working with scripts used by REDengine in Cyberpunk 2077.
Currently includes a compiler, a decompiler and a disassembler.

## usage
```
Usage:
  decompile [opts]
  compile [opts]
  lint [opts]
Compiler options:
  -s, --src SRC        source file or directory
  -b, --bundle BUNDLE  redscript bundle file to read
  -o, --output OUTPUT  redscript bundle file to write
Decompiler options:
  -i  --input INPUT    input redscripts bundle file
  -o, --output OUTPUT  output file or directory
  -m, --mode MODE      dump mode (one of: 'ast', 'bytecode' or 'code')
  -f, --dump-files     split into individual files (doesn't work for everything yet)
  -v, --verbose        verbose output (include implicit conversions)
Lint options:
  -s, --src SRC        source file or directory
  -b, --bundle BUNDLE  redscript bundle file to use, optional
```

You can build the project and decompile all scripts in one command:
```bash
cargo run --bin redscript-cli --release -- decompile -i '/mnt/d/games/Cyberpunk 2077/r6/cache/final.redscript' -o dump.reds
```
*__note__: current version requires nightly version of rust (`rustup default nightly`)*

## language
The scripts use a Swift-like language.

You can find a brief overview of it's features in the [wiki](https://github.com/jac3km4/redscript/wiki).

You can also try it out [in your browser](https://try-redscript.surge.sh).

Some simple reference script examples can be found [here](https://github.com/jac3km4/redscript/blob/master/resources/patches.reds).

## integrating with the game
You can integrate this compiler with the game and make it compile your scripts on startup.

To set it up, you can download the `redscript-mod-{version}.zip` archive from the [latest release](https://github.com/jac3km4/redscript/releases) and extract it in the main game directory. You should end up with the following files:
- `Cyberpunk 2077/engine/tools/scc.exe`
- `Cyberpunk 2077/engine/config/base/scripts.ini`

If the compiler is set up correctly it will save logs to `Cyberpunk 2077/r6/cache/redscript.log` whenever you start the game.
