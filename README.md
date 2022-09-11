<img align="left" width="0px" height="18px"/>
<img src="https://user-images.githubusercontent.com/11986158/145484796-9bf1f77f-e706-4e15-b46b-c9b949f0086c.png" align="left" width="100px" height="100px"/>

<h3>redscript</h3>

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

## language
The scripts use a Swift-like language.
You can find a brief overview of it's features in the [official wiki](https://wiki.redmodding.org/redscript).

You can also try it out [in your browser](https://try-redscript.surge.sh).

For more examples, you can have a look at some projects using redscript:
- [jackhumbert/let_there_be_flight](https://github.com/jackhumbert/let_there_be_flight)
- [pxiberx/cp2077-playground](https://github.com/psiberx/cp2077-playground)
- [djkovrik/CP77Mods](https://github.com/djkovrik/CP77Mods)

## editor support
There's a dedicated [language server for redscript](https://github.com/jac3km4/redscript-ide), with support for code editors:
- [Visual Studio Code plugin](https://github.com/jac3km4/redscript-ide-vscode)

## integrating with the game
You can integrate this compiler with the game and make it compile your scripts on startup.

To set it up, you can download the `redscript-mod-{version}.zip` archive from the [latest release](https://github.com/jac3km4/redscript/releases) and extract it in the main game directory. You should end up with the following files:
- `Cyberpunk 2077/engine/tools/scc.exe`
- `Cyberpunk 2077/engine/config/base/scripts.ini`

If the compiler is set up correctly it will save logs to `Cyberpunk 2077/r6/cache/redscript.log` whenever you start the game.
