# redscript
Toolkit for working with scripts used by REDengine in Cyberpunk 2077.
Currently includes a compiler, a decompiler and a disassembler.

## usage
```
Usage:
  decompile [opts]
  compile [opts]
Compiler options:
  -i, --input INPUT    input source file
  -b, --bundle BUNDLE  redscript bundle file to read
  -o, --output OUTPUT  redscript bundle file to write
Decompiler options:
  -i, --input INPUT    input file
  -o, --output OUTPUT  output file or directory
  -m, --mode MODE      dump mode (one of: 'ast', 'bytecode' or 'code')
  -f, --dump-files     split into individual files (doesn't work for everything yet)
```

You can build the project and decompile all scripts in one command:
```bash
cargo run --release -- decompile -i '/mnt/d/games/Cyberpunk 2077/r6/cache/final.redscript' -o classes.redscript
```
*__note__: current version requires nightly version of rust (`rustup default nightly`)*

## language
The scripts use a Java-like language:
```
  public ref<GameObject> GetReprimandPerformer(EntityID target) {
    Agent agent;
    Handle<GameObject> performer;
    Handle<DeviceComponentPS> ps;
    if(!IsDefined(target)) {
      target = GetPlayer(GetGameInstance()).GetEntityID();
    };
    if(this.m_agentsRegistry.GetReprimandPerformer(target, agent)) {
      ps = GetPS(agent.link);
      return Cast(ToHandle(ps.GetOwnerEntityWeak()));
    };
    return null;
  }
```
