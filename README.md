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
```java
  public final const ref<GameObject> GetReprimandPerformer(EntityID target?) {
    Agent agent;
    ref<GameObject> performer;
    ref<DeviceComponentPS> ps;
    if(!EntityID.IsDefined(target)) {
      target = GetPlayer(this.GetGameInstance()).GetEntityID();
    };
    if(this.m_agentsRegistry.GetReprimandPerformer(target, agent)) {
      ps = this.GetPS(agent.link);
      return cast<GameObject>(WeakRefToRef(ps.GetOwnerEntityWeak()));
    };
    return null;
  }
```

## Ingame Compiler

Go to ini file (if not exist create it)
``Cyberpunk 2077\engine\config\base\scripts.ini``

Add to scripts.ini the following code:
```
[Scripts]
EnableCompilation = "true"
```

Place the scc.exe tool in the following path:
(The tool can be found in releases)
``Cyberpunk 2077\engine\tools\scc.exe``

Time for you to create the mod

Add a file of your mod name
``Cyberpunk 2077\r6\scripts\mod_name.reds``

Add some code to the .reds, for example:
```c++
@insert(CrossingLight)
protected final void PlayTrafficNotificationSound(worldTrafficLightColor status) {
  return;
}
```

Launch the game (this mod will disable the walk walk don't walk don't walk audio)
