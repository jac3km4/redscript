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
```swift
public final const GetReprimandPerformer(opt target: EntityID): ref<GameObject> {
  let agent: Agent;
  let performer: ref<GameObject>;
  let ps: ref<DeviceComponentPS>;
  if(!EntityID.IsDefined(target)) {
    target = GetPlayer(this.GetGameInstance()).GetEntityID();
  };
  if(this.m_agentsRegistry.GetReprimandPerformer(target, agent)) {
    ps = this.GetPS(agent.link);
    return WeakRefToRef(ps.GetOwnerEntityWeak()) as GameObject;
  };
  return null;
}
```

## integrating with the game
You can integrate this compiler with the game and make it compile your scripts on startup. To set that up, you need to follow these steps:

- Create `Cyberpunk 2077\engine\config\base\scripts.ini` file with the contents below <br/>
```ini
[Scripts]
EnableCompilation = "true"
```
- Place the `scc.exe` tool in the following location: <br/>
``Cyberpunk 2077\engine\tools\scc.exe``<br/>
*(The scc executable can be found in Releases)*

- Now you need to add some scripts. The compiler will look for scripts in `Cyberpunk 2077\r6\scripts\`<br />
You can copy the script below to `Cyberpunk 2077\r6\scripts\lights.reds` as an example:

```swift
@insert(CrossingLight)
protected final PlayTrafficNotificationSound(status: worldTrafficLightColor) {
  return;
}
```
*this mod will disable the walk don't walk crosswalk audio*
