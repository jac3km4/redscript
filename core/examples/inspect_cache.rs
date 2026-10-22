//! Inspect and round-trip a cache without compiling scripts or launching the game.
use std::io::{self, Cursor};
use std::{env, fs};

use redscript::bundle::ScriptBundle;

fn main() -> io::Result<()> {
    let args: Vec<_> = env::args_os().collect();
    if args.len() != 3 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Usage: inspect_cache INPUT OUTPUT",
        ));
    }
    let input = fs::read(&args[1])?;
    let bundle = ScriptBundle::load(&mut Cursor::new(&input))?;
    println!("Loaded {} definitions", bundle.pool.definitions().count());
    let mut output = Cursor::new(Vec::new());
    bundle.save(&mut output)?;
    let output = output.into_inner();
    println!("Byte-identical roundtrip: {}", output == input);
    fs::write(&args[2], output)
}
