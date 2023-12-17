use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=../lib/include/scc.h");

    let bindings = bindgen::Builder::default()
        .header("../lib/include/scc.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .clang_arg("-D BINDING_TEST")
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
