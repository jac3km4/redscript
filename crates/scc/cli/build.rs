use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo::rerun-if-changed=../capi/include/scc.h");

    let bindings = bindgen::Builder::default()
        .header("../capi/include/scc.h")
        .allowlist_type(".*[sS][cC][cC].*|StrWithLen")
        .allowlist_var(".*[sS][cC][cC].*")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .clang_arg("-D BINDING_TEST")
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
