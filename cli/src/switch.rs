use std::env::consts::OS;
use std::fs;

use crate::util::{self, error, exec};

const OUT_LINK: &str = "/tmp/nix-build-result";

pub fn switch(conf: &Option<String>) {
    let platform = match OS {
        "macos" => "darwin",
        _ => "linux",
    };

    let conf = match conf {
        Some(val) => val,
        None => "$(hostname)",
    };

    nix_build(platform, conf);
    nix_switch(platform);

    match fs::remove_file(OUT_LINK) {
        Ok(_) => (),
        Err(err) => error(err),
    }
}

fn nix_build(platform: &str, conf: &str) {
    let cmd = format!(
        "nix build {}/#{}Configurations.{}.system --out-link {}",
        system_dir(),
        platform,
        conf,
        OUT_LINK
    );
    exec(cmd);
}

fn nix_switch(platform: &str) {
    let cmd = match platform {
        "darwin" => darwin_switch_cmd(),
        "linux" => nixos_switch_cmd(),
        _ => String::from("echo 'Unsupported operating system!"),
    };
    exec(cmd);
}

fn darwin_switch_cmd() -> String {
    format!(
        "{}/sw/bin/darwin-rebuild switch --flake {}#",
        OUT_LINK,
        system_dir()
    )
}

fn nixos_switch_cmd() -> String {
    format!("sudo nixos-rebuild switch --flake {}#", system_dir())
}

fn system_dir() -> String {
    String::from(util::system_dir().to_str().unwrap())
}
