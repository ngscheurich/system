use std::env::consts::OS;
use std::fs;

use crate::util::{self, error, exec};

const OUT_LINK: &str = "/tmp/nix-build-result";

pub fn switch(host: &Option<String>) {
    let platform = match OS {
        "macos" => "darwin",
        _ => "nixos",
    };

    let host = match host {
        Some(val) => val,
        None => "$(hostname)",
    };

    nix_build(platform, host);
    nix_switch(platform, host);

    match fs::remove_file(OUT_LINK) {
        Ok(_) => (),
        Err(err) => error(err),
    }
}

fn nix_build(platform: &str, host: &str) {
    let cmd = format!(
        "nix build {}/#{}Configurations.{}.system --out-link {}",
        system_dir(),
        platform,
        host,
        OUT_LINK
    );
    exec(cmd);
}

fn nix_switch(platform: &str, host: &str) {
    let cmd = match platform {
        "darwin" => darwin_switch_cmd(host),
        "nixos" => nixos_switch_cmd(host),
        _ => String::from("echo 'Unsupported operating system!"),
    };
    exec(cmd);
}

fn darwin_switch_cmd(host: &str) -> String {
    format!(
        "{}/sw/bin/darwin-rebuild switch --flake {}#{}",
        OUT_LINK,
        system_dir(),
        host
    )
}

fn nixos_switch_cmd(host: &str) -> String {
    format!(
        "sudo nixos-rebuild switch --flake {}#{}",
        system_dir(),
        host
    )
}

fn system_dir() -> String {
    String::from(util::system_dir().to_str().unwrap())
}
