use std::fs;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::os::unix;
use std::path::PathBuf;

use crate::util::*;

pub fn apply(name: &str) -> io::Result<()> {
    let dir = themes_dir(name);
    let name = decorate(name, SGREffect::Bold);

    if dir.exists() {
        apply_file(&dir, "kitty.conf", apply_kitty);
        apply_file(&dir, "tmux.conf", apply_tmux);
        apply_file(&dir, "nvim.lua", apply_nvim);
        success(format!("Applied {} theme", name));
    } else {
        warn(format!("{} is not a valid theme", name));
    }

    Ok(())
}

fn apply_kitty(path: PathBuf) {
    let root = config_path("kitty");
    let dest = PathBuf::from_iter([root.to_str().unwrap(), "colors.conf"]);
    mk_symlink(&path, &dest);

    let cmd = format!("kitty @ set-colors -c {:?}", path);
    exec(cmd);
}

fn apply_tmux(path: PathBuf) {
    let cmd = format!("tmux source-file {:?}", path);
    exec(cmd);
}

fn apply_nvim(path: PathBuf) {
    let root = config_path("nvim");
    let dest = PathBuf::from_iter([root.to_str().unwrap(), "plugin", "colorscheme.lua"]);
    mk_symlink(&path, &dest);
}

fn mk_symlink(path: &PathBuf, dest: &PathBuf) {
    if dest.exists() {
        fs::remove_file(dest).expect("unable to remove link")
    }
    unix::fs::symlink(path, dest).expect("unable to create link");
}

fn apply_file(dir: &PathBuf, name: &str, cb: fn(PathBuf)) {
    let mut file = dir.clone();
    file.push(name);

    if file.exists() {
        cb(file);
    }
}

pub fn list() -> io::Result<()> {
    let dir = themes_dir(".");

    let theme_name = read_theme_name().unwrap();

    for entry in fs::read_dir(dir)? {
        let name = entry.unwrap().file_name();
        let name = name.to_str().unwrap();
        let label = if name == theme_name { "" } else { "" };
        println!("{} {}", label, name);
    }

    Ok(())
}

fn themes_dir(path: &str) -> PathBuf {
    let mut dir = system_dir();
    dir.push("themes");
    dir.push(path);
    return dir;
}

fn read_theme_name() -> io::Result<String> {
    let theme_path: PathBuf = [home_dir().to_str().unwrap(), ".theme"].iter().collect();
    let mut file = File::open(theme_path)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    Ok(buf.trim().to_string())
}
