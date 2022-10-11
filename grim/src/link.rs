use std::fs;
use std::io;
use std::os::unix;
use std::path::PathBuf;

use crate::util::*;

pub fn add(names: &Vec<String>, all: &bool) -> io::Result<()> {
    let dots_dir = dotfiles_dir();

    if *all {
        for entry in fs::read_dir(dots_dir)? {
            mk_symlink(entry?.path());
        }
    } else {
        for name in names {
            let root = dots_dir.to_str().unwrap();
            let path = PathBuf::from_iter([root, name]);
            mk_symlink(path).expect("unable to create link");
        }
    }

    Ok(())
}

pub fn del(names: &Vec<String>, all: &bool) -> io::Result<()> {
    let dots_dir = dotfiles_dir();
    let conf_dir = xdg_config_dir();

    if *all {
        for entry in fs::read_dir(dots_dir)? {
            rm_symlink(entry?.path());
        }
    } else {
        for name in names {
            let root = conf_dir.to_str().unwrap();
            let path = PathBuf::from_iter([root, name]);
            rm_symlink(path).expect("unable to remove link");
        }
    }

    Ok(())
}

pub fn list() -> io::Result<()> {
    let conf_dir = dotfiles_dir();

    for src in fs::read_dir(conf_dir)? {
        let src = src.unwrap();
        let target = config_path(src.file_name().to_str().unwrap());
        let label = if target.exists() { "" } else { "" };
        let name = decorate(src.file_name().to_str().unwrap(), SGREffect::Bold);
        println!(" {} {}", label, name);
    }

    Ok(())
}

fn mk_symlink(path: PathBuf) -> Option<()> {
    let name = name(&path)?;
    let dest = config_path(path.file_name()?.to_str()?);

    if !path.exists() {
        warn(format!("{} is not a valid dotfiles package", name));
    } else if dest.exists() {
        info(format!("{} is already linked", name));
    } else {
        match unix::fs::symlink(path, dest) {
            Ok(_) => success(format!("Added link to {}", name)),
            Err(err) => error(err),
        };
    };

    Some(())
}

fn rm_symlink(path: PathBuf) -> Option<()> {
    let name = name(&path)?;
    let dot_dir = String::from(dotfiles_dir().to_str()?);
    let source = PathBuf::from_iter([dot_dir, String::from(path.file_name()?.to_str()?)]);

    if !source.exists() {
        warn(format!("{} is not a valid dotfiles package", name));
    } else {
        if !path.exists() {
            info(format!("{} is not linked", name));
        } else {
            match fs::remove_file(path) {
                Ok(_) => success(format!("Removed link to {}", name)),
                Err(err) => error(err),
            }
        }
    }

    Some(())
}

fn name(source: &PathBuf) -> Option<String> {
    let name = source.file_name()?.to_str()?;
    Some(decorate(name, SGREffect::Bold))
}

fn dotfiles_dir() -> PathBuf {
    let mut dir = system_dir();
    dir.push("dotfiles");
    return dir;
}
