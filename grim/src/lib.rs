pub mod link;
pub mod switch;
pub mod theme;

pub mod util {
    use std::env;
    use std::io::{BufRead, BufReader};
    use std::path::PathBuf;
    use std::process::{Command, Stdio};

    pub enum SGREffect {
        Bold,
        Faint,
        Underline,
        Black,
        Red,
        Green,
        Yellow,
        Blue,
        Magenta,
        Cyan,
        White,
    }

    pub fn decorate(text: &str, effect: SGREffect) -> String {
        let param = match effect {
            SGREffect::Bold => "1",
            SGREffect::Faint => "2",
            SGREffect::Underline => "4",
            SGREffect::Black => "30",
            SGREffect::Red => "31",
            SGREffect::Green => "32",
            SGREffect::Yellow => "33",
            SGREffect::Blue => "34",
            SGREffect::Magenta => "35",
            SGREffect::Cyan => "36",
            SGREffect::White => "37",
        };

        format!("\x1b[{}m{}\x1b[0m", param, text)
    }

    fn get_env_var(key: &str, default: &str) -> String {
        match env::var(key) {
            Ok(val) => val,
            Err(_) => default.to_string(),
        }
    }

    pub fn system_dir() -> PathBuf {
        let dir = get_env_var("SYSTEM_DIR", "/etc/system");
        PathBuf::from(dir)
    }

    pub fn xdg_config_dir() -> PathBuf {
        let dir =
            if env::var("XDG_CONFIG_HOME").is_ok() {
                env::var("XDG_CONFIG_HOME").unwrap()
            } else {
                format!("{}/.config", env::var("HOME").unwrap())
            };

        PathBuf::from(dir)
    }

    pub fn config_path(name: &str) -> PathBuf {
        [xdg_config_dir().to_str().unwrap(), name].iter().collect()
    }

    pub fn home_dir() -> PathBuf {
        let dir = get_env_var("HOME", "~");
        PathBuf::from(dir)
    }

    pub fn success(msg: String) {
        let sign = decorate("", SGREffect::Green);
        println!("{} {}", sign, msg);
    }

    pub fn info(msg: String) {
        let sign = decorate("", SGREffect::Blue);
        println!("{} {}", sign, msg);
    }

    pub fn warn(msg: String) {
        let sign = decorate("", SGREffect::Yellow);
        println!("{} {}", sign, msg);
    }

    pub fn error(err: std::io::Error) {
        let sign = decorate("", SGREffect::Red);
        println!("{} {}", sign, err);
    }

    pub fn exec(cmd: String) {
        // println!("Executing: {}", cmd);

        let mut child = Command::new("sh")
            .arg("-c")
            .arg(cmd)
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();

        let stdout = child.stdout.take().unwrap();

        let lines = BufReader::new(stdout).lines();
        for line in lines {
            println!("{}", line.unwrap());
        }
    }
}
