use clap::{Args, Parser, Subcommand};

use grim::link;
use grim::switch;
use grim::theme;

/// A book of magic spells
#[derive(Parser)]
#[clap(name = "Grimoire", author, version, arg_required_else_help = true)]
struct Cli {
    #[clap(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Switch to a new Nix system generation
    Switch {
        /// Nix host (defaults to hostname)
        #[clap(short = 'H', long, value_parser)]
        host: Option<String>,
    },

    /// Manage program configuration links
    #[clap(arg_required_else_help = true)]
    Link(Link),

    /// Change the system theme
    #[clap(arg_required_else_help = true)]
    Theme(Theme),
}

#[derive(Args)]
#[clap(args_conflicts_with_subcommands = true)]
struct Link {
    #[clap(subcommand)]
    command: Option<LinkCommands>,
}

#[derive(Debug, Subcommand)]
enum LinkCommands {
    /// Add config link(s)
    #[clap(arg_required_else_help = true)]
    Add {
        /// The config(s) to link
        name: Vec<String>,

        /// Link all configs
        #[clap(short, long)]
        all: bool,
    },

    /// Remove config link(s)
    #[clap(arg_required_else_help = true)]
    Del {
        /// The config(s) to unlink
        name: Vec<String>,

        /// Unlink all configs
        #[clap(short, long)]
        all: bool,
    },

    /// List configs
    List,
}

#[derive(Args)]
#[clap(args_conflicts_with_subcommands = true)]
struct Theme {
    #[clap(subcommand)]
    command: Option<ThemeCommands>,
}

#[derive(Debug, Subcommand)]
enum ThemeCommands {
    /// Apply a theme
    #[clap(arg_required_else_help = true)]
    Apply {
        /// The theme to use
        name: String,
    },

    /// List available themes
    List,
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Switch { host }) => switch::switch(host),
        Some(Commands::Link(link)) => match &link.command {
            Some(LinkCommands::Add { name, all }) => link::add(name, all).unwrap(),
            Some(LinkCommands::Del { name, all }) => link::del(name, all).unwrap(),
            Some(LinkCommands::List) => link::list().unwrap(),
            None => (),
        },
        Some(Commands::Theme(theme)) => match &theme.command {
            Some(ThemeCommands::Apply { name }) => theme::apply(name).unwrap(),
            Some(ThemeCommands::List) => theme::list().unwrap(),
            None => (),
        },
        None => (),
    }
}
