> [!NOTE]
> 🎒 I’m trying something old, but new, over at https://github.com/ngscheurich/dotfiles.

# /etc/system

![A collection of terminal emulator windows runnning neofetch, btm, and Neovim.](img/system.png)

This repository contains configurations for the computer systems I use.
Wanderers beware: this place is ever-evolving and fraught with great peril.

## Contents

- [Prelude](#prelude)
- [Systems](#systems)
- [Nix](#nix)
- [Grimoire](#grimoire)
- [Dotfiles](#dotfiles)
- [Quickstart](#quickstart)

## Prelude 

_Computers, huh?_ Who would have thought? And yet, here we are. These are the
configuration files for my computer systems.

I’ve published them here so that folks such as you or future I might extract
some wisdom from them, or perhaps just be humorously confounded. Your mileage,
as they say, may vary. Also, this proves a handy place to back them up and pull
them down onto new machines.

I use [Nix], a purely functional package manager, and various tools within its
ecosystem to declaratively configure the core parts of these systems.

In addition to the Nix configuration files, this repo also contains:

- Personal [dotfiles](#dotfiles) that I link into my home directory
- [_Grimoire_](#grimoire), a [command-line interface] that simplifies system management tasks
- Shell scripts that do a little of this, a little of that
- Theme files that I use to change the look of my terminal environment

## Systems

### Bienjensu

My personal computer is a 2023 14” [Apple MacBook Pro] that runs on the [Apple
M2 Pro] [SoC] architecture. I use [nix-darwin] to manage as much of the system
as is possible.

| Component        | Description                  |
| ---------------- | ---------------------------- |
| Operating System | macOS 13.2 (Ventura)         |
| CPU              | 3.49 GHz AArch64             |
| Memory           | 16 GB LPDDR5                 |
| Storage          | 512 GB PCIe 4.0 ×4 SSD       |


### Joulie

My work computer is a 2021 14” [Apple MacBook Pro] that runs on the [Apple M1
Pro] [SoC] architecture. I use [nix-darwin] to manage as much of the system as
is possible.

| Component        | Description                  |
| ---------------- | ---------------------------- |
| Operating System | macOS 13.2 (Ventura)         |
| CPU              | 3.2 GHz GHz AArch64          |
| Memory           | 32 GB LPDDR5                 |
| Storage          | 1 TB PCIe 4.0 ×4 SSD         |

## Nix

### Flake

The root of my Nix configuration is a _[flake]_ whose outputs are
configurations for each of my systems.

System-level configuration is managed by [nix-darwin] or [NixOS], depending on
the platform.

Each system’s user-level packages are declared and configured using [Home
Manager]. Some macOS applications are managed by [Homebrew] via nix-darwin.

### Modules

I maintain a set of Nix modules loosely separated by area of concern. This
helps prevent polluting any given system with unneeded binaries and
configuration files, and helps me feel like I’ve really _got it together_, at
least a little bit.

#### Code

One of the primary tasks I use my systems for is **source code editing**. My
editor of choice is [Neovim], which is declared here. Additionally, this
module declares packages that my [Neovim configuration](config/nvim) relies
on for advanced editing features such as [Tree-sitter] and various [LSP]
servers.

#### Crypt

I use public key cryptography for encrypting data like emails and sensitive
documents, and for authenticating ownership of Internet resources such as Git
commits and domain names. This module declares the [GNU Privacy Guard]
package as well as a convenient terminal UI for it.

#### Devel

This module declared packages that are used to build, debug, and run software.
It declares programming languages, database and hardware tools, and lots of
build-time dependencies—[Autoconf], [Cmake], et al.

#### Devops 

I sometimes find myself needing to engage in some light devops work and this
module declares packages support those sorts of tasks and makes them a bit
comfier on the command line.

#### Git

The [Git] version control system is a key piece of my daily workflow. This
declares it along with configuration and some supplementary packages, namely
[Lazygit], a Git terminal UI.

#### Prose

To make it more pleasant to read and edit natural language text, e.g. notes,
documentation, short stories, I specify some packages here like [Vale] and
[Glow].

#### Shell

The packages declared here work to provide a rich, productive experience on the
command-line. Namely, [tmux], [Starship], and [direnv] are critical to my
workflow. This module also declares modern replacements for some common Unix
commands and a smörgåsbord of my favorite CLI and TUI programs related (but not
limited) to:

- File management
- Find/replace
- Monitoring
- Networking
- Multiplexing
- Generative AI

#### Vim

Herein is specified the venerable [Vim] text editor and a simple configuration
that I use to turn it into a more basic, lightweight alternative to Neovim for
quick editing tasks.

## Grimoire

Included in this repository is the source code for _Grimoire_, a command-line
interface I created for system management. I’ll let its `help` describe it
briefly:

    A book of magic spells

    Usage:
      grim [command]

    Available Commands:
      completion  Generate the autocompletion script for the specified shell
      config      Manage program configurations
      help        Help about any command
      switch      Switch to a new Nix system generation
      theme       Change user theme

    Flags:
      -h, --help   help for grim

    Use "grim [command] --help" for more information about a command.

### Building

To build Grimoire, first install Nix then run this command from the `cli` directory:

    nix shell nixpkgs#go -c make

Alternatively, if you have [Go] installed, you can simply `make`.

## Dotfiles

Home Manager has the capability to configure some of the programs that I use,
but I prefer to maintain a classic [dotfiles] setup for some. Programs
configured this way include:

- [Aerospace], a tiling window manager for macOS
- [Emacs], a text editor and Lisp application environment
- [ghostty], my terminal emulator of choice
- [Karabiner-Elements], a keyboard customizer for macOS
- [kitty], another great terminal emulator
- [Lazygit], a terminal UI for Git
- [Neovim], my primary text editor and software development tool
- [skhd], a simple hotkey daemon for macOS
- [tmux], a terminal multiplexer
- [yazi], a terminal file manager
- [zellij], a terminal workspace

These configs can be managed with Grimoire: `grim config --help`.

## Quickstart

### macOS

Install Nix:

    $ sh <(curl -L https://nixos.org/nix/install)

Install [Homebrew]:

    $ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

Clone this repository:

    $ sudo git clone https://github.com/ngscheurich/system.git /etc/system
    $ sudo chown -R $(whoami):staff /etc/system

Set the hostname:

    $ sudo scutil --set HostName [HOSTNAME]

Build the system:

    $ nix build /etc/system/#darwinConfigurations.[HOSTNAME].system --out-link /tmp/nix-build

Switch to the new system generation:

    $ sudo /tmp/nix-build/sw/bin/darwin-rebuild switch --flake /etc/system#[HOSTNAME]

Log out and back in for all changes to take effect.

[aerospace]: https://github.com/nikitabobko/AeroSpace
[apple m1 pro]: https://en.wikipedia.org/wiki/Apple_M1_Pro_and_M1_Max
[apple m2 pro]: https://en.wikipedia.org/wiki/Apple_M2
[apple macbook pro]: https://en.wikipedia.org/wiki/MacBook_Pro
[asdf]: https://asdf-vm.com/
[autoconf]: https://www.gnu.org/software/autoconf/
[cmake]: https://cmake.org/
[command-line interface]: https://en.wikipedia.org/wiki/Command-line_interface
[direnv]: https://direnv.net/
[dotfiles]: https://en.wikipedia.org/wiki/Hidden_file_and_hidden_directory
[emacs]: https://www.gnu.org/software/emacs/
[fish]: https://fishshell.com/
[flake]: https://nixos.wiki/wiki/Flakes
[lsp]: https://microsoft.github.io/language-server-protocol/
[ghostty]: https://ghostty.org/
[git]: https://git-scm.com/
[glow]: https://github.com/charmbracelet/glow
[gnu privacy guard]: https://gnupg.org/
[gnu stow]: https://www.gnu.org/software/stow/
[go]: https://go.dev/
[home manager]: https://github.com/nix-community/home-manager
[homebrew]: https://brew.sh/
[karabiner-elements]: https://karabiner-elements.pqrs.org/
[kitty]: https://sw.kovidgoyal.net/kitty/
[lazygit]: https://github.com/jesseduffield/lazygit
[neovim]: https://neovim.io/
[nix-darwin]: https://github.com/LnL7/nix-darwin
[nix]: https://nixos.org/
[nixos]: https://en.wikipedia.org/wiki/NixOS
[shell-modules]: modules/home/shell.nix
[shell]: https://en.wikipedia.org/wiki/Shell_(computing)
[skhd]: https://github.com/koekeishiya/skhd
[soc]: https://en.wikipedia.org/wiki/System_on_a_chip
[starship]: https://starship.rs/
[themes]: /themes
[tmux]: https://github.com/tmux/tmux
[tree-sitter]: https://tree-sitter.github.io/tree-sitter/
[vale]: https://github.com/errata-ai/vale
[vim]: https://www.vim.org
[xdg]: https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
[yazi]: https://yazi-rs.github.io/
[zellij]: https://zellij.dev/
