set -gx AWS_VAULT_KEYCHAIN_NAME login
set -gx BROWSER open
set -gx EDITOR nvim
set -gx ERL_AFLAGS '-kernel shell_history enabled'
set -gx KERL_CONFIGURE_OPTIONS --without-javac
set -gx NVIM_LISTEN_ADDRESS /tmp/nvimsocket
set -gx RANGER_DEVICONS_SEPARATOR '  '
set -gx XDG_DATA_HOME $HOME/.local/share
set -gx XDG_CONFIG_HOME $HOME/.config

set -l brew '/opt/homebrew/bin/brew'
if test -f $brew
  eval ($brew shellenv)
end

if test -f $__fish_config_dir/secrets.fish
  source $__fish_config_dir/secrets.fish
end

starship init fish | source
