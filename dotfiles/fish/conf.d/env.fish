set -gx AWS_VAULT_KEYCHAIN_NAME login
set -gx BROWSER open
set -gx EDITOR nvim
set -gx ERL_AFLAGS '-kernel shell_history enabled'
set -gx KERL_CONFIGURE_OPTIONS --without-javac
set -gx NVIM_LISTEN_ADDRESS /tmp/nvimsocket
set -gx PLAYDATE_SDK_PATH $HOME/Developer/PlaydateSDK/
set -gx RANGER_DEVICONS_SEPARATOR '  '
set -gx XDG_DATA_HOME $HOME/.local/share
set -gx XDG_CONFIG_HOME $HOME/.config
set -gx XDG_CACHE_HOME $HOME/.cache
set -gx LOCAL_BIN_DIR $HOME/.local/bin
set -gx SCRIPTS_DIR /etc/system/scripts

fish_add_path "$LOCAL_BIN_DIR"
