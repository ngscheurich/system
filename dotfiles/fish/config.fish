function fish_greeting; end

fish_add_path /opt/homebrew/bin

set -gx AWS_VAULT_KEYCHAIN_NAME login
set -gx BROWSER open
set -gx EDITOR nvim
set -gx ERL_AFLAGS '-kernel shell_history enabled'
set -gx KERL_CONFIGURE_OPTIONS --without-javac
set -gx NVIM_LISTEN_ADDRESS /tmp/nvimsocket
set -gx RANGER_DEVICONS_SEPARATOR '  '
set -gx XDG_DATA_HOME $HOME/.local/share
set -gx XDG_CONFIG_HOME $HOME/.config
set -gx PLAYDATE_SDK_PATH $HOME/Developer/PlaydateSDK 

if string match -q -r '^N.*\.local$' "$(hostname)"
  set -gx ANDROID_SDK_ROOT $HOME/Library/Android/sdk

  fish_add_path $ANDROID_SDK_ROOT/emulator
  fish_add_path $ANDROID_SDK_ROOT/platform-tools
  fish_add_path $ANDROID_SDK_ROOT/tools
  fish_add_path $ANDROID_SDK_ROOT/tools/bin
end

thefuck --alias | source

if status is-interactive
  fish_hybrid_key_bindings

  abbr -a cls clear
  abbr -a lg lazygit
  abbr -a love /Applications/love.app/Contents/MacOS/love
  abbr -a pico8 /Applications/PICO-8.app/Contents/MacOS/pico8
  abbr -a serve 'python -m http.server'
  abbr -a ta 'tmux attach'
  abbr -a vboxls 'VBoxManage list runningvms'
  abbr -a vimdiff 'nvim -d'

  alias l exa
  alias la 'exa --long --all'
  alias ll 'exa --long'
  alias ls exa
  alias lt 'exa --tree'
  alias ta 'tmux attach'
  alias tn tmuxdir
  alias weather 'curl wttr.in'

  # FZF colors
  set -gx FZF_DEFAULT_OPTS "--color \
  fg:7,\
  bg:0,\
  hl:8,\
  fg+:3,\
  bg+:0,\
  gutter:8,\
  hl+:1,\
  info:6,\
  prompt:2,\
  pointer:4,\
  marker:1,\
  spinner:5"
end

if test -f $__fish_config_dir/secrets.fish
    source $__fish_config_dir/secrets.fish
end

starship init fish | source
