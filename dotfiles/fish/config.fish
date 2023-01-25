if status is-interactive
  set -gx GPG_TTY (tty)

  set -g fish_key_bindings fish_hybrid_key_bindings

  fish_add_path '/Applications/kitty.app/Contents/MacOS/'

  fzf_configure_bindings

  starship init fish | source
  thefuck --alias | source
end
