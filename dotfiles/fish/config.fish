if status is-interactive
  set -gx GPG_TTY (tty)

  set -g fish_key_bindings fish_vi_key_bindings

  starship init fish | source
end
