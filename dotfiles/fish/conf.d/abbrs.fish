if not set -q ngs_abbrs_added
  abbr cls clear
  abbr g git
  abbr la exa --all --long
  abbr lg lazygit
  abbr ll exa --long
  abbr ls exa
  abbr lt exa --tree
  abbr serve python -m http.server
  abbr ta tmux attach
  abbr vimdiff nvim -d
  abbr weather curl wttr.in

  set -U ngs_abbrs_added
end
