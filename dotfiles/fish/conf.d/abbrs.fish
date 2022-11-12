if not set -q ngs_abbrs_set
  abbr cls clear
  abbr gco git checkout
  abbr grb git rebase
  abbr gst git status
  abbr la exa --all --long
  abbr lg lazygit
  abbr ll exa --long
  abbr ls exa
  abbr lt exa --tree
  abbr serve python -m http.server
  abbr ta tmux attach
  abbr vimdiff nvim -d
  abbr weather curl wttr.in

  set -U ngs_abbrs_set
end
