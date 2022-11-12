if not set -q ngs_fisher_installed
  set -U ngs_fisher_installed
  curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher
  fisher update
end
