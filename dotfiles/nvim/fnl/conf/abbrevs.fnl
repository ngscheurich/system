(fn iabbrev [lhs rhs] (vim.cmd (string.format "iabbrev %s %s" lhs rhs)))
(fn cabbrev [lhs rhs] (vim.cmd (string.format "cabbrev %s %s" lhs rhs)))

(cabbrev "Xa" "xa")
(cabbrev "Xa!" "xa!")
(cabbrev "Qa" "qa")
(cabbrev "Qa!" "qa!")
