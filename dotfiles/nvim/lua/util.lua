local M = { git = {} }

function M.git.format_branch(branch)
  local match = string.match(branch, "^.*/sc%-%d+")

  if match then
    branch = match
  end

  return branch
end

return M
