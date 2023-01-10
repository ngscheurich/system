local function set_sign_text(severity, text)
  local cmd = string.format("sign define DiagnosticSign%s text=%s", severity, text)
  vim.cmd(cmd)
end

set_sign_text("Error", "")
set_sign_text("Warn", "")
set_sign_text("Information", "")
set_sign_text("Hint", "")
