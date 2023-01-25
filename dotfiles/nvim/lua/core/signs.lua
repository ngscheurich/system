local function set_sign_text(severity, text)
  vim.fn.sign_define(
    "DiagnosticSign" .. severity,
    { text = text, texthl = "Diagnostic" .. severity }
  )
end

set_sign_text("Error", "")
set_sign_text("Warn", "")
set_sign_text("Information", "")
set_sign_text("Hint", "")
