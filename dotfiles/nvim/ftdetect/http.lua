vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  pattern = "*.http",
  callback = function()
    vim.bo.filetype = "http"
  end,
})
