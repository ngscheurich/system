vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  pattern = "*.heex",
  callback = function()
    vim.bo.filetype = "heex"
  end,
})
