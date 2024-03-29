vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  pattern = "*.gd",
  callback = function()
    vim.bo.filetype = "gdscript"
  end,
})
