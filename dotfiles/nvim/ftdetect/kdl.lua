vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  pattern = "*.kdl",
  callback = function()
    vim.bo.filetype = "kdl"
  end,
})
