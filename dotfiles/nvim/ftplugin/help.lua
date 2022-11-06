vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = { "help" },
  callback = function()
    vim.opt_local.signcolumn = "no"
  end,
})
