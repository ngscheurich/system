vim.api.nvim_create_autocmd({ "BufWritePost" }, {
  pattern = { "*.njk" },
  command = "!npx prettier -w --parser html %",
})
