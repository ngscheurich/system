return {
  "preservim/vim-pencil",

  ft = { "markdown" },

  config = function()
    vim.api.nvim_create_autocmd("FileType", {
      desc = "Turn on vim-pencil for Markdown files",
      group = vim.api.nvim_create_augroup("ngs.pencil", {}),
      pattern = "markdown",
      callback = function() end,
    })
  end,
}
