local M = { "echasnovski/mini.nvim" }

function M.align()
  require("mini.align").setup()
end

function M.comment()
  require("mini.comment").setup({})
end

function M.pairs()
  require("mini.pairs").setup({})
end

function M.starter()
  require("mini.starter").setup({
    footer = "",
  })
end

function M.surround()
  require("mini.surround").setup({
    mappings = {
      add = "gza",
      delete = "gzd",
      find = "gzf",
      find_left = "gzF",
      highlight = "gzh",
      replace = "gzr",
      update_n_lines = "gzn",
    },
  })
end

function M.config()
  M.align()
  M.comment()
  M.pairs()
  M.starter()
  M.surround()
end

return M
