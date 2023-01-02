local M = { "nvim-treesitter/nvim-treesitter" }

M.dependencies = {
  "nvim-treesitter/playground",
}

M.build = ":TSUpdte"

function M.config()
  require("nvim-treesitter.configs").setup({
    ensure_installed = {
      "bash",
      "css",
      "diff",
      "elixir",
      "gitignore",
      "help",
      "html",
      "javascript",
      "json",
      "lua",
      "markdown",
      "query",
      "regex",
      "sql",
      "toml",
      "yaml",
    },
    highlight = { enable = true },
    indent = { enable = true },
  })
end

return M
