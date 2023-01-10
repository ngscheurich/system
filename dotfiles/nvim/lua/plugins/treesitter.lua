local M = { "nvim-treesitter/nvim-treesitter" }

M.dependencies = {
  "nvim-treesitter/playground",
}

M.build = ":TSUpdate"

function M.config()
  require("nvim-treesitter.configs").setup({
    highlight = { enable = true },
    indent = { enable = true },

    ensure_installed = {
      "bash",
      "c",
      "css",
      "diff",
      "eex",
      "elixir",
      "erlang",
      "fennel",
      "fish",
      "gitignore",
      "graphql",
      "heex",
      "help",
      "html",
      "javascript",
      "json",
      "lua",
      "make",
      "markdown",
      "mermaid",
      "nix",
      "query",
      "regex",
      "rust",
      "sql",
      "toml",
      "tsx",
      "typescript",
      "yaml",
    },
  })
end

return M
