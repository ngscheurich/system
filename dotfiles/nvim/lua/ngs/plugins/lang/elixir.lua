local util = require("ngs.util")

return {
  util.treesitter_ensure("elixir"),
  util.lspconfig_setup("elixirls", {
    cmd = { vim.env.HOME .. "/.local/share/nvim/mason/bin/elixir-ls" },
  }),
}
