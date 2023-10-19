local util = require("ngs.util")

return {
  util.treesitter_ensure("elixir"),
  util.lspconfig_setup("elixirls", {
    cmd = { "/Users/nick/Projects/elixir-ls/release/language_server.sh" },
  }),
}
