return {
  bashls = {},

  clangd = {},

  elixirls = {
    cmd = { "~/Projects/elixir-ls/release/language_server.sh" },
  },

  lua_ls = {
    settings = {
      Lua = {
        telemetry = { enable = false },
        workspace = { checkThirdParty = false },
      },
    },
  },

  rust_analyzer = {},

  solargraph = {},

  tailwindcss = {},

  tsserver = {},
}
