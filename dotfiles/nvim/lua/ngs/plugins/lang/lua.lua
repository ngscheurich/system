local util = require("ngs.util")

return {
  util.treesitter_ensure("lua"),
  util.lspconfig_setup("lua_ls", {
    settings = {
      Lua = {
        telemetry = { enable = false },
        workspace = { checkThirdParty = false },
        runtime = { version = "LuaJIT" },
      },
    },
  }),
  util.conform_setup("lua", { "stylua" }),
}
