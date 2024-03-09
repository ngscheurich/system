local ls = require("luasnip")
local util = require("ngs.util")
local s, i = ls.snippet, ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt

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
  util.formatter_setup("lua", { "stylua" }),
  util.luasnip_add("lua", {
    s(
      "nvim-plugin-info",
      fmt(
        [[
      -- =================================================================
      --  {}
      -- -----------------------------------------------------------------
      --  {}
      --  {}
      -- -----------------------------------------------------------------
      ]],
        { i(1), i(2), i(3) }
      )
    ),
  }),
}
