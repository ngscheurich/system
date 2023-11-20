local ls = require("luasnip")
local util = require("ngs.util")
local s, t, i = ls.snippet, ls.text_node, ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt
local rep = require("luasnip.extras").rep

return {
  util.treesitter_ensure("elixir"),
  util.lspconfig_setup("elixirls", {
    cmd = { "elixir-ls" },
  }),
  util.luasnip_add("lua", {
    s("pry", { t({
      "require IEx",
      "IEx.pry()",
    }) }),

    s(
      "ins",
      fmt('IO.inspect({}, label: "{}")', {
        i(1),
        rep(1),
      })
    ),
  }),
}
