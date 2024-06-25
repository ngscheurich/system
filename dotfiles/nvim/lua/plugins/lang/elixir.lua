local ls = require("luasnip")
local util = require("util")
local s, t, i = ls.snippet, ls.text_node, ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt
local rep = require("luasnip.extras").rep

return {
  util.treesitter_ensure("elixir"),
  util.lspconfig_setup("lexical", {
    cmd = { vim.fn.stdpath("data") .. "/mason/bin/lexical" },
  }),
  util.luasnip_add("elixir", {
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
