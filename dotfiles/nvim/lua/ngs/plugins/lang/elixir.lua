local ls = require("luasnip")
local util = require("ngs.util")
local s, t, i = ls.snippet, ls.text_node, ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt
local rep = require("luasnip.extras").rep

return {
  util.treesitter_ensure("elixir"),
  -- util.lspconfig_setup("lexical", {
  --   cmd = { vim.fn.expand("~") .. "/Projects/lexical/_build/dev/package/lexical/bin/start_lexical.sh" },
  -- }),
  util.lspconfig_setup("elixirls", {
    cmd = { vim.fn.stdpath("data") .. "/mason/bin/elixir-ls" },
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
