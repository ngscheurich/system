local ls = require("luasnip")
local util = require("util")
local s, t = ls.snippet, ls.text_node

return {
  util.treesitter_ensure("bash"),
  util.lspconfig_setup("bashls"),
  util.formatter_setup("sh", { "shfmt" }),
  util.linter_setup("sh", { "shellcheck" }),
  util.luasnip_add("sh", {
    s("bash-header", { t({
      "#!/usr/bin/env bash",
      "",
      "set -euo pipefail",
    }) }),
  }),
}
