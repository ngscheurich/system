local util = require("ngs.util")

return {
  {
    "habamax/vim-godot",
    config = function()
      vim.g.godot_executable = "/Applications/Godot.app"
    end
  },

  util.treesitter_ensure("gdscript"),
  util.lspconfig_setup("gdscript"),
}
