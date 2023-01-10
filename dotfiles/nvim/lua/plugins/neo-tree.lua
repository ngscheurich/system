return {
  "nvim-neo-tree/neo-tree.nvim",

  branch = "v2.x",

  dependencies = {
    "MunifTanjim/nui.nvim",
    "nvim-lua/plenary.nvim",
    "nvim-tree/nvim-web-devicons",
    { "s1n7ax/nvim-window-picker", config = true },
  },

  config = function()
    vim.g.neo_tree_remove_legacy_commands = 1

    require("neo-tree").setup({
      default_component_configs = {
        indent = {
          with_markers = false,
        },
      },
      hijack_netrw_behavior = "disabled",
      window = {
        width = 30,
      },
    })
  end,

  keys = {
    { "<Leader>ee", "<Cmd>NeoTreeFocusToggle<CR>", desc = "Sidebar" },
    { "<Leader>ef", "<Cmd>NeoTreeFloatToggle<CR>", desc = "Float" },
    { "<Leader>es", "<Cmd>NeoTreeRevealInSplitToggle<CR>", desc = "Split" },
  },
}
