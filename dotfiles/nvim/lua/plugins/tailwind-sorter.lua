return {
  "laytan/tailwind-sorter.nvim",

  ft = { "html", "heex" },

  dependencies = {
    "nvim-treesitter/nvim-treesitter",
    "nvim-lua/plenary.nvim",
  },

  build = "cd formatter && npm i && npm run build",

  config = {
    on_save_enabled = true,
    on_save_pattern = { "*.html", "*.heex", "*.ex" },
  },
}
