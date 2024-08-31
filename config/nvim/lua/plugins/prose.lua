-- ===================================================================
--  Long-Form Writing Affordances
-- ===================================================================

return {
  -- =================================================================
  --  Markdown Preview for (Neo)vim
  -- -----------------------------------------------------------------
  --  https://github.com/iamcco/markdown-preview.nvim
  --  Preview Markdown in a browser
  -- -----------------------------------------------------------------
  {
    "iamcco/markdown-preview.nvim",
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    ft = { "markdown" },
    build = function()
      vim.fn["mkdp#util#install"]()
    end,
  },
}
