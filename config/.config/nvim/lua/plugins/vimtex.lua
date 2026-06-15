return {
  "lervag/vimtex",
  ft = "tex",
  init = function()
    vim.g.vimtex_view_method = "general"
    vim.g.vimtex_view_general_viewer = "xreader"
    vim.g.vimtex_view_general_options = "@pdf"
    vim.g.vimtex_quickfix_mode = 0
  end,
}
