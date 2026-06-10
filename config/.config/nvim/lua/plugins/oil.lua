return {
  "stevearc/oil.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  lazy = false,
  opts = {
    view_options = {
      show_hidden = true,
    },
  },
  keys = {
    { "-", "<cmd>Oil<CR>", desc = "Open parent directory (oil)" },
  },
}
