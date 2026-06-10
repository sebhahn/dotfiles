return {
  "folke/flash.nvim",
  event = "VeryLazy",
  opts = {},
  keys = {
    {
      "<leader><leader>",
      mode = { "n", "x", "o" },
      function()
        require("flash").jump()
      end,
      desc = "Flash jump",
    },
    {
      "<leader>/",
      mode = { "n", "x", "o" },
      function()
        require("flash").treesitter()
      end,
      desc = "Flash treesitter",
    },
  },
}
