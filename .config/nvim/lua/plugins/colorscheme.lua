return {
	-- {
	-- 	"catppuccin/nvim",
	-- 	lazy = false,
	-- 	name = "catppuccin",
	-- 	priority = 1001,
	-- },
	-- {
	-- 	"olimorris/onedarkpro.nvim",
	-- 	lazy = false,
	-- 	priority = 1000,
	-- 	config = function()
	-- 		vim.cmd.colorscheme("onedark")
	-- 	end,
	-- },
  {
    -- "folke/tokyonight.nvim",
    "rose-pine/neovim",
    lazy = false,
    priority = 1001,
    config = function()
      require("rose-pine").setup({
          dark_variant = "moon",
		})
      vim.cmd("colorscheme rose-pine")
	end,
  },
}
