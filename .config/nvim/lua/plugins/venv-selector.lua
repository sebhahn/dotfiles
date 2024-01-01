return {
	{
		"linux-cultist/venv-selector.nvim",
		build = ":TSUpdate",
		dependencies = {
			"neovim/nvim-lspconfig",
			"nvim-telescope/telescope.nvim",
			"mfussenegger/nvim-dap-python",
		},
		config = function()
			require("venv-selector").setup({
				pyenv_path = "/home/shahn/.pyenv/versions",
			})
		end,
		event = "VeryLazy", -- Optional: needed only if you want to type `:VenvSelect` without a keymapping
	},
}
