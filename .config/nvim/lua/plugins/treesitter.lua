return {
	{
		"nvim-treesitter/nvim-treesitter",
    event = 'VeryLazy',
		build = ":TSUpdate",
		config = function()
			local config = require("nvim-treesitter.configs")
			config.setup({
				ensure_installed = {
					"cpp",
					"python",
					"lua",
					"java",
					"yaml",
					"org",
				},
				auto_install = true,
				highlight = {
					enable = true,
					additional_vim_regex_highlighting = { "org" },
				},
				indent = { enable = true },
			})
		end,
	},
}
