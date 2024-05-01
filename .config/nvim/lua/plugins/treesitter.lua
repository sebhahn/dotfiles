return {
	{
		"nvim-treesitter/nvim-treesitter",
    event = { "BufReadPre", "BufNewFile" },
		build = ":TSUpdate",
    dependencies = {
      "windwp/nvim-ts-autotag",
    },

		config = function()
			local treesitter = require("nvim-treesitter.configs")

      -- configure treesitter
			treesitter.setup({
				ensure_installed = {
          "bash",
          "dockerfile",
          "gitignore",
          "json",
					"python",
					"lua",
          "markdown",
          "markdown_inline",
					"org",
					"vim",
					"yaml",
				},
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "<C-space>",
            node_incremental = "<C-space>",
            scope_incremental = false,
            node_decremental = "<bs>",
          },
        },
				auto_install = true,
				highlight = {
					enable = true,
					additional_vim_regex_highlighting = { "org" },
				},
        -- enable indentation
				indent = { enable = true },
        -- enable autotagging
				autotag = { enable = true },
			})
		end,
	},
}
