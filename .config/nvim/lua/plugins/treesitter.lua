return {
	{
		"nvim-treesitter/nvim-treesitter",
    event = { "BufReadPre", "BufNewFile" },
		build = ":TSUpdate",
    dependencies = {
      {
        "windwp/nvim-ts-autotag",
        opts = {},
      },
    },

		config = function()
      require("nvim-treesitter").setup({
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
        auto_install = true,
      })

      -- Activate treesitter highlighting for every buffer whose parser is available
      vim.api.nvim_create_autocmd("FileType", {
        callback = function(ev)
          local ok = pcall(vim.treesitter.start, ev.buf)
          if not ok then return end
          if vim.bo[ev.buf].filetype == "org" then
            vim.bo[ev.buf].syntax = "on"
          end
        end,
      })
		end,
	},
}
