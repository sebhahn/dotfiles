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
      -- On the main branch setup() only takes install_dir: parsers are
      -- installed with install(), which skips the ones already present.
      require("nvim-treesitter").install({
        "bash",
        "css",
        "dockerfile",
        "gitignore",
        "html",
        "javascript",
        "json",
        "lua",
        "markdown",
        "markdown_inline",
        -- no "org": that grammar comes from nvim-orgmode, not this registry
        "python",
        "tsx",
        "typescript",
        "vim",
        "yaml",
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
