return {
	{
		"nvim-telescope/telescope-ui-select.nvim",
		"nvim-telescope/telescope-file-browser.nvim",
	},
	{
		"nvim-telescope/telescope.nvim",
		dependencies = {
      "nvim-lua/plenary.nvim",
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
      "nvim-tree/nvim-web-devicons",
      "ahmedkhalf/project.nvim",
    },
		config = function()

      require("project_nvim").setup({
        detection_methods = { "pattern", "lsp" },
        patterns = { ".git", ".projectile", "pyproject.toml", "setup.py", "Makefile", ".svn", ".hg" },
        silent_chdir = true,
        show_hidden = true,
      })

      local telescope = require("telescope")
      local actions = require("telescope.actions")

      telescope.setup({
          defaults = {
            path_display = {"smart"},
            preview = {
              treesitter = false,
            },
            mappings = {
              i = {
                ["<C-k>"] = actions.move_selection_previous,
                ["<C-j>"] = actions.move_selection_next,
                ["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
              },
            },
          },
      })

      telescope.load_extension("fzf")
      telescope.load_extension("ui-select")
      telescope.load_extension("projects")
      telescope.load_extension("file_browser")
    end,
	},
}
