return {
	{
		"nvim-telescope/telescope-ui-select.nvim",
		"nvim-telescope/telescope-project.nvim",
	},
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.6",
		dependencies = {
      "nvim-lua/plenary.nvim",
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
      "nvim-tree/nvim-web-devicons",
    },
		config = function()

      local telescope = require("telescope")
      local actions = require("telescope.actions")
      -- local sorters = require("telescope.sorters")

      telescope.setup({
          defaults = {
            file_ignore_patterns = {
              "node_modules/.*",
              "%.env",
              "yarn.lock",
              "package%-lock.json",
              "lazy%-lock.json",
              "init.sql",
              "target/.*",
              ".git/.*",
              "~/mbsync/",
              "/data-write/",
              "/data-read/",
            },
            path_display = {"smart"},
            mappings = {
              i = {
                ["<C-k>"] = actions.move_selection_previous, -- move to prev result
                ["<C-j>"] = actions.move_selection_next, -- move to next result
                ["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
              },
            },
          },
          -- file_sorter = sorters.get_fuzzy_file,
          -- generic_sorter = sorters.get_generic_fuzzy_sorter,
          -- extensions = {
          --   ["ui-select"] = { require("telescope.themes").get_dropdown({}) },
          -- },
      })

      telescope.load_extension("fzf")
      telescope.load_extension("ui-select")
      telescope.load_extension("project")
    end,
	},
}
