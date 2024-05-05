return {
  "AckslD/swenv.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  event = { "BufReadPre", "BufNewFile" },
  config = function()

    local swenv = require("swenv")
    local swenv_api = require('swenv.api')

    swenv.setup({
        -- Should return a list of tables with a `name` and a `path` entry each.
        -- Gets the argument `venvs_path` set below.
        -- By default just lists the entries in `venvs_path`.
        get_venvs = function(venvs_path)
          return swenv_api.get_venvs(venvs_path)
        end,
        -- Path passed to `get_venvs`.
        venvs_path = vim.fn.expand('~/.pyenv/versions'),
        -- Something to do after setting an environment, for example call vim.cmd.LspRestart
        post_set_venv = vim.cmd.LspRestart,
    })

    -- local keymap = vim.keymap -- for conciseness
    -- keymap.set("n", "<leader>vs", swenv_api.pick_venv , { desc = "Set env" })
    -- keymap.set("n", "<leader>vg", swenv_api.get_current_venv , { desc = "Get env" })

  end,
}
