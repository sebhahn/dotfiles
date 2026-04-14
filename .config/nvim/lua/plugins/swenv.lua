return {
  "linux-cultist/venv-selector.nvim",
  branch = "regexp",
  ft = "python",
  dependencies = {
    "nvim-telescope/telescope.nvim",
    "neovim/nvim-lspconfig",
  },
  opts = {
    name = { ".venv", "venv", "env", ".env" },
    auto_refresh = true,
    notify_user_on_activate = true,
  },
  keys = {
    { "<leader>vv", "<cmd>VenvSelect<cr>", desc = "Select venv" },
    { "<leader>va", "<cmd>VenvSelectCached<cr>", desc = "Activate cached venv" },
  },
  config = function(_, opts)
    require("venv-selector").setup(opts)

    vim.api.nvim_create_autocmd("FileType", {
      pattern = "python",
      callback = function()
        local vs = require("venv-selector")

        -- Already active, nothing to do
        if vs.venv() then return end

        -- Try to restore cached venv for this project
        pcall(vim.cmd, "VenvSelectCached")
        if vs.venv() then return end

        -- Fall back: auto-activate .venv in the project root without UI
        local root = vim.fs.root(0, { "pyproject.toml", "setup.py", "setup.cfg", ".git", ".venv" })
        if not root then return end
        local venv = root .. "/.venv"
        if vim.fn.isdirectory(venv) == 1 then
          -- retrieve_from_cache will fail; directly set via the selector
          vim.fn.setenv("VIRTUAL_ENV", venv)
          vim.fn.setenv("PATH", venv .. "/bin:" .. vim.fn.getenv("PATH"))
          vim.g.venv_selector_venv_dir = venv
          vim.notify("venv: activated " .. venv, vim.log.levels.INFO)
        end
      end,
    })
  end,
}
