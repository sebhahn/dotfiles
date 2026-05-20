return {
  "nvim-neotest/neotest",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-neotest/nvim-nio",
    "antoinemadec/FixCursorHold.nvim",
    "nvim-treesitter/nvim-treesitter",
    "nvim-neotest/neotest-python",
  },
  config = function()
    require("neotest").setup({
      adapters = {
        require("neotest-python")({
          runner = "pytest",
          python = function()
            local venv = vim.env.VIRTUAL_ENV
            if venv then
              return venv .. "/bin/python"
            end
            return "python3"
          end,
        }),
      },
    })
  end,
}
