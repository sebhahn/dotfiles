return {
  "nvim-lualine/lualine.nvim",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  config = function()
    local lualine = require("lualine")
    local lazy_status = require("lazy.status") -- to configure lazy pending updates count

    lualine.setup({
      options = {
        theme = "onedark",
      },
      sections = {
        lualine_x = {
          {
            function()
              local ok, venv_selector = pcall(require, "venv-selector")
              if not ok then return "" end
              local venv = venv_selector.venv()
              if not venv then return "" end
              return venv:match("[^/]+$") or venv
            end,
            cond = function()
              return vim.bo.filetype == "python"
            end,
            icon = "",
            color = { fg = "#8fb55e" },
          },
          {
            lazy_status.updates,
            cond = lazy_status.has_updates,
            color = { fg = "#ff9e64" },
          },
          { "encoding" },
          { "fileformat" },
          { "filetype" },
        },
      },
    })
  end,
}
