return {
  "neovim/nvim-lspconfig",
  event = { "BufReadPre", "BufNewFile" },

  config = function()
    -- keymaps applied to every LSP client on attach
    vim.api.nvim_create_autocmd("LspAttach", {
      callback = function(ev)
        local map = function(mode, lhs, rhs)
          vim.keymap.set(mode, lhs, rhs, { buffer = ev.buf })
        end
        map("n", "gd", vim.lsp.buf.definition)
        map("n", "gr", vim.lsp.buf.references)
        map("n", "K", vim.lsp.buf.hover)
        map("i", "<C-k>", vim.lsp.buf.signature_help)
      end,
    })

    -- global defaults shared by all servers
    vim.lsp.config("*", {
      capabilities = require("cmp_nvim_lsp").default_capabilities(),
    })

    -------------------------------------------------------
    -- PYRIGHT
    -------------------------------------------------------
    vim.lsp.config("pyright", {
      cmd = { "pyright-langserver", "--stdio" },
      filetypes = { "python" },
      root_markers = { "pyproject.toml", "setup.py", ".git" },
      settings = {
        python = {
          venvPath = ".",
          venv = ".venv",
        },
      },
    })
    vim.lsp.enable("pyright")

    -------------------------------------------------------
    -- YAML LS
    -------------------------------------------------------
    vim.lsp.config("yamlls", {
      cmd = { "yaml-language-server", "--stdio" },
      filetypes = { "yaml" },
      root_markers = { ".git" },
      settings = {
        yaml = { validate = true },
      },
    })
    vim.lsp.enable("yamlls")

    -------------------------------------------------------
    -- LUA LS
    -------------------------------------------------------
    vim.lsp.config("lua_ls", {
      cmd = { "lua-language-server" },
      filetypes = { "lua" },
      root_markers = { ".luarc.json", ".git" },
      settings = {
        Lua = {
          diagnostics = {
            globals = { "vim" },
          },
        },
      },
    })
    vim.lsp.enable("lua_ls")
  end,
}
