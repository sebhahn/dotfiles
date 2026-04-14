return {
  "neovim/nvim-lspconfig",
  event = { "BufReadPre", "BufNewFile" },

  config = function()
    local capabilities = require("cmp_nvim_lsp").default_capabilities()

    -------------------------------------------------------
    -- DEFAULT LSP CONFIG TABLE
    -------------------------------------------------------
    local default_config = {
      capabilities = capabilities,

      on_attach = function(_, bufnr)
        local map = function(mode, lhs, rhs)
          vim.keymap.set(mode, lhs, rhs, { buffer = bufnr })
        end

        map("n", "gd", vim.lsp.buf.definition)
        map("n", "gr", vim.lsp.buf.references)
        map("n", "K", vim.lsp.buf.hover)
      end,
    }

    local function with_default(cfg)
      return vim.tbl_deep_extend("force", default_config, cfg)
    end

    -------------------------------------------------------
    -- PYRIGHT
    -------------------------------------------------------
    vim.lsp.start(with_default({
      name = "pyright",
      cmd = { "pyright-langserver", "--stdio" },
      root_dir = vim.fs.root(0, { "pyproject.toml", "setup.py", ".git" }),
    }))

    -------------------------------------------------------
    -- LUA LS
    -------------------------------------------------------
    vim.lsp.start(with_default({
      name = "lua_ls",
      cmd = { "lua-language-server" },
      root_dir = vim.fs.root(0, { ".luarc.json", ".git" }),
      settings = {
        Lua = {
          diagnostics = {
            globals = { "vim" },
          },
        },
      },
    }))
  end,
}
