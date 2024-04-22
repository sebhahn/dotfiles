return {
	{
		"williamboman/mason.nvim",
		lazy = false,
		config = function()
			require("mason").setup({
				ensure_installed = {
					"yapf",
					"mypy",
					"ruff",
				},
			})
		end,
	},
	{
		"williamboman/mason-lspconfig.nvim",
		lazy = false,
		opts = {
			auto_install = true,
			ensure_installed = {
				"lua_ls",
				"tsserver",
				"pyright",
				"ruff_lsp",
				"ltex",
				"yamlls",
				"ansiblels",
				"jsonls",
			},
		},
	},
	{
		"neovim/nvim-lspconfig",
		lazy = false,
		config = function()

			local lspconfig = require("lspconfig")
			local lsp_capabilities = require("cmp_nvim_lsp").default_capabilities()
      local lsp_attach = function(client, bufnr)
        -- Create your keybindings here...
      end

      -- Call setup on each LSP server
      require('mason-lspconfig').setup_handlers({
          function(server_name)
            lspconfig[server_name].setup({
                on_attach = lsp_attach,
                capabilities = lsp_capabilities,
            })
          end
        })

      -- Lua LSP settings
      lspconfig.lua_ls.setup {
        settings = {
          Lua = {
            diagnostics = {
              -- Get the language server to recognize the `vim` global
              globals = {'vim'},
            },
          },
        },
      }

      -- Lua LSP settings
      lspconfig.lua_ls.setup {
        settings = {
          Lua = {
            diagnostics = {
              -- Get the language server to recognize the `vim` global
              globals = {'vim'},
            },
          },
        },
      }

		end,
	},
}
