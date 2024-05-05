return {
	"folke/which-key.nvim",
	event = "VeryLazy",
	init = function()
		vim.o.timeout = true
		vim.o.timeoutlen = 500
	end,

	config = function()
    local wk = require("which-key")

    local mappings = {
			a = {name="+applications",
        a = { "<cmd>Alpha<CR>", "alpha" },
        l = { "<cmd>Lazy<CR>", "Lazy" },
        m = { "<cmd>Mason<CR>", "Mason" },
      },

      b = {name= "+buffer",
           b = { "<cmd>Neotree buffers reveal float<CR>", "Select buffer" },
           d = { "<cmd>bdelete<CR>", "Kill buffer" },
           n = { "<cmd>bnext<CR>", "Next buffer" },
           p = { "<cmd>bprevious<CR>", "Previous buffer" },
           f = { "<cmd>Telescope buffers<CR>", "Find buffers" },
      },

			e = {name= "+explorer",
           e = { "<cmd>NvimTreeToggle<CR>", "Neotree" },
           f = { "<cmd>NvimTreeFindFileToggle!<CR>", "Neotree file" },
           c = { "<cmd>NvimTreeCollapse<CR>", "Neotree collapse" },
           r = { "<cmd>NvimTreeRefresh<CR>", "Neotree refresh" },
      },

      f = {name = "+files",
           f = { "<cmd>lua require('telescope.builtin').find_files()<CR>", "Find files" },
           g = { "<cmd>Telescope live_grep<CR>", "Find text pattern in files"},
           r = { "<cmd>Telescope oldfiles<CR>", "Recent files" },
           s = { "<cmd>w!<CR>", "Save" },
      },

      g = {name = "+git/version-control",
           b = { "<cmd>GitBlameToggle<CR>", "Git blame" },
           g = { "<cmd>LazyGit<CR>", "Lazygit" },
           m = { "<cmd>Telescope git_commits<CR>", "Git commits" },
           s = { "<cmd>Telescope git_status<CR>", "Git status" },
      },

      h = {name = "+help",
           h = { "<cmd>Telescope help_tags<CR>", "Find help" },
           m = { "<cmd>Telescope man_pages<CR>", "Man pages" },
           r = { "<cmd>Telescope registers<CR>", "Registers" },
           k = { "<cmd>Telescope keymaps<CR>", "Keymaps" },
           c = { "<cmd>Telescope commands<CR>", "Commands" },
      },

			-- ["l"] = "+lsp",
	-- 		["lf"] = { "<cmd>lua vim.lsp.buf.format({async=true})<cr>", "code format" },
	-- 		["la"] = { "<cmd>lua vim.lsp.buf.rename()<cr>", "rename" },
	-- 		["lk"] = { "<cmd>lua vim.lsp.buf.hover()<cr>", "hover" },
	-- 		["ld"] = { "<cmd>lua vim.lsp.buf.definition()<cr>", "definition" },
	-- 		["lr"] = { "<cmd>lua vim.lsp.buf.references()<cr>", "references" },
	-- 		["lc"] = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "code action" },
	-- 		["ll"] = { "<cmd>lua vim.lsp.buf.codelens.run()<cr>", "codelens action" },
	-- 		["ls"] = { "<cmd>Telescope lsp_document_symbols<cr>", "document symbols" },
	-- 		["lS"] = { "<cmd>Telescope lsp_dynamic_document_symbols<cr>", "workspace symbols" },

      o = {name = "+org",
           a = { "<cmd>lua require('orgmode').action('agenda.prompt')<CR>", "agenda" },
           o = { "<cmd>lua require('orgmode').action('capture.refile')<CR>", "capture" },
      },

      q = {name = "+quit",
           q = { "<cmd>wqall!<cr>", "quit" },
      },

      Q = {name = "+quickfix",
           o = { "<cmd>copen<CR>", "Open quickfix list" },
           f = { "<cmd>cfirst<CR>", "Jump to first item" },
           n = { "<cmd>cnext<CR>", "Jump to next item" },
           p = { "<cmd>cprev<CR>", "Jump to previous item" },
           l = { "<cmd>clast<CR>", "Jump to last item" },
           c = { "<cmd>cclose<CR>", "Close quickfix list" },
      },

      r = {name =  "+run",
           r = { "<cmd>RunCode<CR>", "Run code" },
           f = { "<cmd>RunFile<CR>", "Run file" },
           t = { "<cmd>RunFile tab<CR>", "Run file tab" },
           p = { "<cmd>RunProject<CR>", "Run project" },
           c = { "<cmd>RunClose<CR>", "Run close" },
           m = { "<cmd>CRFiletype<CR>", "CRFiletype" },
           n = { "<cmd>CRProjects<CR>", "CRProjects" },
           l = { "<cmd>lua require('lint').try_lint()<CR>", "Lint file" },
      },

      s = {name = "+search",
           b = { "<cmd>Telescope current_buffer_fuzzy_find<CR>", "Find in current buffer" },
           m = { "<cmd>Telescope marks<CR>", "Telescope bookmarks" },
      },

      -- s = {
      --   name = "Search",
      --   b = { "<cmd>Telescope git_branches<cr>", "Checkout branch" },
      --   c = { "<cmd>Telescope colorscheme<cr>", "Colorscheme" },
      --   h = { "<cmd>Telescope help_tags<cr>", "Find Help" },
      --   M = { "<cmd>Telescope man_pages<cr>", "Man Pages" },
      --   r = { "<cmd>Telescope oldfiles<cr>", "Open Recent File" },
      --   R = { "<cmd>Telescope registers<cr>", "Registers" },
      --   k = { "<cmd>Telescope keymaps<cr>", "Keymaps" },
      --   C = { "<cmd>Telescope commands<cr>", "Commands" },
      -- },

			p = {name = "+project",
           p = { "<cmd>lua require('telescope').extensions.project.project{}<CR>", "display" }
      },

      t = {name =  "+terminal",
           p = { "<cmd>lua _NCDU_TOGGLE()<CR>", "NCDU" },
           t = { "<cmd>lua _HTOP_TOGGLE()<CR>", "Htop" },
           p = { "<cmd>lua _PYTHON_TOGGLE()<CR>", "Python" },
           f = { "<cmd>ToggleTerm direction=float<CR>", "Float" },
           h = { "<cmd>ToggleTerm size=10 direction=horizontal<CR>", "Horizontal" },
           v = { "<cmd>ToggleTerm size=80 direction=vertical<CR>", "Vertical" },
      },

      T = {name = "+toggle/themes",
           c = { "<cmd>Telescope colorscheme<CR>", "colorscheme" },
           n = { "<cmd>set nu!<CR>", "toggle-line-number" },
           r = { "<cmd>set rnu!<CR>", "toggle-relative-number" },
      },

      v = {name = "+venv",
           v = { "<cmd>lua require('swenv.api').pick_venv()<CR>", "Select venv" },
           g = { "<cmd>lua require('swenv.api').get_current_venv()<CR>", "Get venv" },
      },

      w = {name = "+windows",
           v = { "<C-w>v", "Split window vertically" },
           s = { "<C-w>s", "Split window horizontally" },
           w = { "<C-w>w", "Other window" },
           x = { "<cmd>close<CR>", "Close current split" },
           m = { "<cmd>MaximizerToggle<CR>", "Maximize window" },
           e = { "<C-w>=", "Make splits equal size" },
           r = { "<C-w><C-r>", "Rotate window forward" },
           R = { "<C-w>r", "Rotate window backward" },
           k = { "<C-w>-", "Split windows height shorter" },
           j = { "<C-w>+", "Split windows height taller" },
           h = { "<C-w>>5", "Split windows width bigger" },
           l = { "<C-w><5", "Split windows width smaller" },
      },

    }

		local opts = {
			mode = "n", -- NORMAL mode
			prefix = "<leader>",
			buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
			silent = true, -- use `silent` when creating keymaps
			noremap = true, -- use `noremap` when creating keymaps
			nowait = true, -- use `nowait` when creating keymaps
		}
    wk.register(mappings, opts)

		local setup = {
			plugins = {
				marks = true, -- shows a list of your marks on ' and `
				registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
				spelling = {
					enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
					suggestions = 20, -- how many suggestions should be shown in the list?
				},
				-- the presets plugin, adds help for a bunch of default keybindings in Neovim
				-- No actual key bindings are created
				presets = {
					operators = false, -- adds help for operators like d, y, ... and registers them for motion / text object completion
					motions = true, -- adds help for motions
					text_objects = true, -- help for text objects triggered after entering an operator
					windows = true, -- default bindings on <c-w>
					nav = true, -- misc bindings to work with windows
					z = true, -- bindings for folds, spelling and others prefixed with z
					g = true, -- bindings for prefixed with g
				},
			},
			-- add operators that will trigger motion and text object completion
			-- to enable all native operators, set the preset / operators plugin above
			-- operators = { gc = "Comments" },
			key_labels = {
				-- override the label used to display some keys. It doesn't effect WK in any other way.
				-- For example:
				-- ["<space>"] = "SPC",
				-- ["<cr>"] = "RET",
				-- ["<tab>"] = "TAB",
			},
			icons = {
				breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
				separator = "➜", -- symbol used between a key and it's label
				group = "+", -- symbol prepended to a group
			},
			popup_mappings = {
				scroll_down = "<c-d>", -- binding to scroll down inside the popup
				scroll_up = "<c-u>", -- binding to scroll up inside the popup
			},
			window = {
				border = "rounded", -- none, single, double, shadow
				position = "bottom", -- bottom, top
				margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
				padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
				winblend = 0,
			},
			layout = {
				height = { min = 4, max = 25 }, -- min and max height of the columns
				width = { min = 20, max = 50 }, -- min and max width of the columns
				spacing = 3, -- spacing between columns
				align = "left", -- align columns left, center or right
			},
			ignore_missing = false, -- enable this to hide mappings for which you didn't specify a label
			hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " }, -- hide mapping boilerplate
			show_help = true, -- show help message on the command line when the popup is visible
      show_keys = true, -- show the currently pressed key and its label as a message in the command line
			triggers = "auto", -- automatically setup triggers
			-- triggers = {"<leader>"} -- or specify a list manually
      triggers_nowait = {
        -- marks
        "`",
        "'",
        "g`",
        "g'",
        -- registers
        '"',
        "<c-r>",
        -- spelling
        "z=",
      },
			triggers_blacklist = {
				-- list of mode / prefixes that should never be hooked by WhichKey
				-- this is mostly relevant for key maps that start with a native binding
				-- most people should not need to change this
				i = { "j", "k" },
				v = { "j", "k" },
			},
		}
		wk.setup(setup)
	end,
}
