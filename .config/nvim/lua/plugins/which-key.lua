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
      ["e"] = "File explorer",
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

	end,

	-- config = function()
	-- 	local wk = require("which-key")
	-- 	local mappings = {

	-- 		["e"] = { "<cmd>Neotree filesystem reveal left<cr>", "Neotree" }, -- File Explorer

	-- 		["a"] = "+applications",
	-- 		["aa"] = { "<cmd>Alpha<cr>", "alpha" },
	-- 		["al"] = { "<cmd>Lazy<vr>", "plugin manager" },

	-- 		["b"] = "+buffer",
	-- 		["bb"] = { "<cmd>Neotree buffers reveal float<cr>", "Select buffer" },
	-- 		["bd"] = { "<cmd>bdelete<cr>", "Kill buffer" },
	-- 		["bn"] = { "<cmd>bnext<cr>", "Next buffer" },
	-- 		["bp"] = { "<cmd>bprevious<cr>", "Previous buffer" },
	-- 		["bf"] = { "<cmd>Telescope buffers<cr>", "Find buffers" },

	-- 		["f"] = "+files",
	-- 		["ff"] = { "<cmd>lua require('telescope.builtin').find_files()<cr>", "Find files" },
	-- 		["fg"] = { "<cmd>Telescope live_grep<cr>", "Find Text Pattern In All Files" },
	-- 		["fr"] = { "<cmd>Telescope oldfiles<cr>", "Open Recent File" },
	-- 		["fs"] = { "<cmd>w!<cr>", "Save" },
	-- 		["fo"] = { "<cmd>Telescope oldfiles<cr>", "Find oldfiles" },

	-- 		["g"] = "+git/version-control",
	-- 		["gm"] = { "<cmd>Telescope git_commits<cr>", "Git commits" },
	-- 		["gs"] = { "<cmd>Telescope git_status<cr>", "Git status" },

	-- 		["l"] = "+lsp",
	-- 		["lf"] = { "<cmd>lua vim.lsp.buf.format({async=true})<cr>", "code format" },
	-- 		["la"] = { "<cmd>lua vim.lsp.buf.rename()<cr>", "rename" },
	-- 		["lk"] = { "<cmd>lua vim.lsp.buf.hover()<cr>", "hover" },
	-- 		["ld"] = { "<cmd>lua vim.lsp.buf.definition()<cr>", "definition" },
	-- 		["lr"] = { "<cmd>lua vim.lsp.buf.references()<cr>", "references" },
	-- 		["lc"] = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "code action" },
	-- 		["ll"] = { "<cmd>lua vim.lsp.buf.codelens.run()<cr>", "codelens action" },
	-- 		["ls"] = { "<cmd>Telescope lsp_document_symbols<cr>", "document symbols" },
	-- 		["lS"] = { "<cmd>Telescope lsp_dynamic_document_symbols<cr>", "workspace symbols" },

	-- 		["p"] = "+project",
	-- 		["pp"] = { "<cmd>lua require('telescope').extensions.project.project{}<cr>", "display" },

	-- 		["q"] = "+quit",
	-- 		["qq"] = { "<cmd>wqall!<cr>", "quit" },

	-- 		["h"] = "+help",
	-- 		["hh"] = { "<cmd>Telescope help_tags<cr>", "find help" },
	-- 		["hm"] = { "<cmd>Telescope man_pages<cr>", "man pages" },
	-- 		["hr"] = { "<cmd>Telescope registers<cr>", "registers" },
	-- 		["hk"] = { "<cmd>Telescope keymaps<cr>", "keymaps" },
	-- 		["hc"] = { "<cmd>Telescope commands<cr>", "commands" },

	-- 		["o"] = "+org",
	-- 		["oa"] = { "<cmd>lua require('orgmode').action('agenda.prompt')<cr>", "agenda" },
	-- 		["oc"] = { "<cmd>lua require('orgmode').action('capture.refile')<cr>", "capture" },

	-- 		-- ["s"] = "+search",
	-- 		-- ["sb"] = { "<cmd>Telescope current_buffer_fuzzy_find<cr>", "Find in current buffer" },
	-- 		-- ["sm"] = { "<cmd>Telescope marks<cr>", "telescope bookmarks" },

	-- 		["t"] = "+terminal",
	-- 		["tt"] = { "<cmd>lua _HTOP_TOGGLE()<cr>", "Htop" },
	-- 		["tp"] = { "<cmd>lua _PYTHON_TOGGLE()<cr>", "Python" },
	-- 		["tf"] = { "<cmd>ToggleTerm direction=float<cr>", "Float" },
	-- 		["th"] = { "<cmd>ToggleTerm size=10 direction=horizontal<cr>", "Horizontal" }, -- Horizontal Terminal,
	-- 		["tv"] = { "<cmd>ToggleTerm size=80 direction=vertical<cr>", "Vertical" }, -- Vertical Terminal

	-- 		["T"] = "+toggle/themes",
	-- 		["Tc"] = { "<cmd>Telescope colorscheme<cr>", "colorscheme" },
	-- 		["Tn"] = { "<cmd>set nu!<cr>", "toggle-line-number" },
	-- 		["Tr"] = { "<cmd>set rnu!<cr>", "toggle-relative-number" },

	-- 		["v"] = "+venv",
	-- 		["vs"] = { "<cmd>VenvSelect<cr>", "venv-select" },
	-- 		["vc"] = { "<cmd>VenvSelectCached<cr>", "venv-cached" },

	-- 		["w"] = "+windows",
	-- 		["wv"] = { "<cmd>vsplit<cr>", "split-window-right" },
	-- 		["ws"] = { "<cmd>split<cr>", "split-window-below" },
	-- 		["ww"] = { "<c-w>w", "other-window" },
	-- 		["wd"] = { "<cmd>q<cr>", "delete-window" },
	-- 		["wm"] = { "<c-w><c-o>", "maximize-window" },
	-- 		["wh"] = { "<c-w>=", "equalize-horizontally" },
	-- 		["wr"] = { "<c-w><c-r>", "rotate-window-forward" },
	-- 		["wR"] = { "<c-w>r", "rotate-window-backward" },
	-- 	}

	-- 	local opts = {
	-- 		mode = "n", -- NORMAL mode
	-- 		prefix = "<leader>",
	-- 		buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
	-- 		silent = true, -- use `silent` when creating keymaps
	-- 		noremap = true, -- use `noremap` when creating keymaps
	-- 		nowait = true, -- use `nowait` when creating keymaps
	-- 	}
	-- 	-- wk.register(mappings, opts)

	-- 	local setup = {
	-- 		plugins = {
	-- 			marks = true, -- shows a list of your marks on ' and `
	-- 			registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
	-- 			spelling = {
	-- 				enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
	-- 				suggestions = 20, -- how many suggestions should be shown in the list?
	-- 			},
	-- 			-- the presets plugin, adds help for a bunch of default keybindings in Neovim
	-- 			-- No actual key bindings are created
	-- 			presets = {
	-- 				operators = false, -- adds help for operators like d, y, ... and registers them for motion / text object completion
	-- 				motions = true, -- adds help for motions
	-- 				text_objects = true, -- help for text objects triggered after entering an operator
	-- 				windows = true, -- default bindings on <c-w>
	-- 				nav = true, -- misc bindings to work with windows
	-- 				z = true, -- bindings for folds, spelling and others prefixed with z
	-- 				g = true, -- bindings for prefixed with g
	-- 			},
	-- 		},
	-- 		-- add operators that will trigger motion and text object completion
	-- 		-- to enable all native operators, set the preset / operators plugin above
	-- 		-- operators = { gc = "Comments" },
	-- 		key_labels = {
	-- 			-- override the label used to display some keys. It doesn't effect WK in any other way.
	-- 			-- For example:
	-- 			-- ["<space>"] = "SPC",
	-- 			-- ["<cr>"] = "RET",
	-- 			-- ["<tab>"] = "TAB",
	-- 		},
	-- 		icons = {
	-- 			breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
	-- 			separator = "➜", -- symbol used between a key and it's label
	-- 			group = "+", -- symbol prepended to a group
	-- 		},
	-- 		popup_mappings = {
	-- 			scroll_down = "<c-d>", -- binding to scroll down inside the popup
	-- 			scroll_up = "<c-u>", -- binding to scroll up inside the popup
	-- 		},
	-- 		window = {
	-- 			border = "rounded", -- none, single, double, shadow
	-- 			position = "bottom", -- bottom, top
	-- 			margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
	-- 			padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
	-- 			winblend = 0,
	-- 		},
	-- 		layout = {
	-- 			height = { min = 4, max = 25 }, -- min and max height of the columns
	-- 			width = { min = 20, max = 50 }, -- min and max width of the columns
	-- 			spacing = 3, -- spacing between columns
	-- 			align = "left", -- align columns left, center or right
	-- 		},
	-- 		ignore_missing = true, -- enable this to hide mappings for which you didn't specify a label
	-- 		hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " }, -- hide mapping boilerplate
	-- 		show_help = true, -- show help message on the command line when the popup is visible
	-- 		triggers = "auto", -- automatically setup triggers
	-- 		-- triggers = {"<leader>"} -- or specify a list manually
	-- 		triggers_blacklist = {
	-- 			-- list of mode / prefixes that should never be hooked by WhichKey
	-- 			-- this is mostly relevant for key maps that start with a native binding
	-- 			-- most people should not need to change this
	-- 			i = { "j", "k" },
	-- 			v = { "j", "k" },
	-- 		},
	-- 	}
	-- 	wk.setup(setup)
	-- end,
}
