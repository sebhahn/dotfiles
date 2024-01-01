return {
	{
		"goolord/alpha-nvim",
		config = function()
			local alpha = require("alpha")
			local dashboard = require("alpha.themes.dashboard")

			dashboard.section.header.val = {
				[[                                                                       ]],
				[[                                                                       ]],
				[[                                                                       ]],
				[[                                                                       ]],
				[[                                                                       ]],
				[[                                                                       ]],
				[[                                                                       ]],
				[[                                                                     ]],
				[[       ████ ██████           █████      ██                     ]],
				[[      ███████████             █████                             ]],
				[[      █████████ ███████████████████ ███   ███████████   ]],
				[[     █████████  ███    █████████████ █████ ██████████████   ]],
				[[    █████████ ██████████ █████████ █████ █████ ████ █████   ]],
				[[  ███████████ ███    ███ █████████ █████ █████ ████ █████  ]],
				[[ ██████  █████████████████████ ████ █████ █████ ████ ██████ ]],
				[[                                                                       ]],
				[[                                                                       ]],
				[[                                                                       ]],
			}

			dashboard.section.buttons.val = {
				dashboard.button("f", "  Find file", ":Telescope find_files <cr>"),
				dashboard.button("e", "  New file", ":ene <BAR> startinsert <cr>"),
				dashboard.button("r", "  Recently used files", ":Telescope oldfiles <cr>"),
				dashboard.button("t", "  Find text", ":Telescope live_grep <cr>"),
				--dashboard.button("c", "  Configuration", ":e ~/.config/nvim/init.vim<CR>"),
				dashboard.button("q", "  Quit Neovim", ":qa<cr>"),
			}

			local function footer()
				return "Don't Stop Until You are Proud..."
			end
			dashboard.section.footer.val = footer()

			dashboard.section.footer.opts.hl = "Type"
			dashboard.section.header.opts.hl = "Include"
			dashboard.section.buttons.opts.hl = "Keyword"
			dashboard.opts.opts.noautocmd = true

			alpha.setup(dashboard.opts)
		end,
	},
}
