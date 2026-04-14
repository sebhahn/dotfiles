return {
	{
		"goolord/alpha-nvim",
    dependencies = {
      'nvim-mini/mini.icons',
      'nvim-lua/plenary.nvim'
    },
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
				dashboard.button("f", "  Find file", "<cmd>Telescope find_files<CR>"),
				dashboard.button("e", "  New file", "<cmd>ene <BAR> startinsert<CR>"),
				dashboard.button("r", "  Recently used files", "<cmd>Telescope oldfiles<CR>"),
				dashboard.button("t", "  Find text", "<cmd>Telescope live_grep<CR>"),
				--dashboard.button("c", "  Configuration", ":e ~/.config/nvim/init.vim<CR>"),
				dashboard.button("q", "  Quit", "<cmd>qa<CR>"),
			}

			local function footer()
				return "Befriend your shadows - Follow meaning not pleasure - Become whole not perfect"
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
