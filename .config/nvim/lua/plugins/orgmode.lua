return {
	"nvim-orgmode/orgmode",
	event = "VeryLazy",
	config = function()
		-- Load treesitter grammar for org
		require("orgmode").setup_ts_grammar()

		require("orgmode").setup({
			org_todo_keywords = { "TODO", "INPR", "WAIT", "HOLD", "|", "DONE", "CNCL" },
			org_hide_leading_stars = true,
			org_log_into_drawer = "LOGBOOK",
			org_agenda_files = "~/ownCloud/org/roam/areas/agenda/*",
			org_default_notes_file = "~/ownCloud/org/roam/areas/agenda/refile.org",
		})
	end,
}
