return {
  "numToStr/Comment.nvim",
  event = { "BufReadPre", "BufNewFile" },
  dependencies = {
    "JoosepAlviste/nvim-ts-context-commentstring",
  },
  config = function()
    local comment = require("Comment")
    local ts_context_commentstring = require("ts_context_commentstring.integrations.comment_nvim")

    -- enable comment
    comment.setup({
        -- for commenting tsx, jsx, svelte, html files
        pre_hook = ts_context_commentstring.create_pre_hook(),
    })

    local api = require("Comment.api")
    local esc = vim.api.nvim_replace_termcodes("<Esc>", true, false, true)

    vim.keymap.set("n", "<leader>cl", api.toggle.linewise.current, { desc = "Comment line" })
    vim.keymap.set("x", "<leader>cl", function()
      vim.api.nvim_feedkeys(esc, "nx", false)
      api.toggle.linewise(vim.fn.visualmode())
    end, { desc = "Comment selection" })
  end,
}
