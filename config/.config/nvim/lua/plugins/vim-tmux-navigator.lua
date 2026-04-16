return {
  'christoomey/vim-tmux-navigator',
  -- Only load this plugin if tmux is being used
  event = function()
    if vim.fn.exists("$TMUX") == 1 then
      return "VeryLazy"
    end
  end,
}
