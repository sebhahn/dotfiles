-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = wezterm.config_builder()

config.font = wezterm.font('JetBrainsMono Nerd Font', { weight = 'Regular' })
config.font_size = 11.0

config.color_scheme = 'OneDark (base16)'
-- config.color_scheme = 'Ollie (Gogh)'

config.window_background_opacity = 0.93

config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}

config.enable_scroll_bar = false
config.hide_tab_bar_if_only_one_tab = true
-- config.window_decorations = "RESIZE"

config.default_prog = { 'zsh', '-l' }

config.window_close_confirmation = 'NeverPrompt'
config.skip_close_confirmation_for_processes_named = { 'bash', 'sh', 'zsh', 'fish', 'tmux' }

config.initial_rows = 42
config.initial_cols = 120

config.warn_about_missing_glyphs = false

return config
