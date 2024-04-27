-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- This is where you actually apply your config choices

--config.font = wezterm.font 'Hack Nerd Font Mono'
config.font = wezterm.font('JetBrainsMono Nerd Font', { weight = 'Regular' })

config.font_size = 11.0

-- config.color_scheme = 'Material (base16)'
-- config.color_scheme = 'MaterialDark'
-- config.color_scheme = 'Bamboo'
config.color_scheme = "OneHalfDark"

config.hide_tab_bar_if_only_one_tab = true
config.term = 'xterm-256color'
config.window_background_opacity = 0.95

config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}

config.enable_scroll_bar = false

config.default_prog = { 'zsh', '-l' }

config.window_close_confirmation = 'NeverPrompt'
config.skip_close_confirmation_for_processes_named = { 'bash', 'sh', 'zsh', 'fish', 'tmux' }

config.initial_rows = 42
config.initial_cols = 120

-- and finally, return the configuration to wezterm
return config
