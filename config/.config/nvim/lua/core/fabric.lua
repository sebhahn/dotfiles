local function load_patterns()
  local dir = vim.fn.expand("~/.config/fabric/patterns")
  local patterns = {}
  for _, name in ipairs(vim.fn.readdir(dir)) do
    if vim.fn.isdirectory(dir .. "/" .. name) == 1 then
      table.insert(patterns, name)
    end
  end
  table.sort(patterns)
  return patterns
end

local patterns = load_patterns()

local function fabric_selection()
  local start_line = vim.fn.line("v")
  local end_line = vim.fn.line(".")
  if start_line > end_line then
    start_line, end_line = end_line, start_line
  end

  vim.cmd("normal! \27")

  vim.ui.select(patterns, { prompt = "Fabric pattern" }, function(choice)
    if not choice then
      return
    end
    vim.cmd(string.format("%d,%d!fabric --pattern %s", start_line, end_line, choice))
  end)
end

vim.keymap.set("v", "<leader>if", fabric_selection, { desc = "Fabric: rewrite selection" })
