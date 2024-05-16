vim.cmd("let g:netrw_liststyle = 3")

-- Set leader key to space
vim.g.mapleader = " "

local keymap = vim.keymap

-- Navigate vim panes better
keymap.set("n", "<c-k>", ":wincmd k<CR>")
keymap.set("n", "<c-j>", ":wincmd j<CR>")
keymap.set("n", "<c-h>", ":wincmd h<CR>")
keymap.set("n", "<c-l>", ":wincmd l<CR>")
-- keymap.set("n", "<leader>h", ":nohlsearch<CR>")
-- keymap.set("n", "<leader>nh", ":nohl<CR>", {desc = "Clear search highlights"})

-- increment/decrement numbers
-- keymap.set("n", "<leader>+", "<C-a>", {desc = "Increment number"})
-- keymap.set("n", "<leader>-", "<C-x>", {desc = "Decrement number"})

-- window management
-- keymap.set("n", "<leader>wv", "<C-w>v", {desc = "Split window vertically"})
-- keymap.set("n", "<leader>ws", "<C-w>s", {desc = "Split window horizontally"})
-- keymap.set("n", "<leader>we", "<C-w>=", {desc = "Make splits equal size"})
-- keymap.set("n", "<leader>wx", "<cmd>close<CR>", {desc = "Close current split"})
-- keymap.set("n", "<leader>wj", "<C-w>-", {desc = "Split windows height shorter"})
-- keymap.set("n", "<leader>wk", "<C-w>+", {desc = "Split windows height taller"})
-- keymap.set("n", "<leader>wl", "<C-w>>5", {desc = "Split windows width bigger"})
-- keymap.set("n", "<leader>wh", "<C-w><5", {desc = "Split windows width smaller"})

-- vim-maximizer
-- keymap.set("n", "<leader>wm", "<cmd>MaximizerToggle<CR>", {desc = "Maximize window"}) 

vim.wo.number = true

-- tab management
-- keymap.set("n", "<leader>to", "<cmd>tabnew<CR>", {desc = "Open new tab" })
-- keymap.set("n", "<leader>tx", "<cmd>tabclose<CR>", {desc = "Close tab"})
-- keymap.set("n", "<leader>tt", "<cmd>tabn<CR>", {desc = "Next tab"})
-- keymap.set("n", "<leader>tp", "<cmd>tabp<CR>", {desc = "Previous tab"})
-- keymap.set("n", "<leader>tf", "<cmd>tabnew %<CR>", {desc = "tab??"})

-- diff keymaps
-- keymap.set("n", "<leader>cc", "<cmd>diffput<CR>") -- put diff from current to other during diff
-- keymap.set("n", "<leader>cj", "<cmd>diffget 1<CR>") -- get diff from left (local) during merge
-- keymap.set("n", "<leader>ck", "<cmd>diffget 3<CR>") -- get diff from right (remote) during merge
-- keymap.set("n", "<leader>cn", "]c") -- next diff hunk
-- keymap.set("n", "<leader>cp", "[c") -- previous diff hunk

-- quickfix keymaps
-- keymap.set("n", "<leader>qq", "<cmd>q!<CR>", {desc = "Quit without saving"})
-- keymap.set("n", "<leader>qo", "<cmd>copen<CR>", {desc = "Open quickfix list"})
-- keymap.set("n", "<leader>qf", "<cmd>cfirst<CR>", {desc = "Jump to first item"})
-- keymap.set("n", "<leader>qn", "<cmd>cnext<CR>", {desc = "Jump to next item"})
-- keymap.set("n", "<leader>qp", "<cmd>cprev<CR>", {desc = "Jump to previous item"})
-- keymap.set("n", "<leader>ql", "<cmd>clast<CR>", {desc = "Jump to last item"})
-- keymap.set("n", "<leader>qc", "<cmd>cclose<CR>", {desc = "Close quickfix list"})

-- files/telescope
local builtin = require('telescope.builtin')
local utils = require("telescope.utils")

-- keymap.set("n", "<leader>wq", "<cmd>wq<CR>", {desc = "File save and quit"})
-- keymap.set("n", "<leader>fs", "<cmd>w<CR>", {desc = "File save"})
-- keymap.set("n", "<leader>ww", "<cmd>w<CR>", {desc = "File save"})

-- keymap.set('n', '<leader>ff', builtin.find_files, {desc = "Fuzzy find files in cwd"})
-- keymap.set('n', '<leader>fr', "<cmd>Telescope oldfiles<CR>", {desc = "Fuzzy find recent files"})
-- keymap.set('n', '<leader>fg', builtin.live_grep, {desc = "Find string in cwd"})
-- keymap.set('n', '<leader>fc', "<cmd>Telescope grep_string<CR>", {desc = "Find string under cursor in cwd"})
-- keymap.set('n', '<leader>fb', builtin.buffers, {desc = "Find buffers"})
-- keymap.set('n', '<leader>fz', "<cmd>Telescope current_buffer_fuzzy_find<CR>", {desc = "Fuzzy find buffers"})
-- keymap.set('n', '<leader>fh', builtin.help_tags, {desc = "Help tags"})
-- keymap.set('n', '<leader>fo', "<cmd>Telescope lsp_document_symbols<CR>", {desc = "LSP document symbols"})
-- keymap.set('n', '<leader>fi', "<cmd>Telescope lsp_incoming_calls<CR>", {desc = "LSP incoming calls"})
-- keymap.set('n', '<leader>fm', function() builtin.treesitter({default_text=":method:"}) end, {desc = "treesitter?"})
-- keymap.set("n", '<leader>fd', function() builtin.find_files({ cwd = utils.buffer_dir() }) end, {desc = "Find files in cwd"})
-- keymap.set("n", "<leader>ft", "<cmd>TodoTelescope<cr>", { desc = "Find todos" })

-- keymap.set("n", "gx", "<cmd>!open <c-r><c-a><CR>")-- open URL under cursor

-- git-blame
-- keymap.set("n", "<leader>gb", "<cmd>GitBlameToggle<CR>") -- toggle git blame

-- harpoon
-- keymap.set("n", "<leader>ha", require("harpoon.mark").add_file)
-- keymap.set("n", "<leader>hh", require("harpoon.ui").toggle_quick_menu)
-- keymap.set("n", "<leader>h1", function() require("harpoon.ui").nav_file(1) end)
-- keymap.set("n", "<leader>h2", function() require("harpoon.ui").nav_file(2) end)
-- keymap.set("n", "<leader>h3", function() require("harpoon.ui").nav_file(3) end)
-- keymap.set("n", "<leader>h4", function() require("harpoon.ui").nav_file(4) end)
-- keymap.set("n", "<leader>h5", function() require("harpoon.ui").nav_file(5) end)
-- keymap.set("n", "<leader>h6", function() require("harpoon.ui").nav_file(6) end)
-- keymap.set("n", "<leader>h7", function() require("harpoon.ui").nav_file(7) end)
-- keymap.set("n", "<leader>h8", function() require("harpoon.ui").nav_file(8) end)
-- keymap.set("n", "<leader>h9", function() require("harpoon.ui").nav_file(9) end)

-- lsp
-- keymap.set('n', '<leader>gg', '<cmd>lua vim.lsp.buf.hover()<CR>')
-- keymap.set('n', '<leader>gd', '<cmd>lua vim.lsp.buf.definition()<CR>')
-- keymap.set('n', '<leader>gD', '<cmd>lua vim.lsp.buf.declaration()<CR>')
-- keymap.set('n', '<leader>gi', '<cmd>lua vim.lsp.buf.implementation()<CR>')
-- keymap.set('n', '<leader>gt', '<cmd>lua vim.lsp.buf.type_definition()<CR>')
-- keymap.set('n', '<leader>gr', '<cmd>lua vim.lsp.buf.references()<CR>')
-- keymap.set('n', '<leader>gs', '<cmd>lua vim.lsp.buf.signature_help()<CR>')
-- keymap.set('n', '<leader>rr', '<cmd>lua vim.lsp.buf.rename()<CR>')
-- keymap.set('n', '<leader>gf', '<cmd>lua vim.lsp.buf.format({async = true})<CR>')
-- keymap.set('v', '<leader>gf', '<cmd>lua vim.lsp.buf.format({async = true})<CR>')
-- keymap.set('n', '<leader>ga', '<cmd>lua vim.lsp.buf.code_action()<CR>')
-- keymap.set('n', '<leader>gl', '<cmd>lua vim.diagnostic.open_float()<CR>')
-- keymap.set('n', '<leader>gp', '<cmd>lua vim.diagnostic.goto_prev()<CR>')
-- keymap.set('n', '<leader>gn', '<cmd>lua vim.diagnostic.goto_next()<CR>')
-- keymap.set('n', '<leader>tr', '<cmd>lua vim.lsp.buf.document_symbol()<CR>')
-- keymap.set('i', '<C-Space>', '<cmd>lua vim.lsp.buf.completion()<CR>')

-- debugging
-- keymap.set("n", "<leader>bb", "<cmd>lua require'dap'.toggle_breakpoint()<cr>")
-- keymap.set("n", "<leader>bc", "<cmd>lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<cr>")
-- keymap.set("n", "<leader>bl", "<cmd>lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<cr>")
-- keymap.set("n", '<leader>br', "<cmd>lua require'dap'.clear_breakpoints()<cr>")
-- keymap.set("n", '<leader>ba', '<cmd>Telescope dap list_breakpoints<cr>')
-- keymap.set("n", "<leader>dc", "<cmd>lua require'dap'.continue()<cr>")
-- keymap.set("n", "<leader>dj", "<cmd>lua require'dap'.step_over()<cr>")
-- keymap.set("n", "<leader>dk", "<cmd>lua require'dap'.step_into()<cr>")
-- keymap.set("n", "<leader>do", "<cmd>lua require'dap'.step_out()<cr>")
-- keymap.set("n", '<leader>dd', function() require('dap').disconnect(); require('dapui').close(); end)
-- keymap.set("n", '<leader>dt', function() require('dap').terminate(); require('dapui').close(); end)
-- keymap.set("n", "<leader>dr", "<cmd>lua require'dap'.repl.toggle()<cr>")
-- keymap.set("n", "<leader>dl", "<cmd>lua require'dap'.run_last()<cr>")
-- keymap.set("n", '<leader>di', function() require "dap.ui.widgets".hover() end)
-- keymap.set("n", '<leader>d?', function() local widgets = require "dap.ui.widgets"; widgets.centered_float(widgets.scopes) end)
-- keymap.set("n", '<leader>df', '<cmd>Telescope dap frames<cr>')
-- keymap.set("n", '<leader>dh', '<cmd>Telescope dap commands<cr>')
-- keymap.set("n", '<leader>de', function() require('telescope.builtin').diagnostics({default_text=":E:"}) end)
