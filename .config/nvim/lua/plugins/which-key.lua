return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  dependencies = {
    "echasnovski/mini.icons",
  },
  init = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 500
  end,

  config = function()
    local wk = require("which-key")

    wk.add({
      { "<leader>a", group = "applications", nowait = true, remap = false },
      { "<leader>aa", "<cmd>Alpha<CR>", desc = "Alpha", nowait = true, remap = false },
      { "<leader>al", "<cmd>Lazy<CR>", desc = "Lazy", nowait = true, remap = false },
      { "<leader>am", "<cmd>Mason<CR>", desc = "Mason", nowait = true, remap = false },

      { "<leader>b", group = "buffer", nowait = true, remap = false },
      { "<leader>bb", "<cmd>Neotree buffers reveal float<CR>", desc = "Select buffer", nowait = true, remap = false },
      { "<leader>bd", "<cmd>bdelete<CR>", desc = "Kill buffer", nowait = true, remap = false },
      { "<leader>bf", "<cmd>Telescope buffers<CR>", desc = "Find buffers", nowait = true, remap = false },
      { "<leader>bn", "<cmd>bnext<CR>", desc = "Next buffer", nowait = true, remap = false },
      { "<leader>bp", "<cmd>bprevious<CR>", desc = "Previous buffer", nowait = true, remap = false },

      { "<leader>e", group = "explorer", nowait = true, remap = false },
      { "<leader>ec", "<cmd>NvimTreeCollapse<CR>", desc = "Neotree collapse", nowait = true, remap = false },
      { "<leader>ee", "<cmd>NvimTreeToggle<CR>", desc = "Neotree", nowait = true, remap = false },
      { "<leader>ef", "<cmd>NvimTreeFindFileToggle!<CR>", desc = "Neotree file", nowait = true, remap = false },
      { "<leader>er", "<cmd>NvimTreeRefresh<CR>", desc = "Neotree refresh", nowait = true, remap = false },

      { "<leader>f", group = "files", nowait = true, remap = false },
      {
        "<leader>fc",
        "<cmd>Telescope grep_string<CR>",
        desc = "Find string under cursor in cwd",
        nowait = true,
        remap = false,
      },
      {
        "<leader>fd",
        "<cmd>lua require('telescope.builtin').find_files({ cwd = require('telescope.utils').buffer_dir() })<CR>",
        desc = "Find files in cwd",
        nowait = true,
        remap = false,
      },
      {
        "<leader>ff",
        "<cmd>lua require('telescope.builtin').find_files({find_command = {'rg', '--files', '--hidden', '-g', '!.git' }})<CR>",
        desc = "Find files",
        nowait = true,
        remap = false,
      },
      {
        "<leader>fg",
        "<cmd>Telescope live_grep<CR>",
        desc = "Find text pattern in files",
        nowait = true,
        remap = false,
      },
      {
        "<leader>fh",
        "<cmd>lua require('telescope.builtin').help_tags()<CR>",
        desc = "Help tags",
        nowait = true,
        remap = false,
      },

      { "<leader>fr", "<cmd>Telescope oldfiles<CR>", desc = "Recent files", nowait = true, remap = false },
      { "<leader>fs", "<cmd>w!<CR>", desc = "Save", nowait = true, remap = false },

      {
        "<leader>fz",
        "<cmd>Telescope current_buffer_fuzzy_find<CR>",
        desc = "Fuzzy find buffers",
        nowait = true,
        remap = false,
      },

      { "<leader>g", group = "git/version-control", nowait = true, remap = false },
      { "<leader>gb", "<cmd>GitBlameToggle<CR>", desc = "git blame", nowait = true, remap = false },
      { "<leader>gg", "<cmd>LazyGit<CR>", desc = "lazygit", nowait = true, remap = false },
      { "<leader>gm", "<cmd>Telescope git_commits<CR>", desc = "git commits", nowait = true, remap = false },
      { "<leader>gs", "<cmd>Telescope git_status<CR>", desc = "git status", nowait = true, remap = false },

      { "<leader>h", group = "harpoon", nowait = true, remap = false },
      { "<leader>h1", "<cmd>lua require('harpoon.ui').nav_file(1)<CR>", desc = "file 1", nowait = true, remap = false },
      { "<leader>h2", "<cmd>lua require('harpoon.ui').nav_file(2)<CR>", desc = "file 2", nowait = true, remap = false },
      { "<leader>h3", "<cmd>lua require('harpoon.ui').nav_file(3)<CR>", desc = "file 3", nowait = true, remap = false },
      { "<leader>h4", "<cmd>lua require('harpoon.ui').nav_file(4)<CR>", desc = "file 4", nowait = true, remap = false },
      { "<leader>h5", "<cmd>lua require('harpoon.ui').nav_file(5)<CR>", desc = "file 5", nowait = true, remap = false },
      { "<leader>h6", "<cmd>lua require('harpoon.ui').nav_file(6)<CR>", desc = "file 6", nowait = true, remap = false },
      { "<leader>h7", "<cmd>lua require('harpoon.ui').nav_file(7)<CR>", desc = "file 7", nowait = true, remap = false },
      { "<leader>h8", "<cmd>lua require('harpoon.ui').nav_file(8)<CR>", desc = "file 8", nowait = true, remap = false },
      { "<leader>h9", "<cmd>lua require('harpoon.ui').nav_file(9)<CR>", desc = "file 9", nowait = true, remap = false },
      {
        "<leader>ha",
        "<cmd>lua require('harpoon.mark').add_file()<CR>",
        desc = "add file",
        nowait = true,
        remap = false,
      },
      {
        "<leader>hh",
        "<cmd>lua require('harpoon.ui').toggle_quick_menu()<CR>",
        desc = "menu",
        nowait = true,
        remap = false,
      },
      {
        "<leader>hj",
        "<cmd>lua require('harpoon.ui').nav_prev()<CR>",
        desc = "prev file",
        nowait = true,
        remap = false,
      },
      {
        "<leader>hk",
        "<cmd>lua require('harpoon.ui').nav_next()<CR>",
        desc = "prev file",
        nowait = true,
        remap = false,
      },
      {
        "<leader>ht",
        "<cmd>lua require('harpoon.term').gotoTerminal(1)<CR>",
        desc = "term 1",
        nowait = true,
        remap = false,
      },

      { "<leader>H", group = "help", nowait = true, remap = false },
      { "<leader>Hc", "<cmd>Telescope commands<CR>", desc = "Commands", nowait = true, remap = false },
      { "<leader>Hh", "<cmd>Telescope help_tags<CR>", desc = "Find help", nowait = true, remap = false },
      { "<leader>Hk", "<cmd>Telescope keymaps<CR>", desc = "Keymaps", nowait = true, remap = false },
      { "<leader>Hm", "<cmd>Telescope man_pages<CR>", desc = "Man pages", nowait = true, remap = false },
      { "<leader>Hr", "<cmd>Telescope registers<CR>", desc = "Registers", nowait = true, remap = false },

      -- ["l"] = "+lsp",
      -- 		["lf"] = { "<cmd>lua vim.lsp.buf.format({async=true})<cr>", "code format" },
      -- 		["la"] = { "<cmd>lua vim.lsp.buf.rename()<cr>", "rename" },
      -- 		["lk"] = { "<cmd>lua vim.lsp.buf.hover()<cr>", "hover" },
      -- 		["ld"] = { "<cmd>lua vim.lsp.buf.definition()<cr>", "definition" },
      -- 		["lr"] = { "<cmd>lua vim.lsp.buf.references()<cr>", "references" },
      -- 		["lc"] = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "code action" },
      -- 		["ll"] = { "<cmd>lua vim.lsp.buf.codelens.run()<cr>", "codelens action" },
      -- 		["ls"] = { "<cmd>Telescope lsp_document_symbols<cr>", "document symbols" },
      -- 		["lS"] = { "<cmd>Telescope lsp_dynamic_document_symbols<cr>", "workspace symbols" },

      { "<leader>o", group = "org", nowait = true, remap = false },
      {
        "<leader>oa",
        "<cmd>lua require('orgmode').action('agenda.prompt')<CR>",
        desc = "agenda",
        nowait = true,
        remap = false,
      },
      {
        "<leader>oo",
        "<cmd>lua require('orgmode').action('capture.refile')<CR>",
        desc = "capture",
        nowait = true,
        remap = false,
      },

      { "<leader>p", group = "project", nowait = true, remap = false },
      {
        "<leader>pp",
        "<cmd>lua require('telescope').extensions.project.project{}<CR>",
        desc = "display",
        nowait = true,
        remap = false,
      },

      { "<leader>q", group = "quit", nowait = true, remap = false },
      { "<leader>qq", "<cmd>wqall!<cr>", desc = "quit", nowait = true, remap = false },

      { "<leader>Q", group = "quickfix", nowait = true, remap = false },
      { "<leader>Qc", "<cmd>cclose<CR>", desc = "Close quickfix list", nowait = true, remap = false },
      { "<leader>Qf", "<cmd>cfirst<CR>", desc = "Jump to first item", nowait = true, remap = false },
      { "<leader>Ql", "<cmd>clast<CR>", desc = "Jump to last item", nowait = true, remap = false },
      { "<leader>Qn", "<cmd>cnext<CR>", desc = "Jump to next item", nowait = true, remap = false },
      { "<leader>Qo", "<cmd>copen<CR>", desc = "Open quickfix list", nowait = true, remap = false },
      { "<leader>Qp", "<cmd>cprev<CR>", desc = "Jump to previous item", nowait = true, remap = false },

      { "<leader>r", group = "run", nowait = true, remap = false },
      { "<leader>rc", "<cmd>RunClose<CR>", desc = "Run close", nowait = true, remap = false },
      { "<leader>rf", "<cmd>RunFile<CR>", desc = "Run file", nowait = true, remap = false },
      { "<leader>rl", "<cmd>lua require('lint').try_lint()<CR>", desc = "Lint file", nowait = true, remap = false },
      { "<leader>rm", "<cmd>CRFiletype<CR>", desc = "CRFiletype", nowait = true, remap = false },
      { "<leader>rn", "<cmd>CRProjects<CR>", desc = "CRProjects", nowait = true, remap = false },
      { "<leader>rp", "<cmd>RunProject<CR>", desc = "Run project", nowait = true, remap = false },
      { "<leader>rr", "<cmd>RunCode<CR>", desc = "Run code", nowait = true, remap = false },
      { "<leader>rt", "<cmd>RunFile tab<CR>", desc = "Run file tab", nowait = true, remap = false },

      { "<leader>s", group = "search", nowait = true, remap = false },
      {
        "<leader>sb",
        "<cmd>Telescope current_buffer_fuzzy_find<CR>",
        desc = "Find in current buffer",
        nowait = true,
        remap = false,
      },
      { "<leader>sm", "<cmd>Telescope marks<CR>", desc = "Telescope bookmarks", nowait = true, remap = false },

      -- s = {
      --   name = "Search",
      --   b = { "<cmd>Telescope git_branches<cr>", "Checkout branch" },
      --   c = { "<cmd>Telescope colorscheme<cr>", "Colorscheme" },
      --   h = { "<cmd>Telescope help_tags<cr>", "Find Help" },
      --   M = { "<cmd>Telescope man_pages<cr>", "Man Pages" },
      --   r = { "<cmd>Telescope oldfiles<cr>", "Open Recent File" },
      --   R = { "<cmd>Telescope registers<cr>", "Registers" },
      --   k = { "<cmd>Telescope keymaps<cr>", "Keymaps" },
      --   C = { "<cmd>Telescope commands<cr>", "Commands" },
      -- },

      { "<leader>t", group = "terminal", nowait = true, remap = false },
      { "<leader>tf", "<cmd>ToggleTerm direction=float<CR>", desc = "Float", nowait = true, remap = false },
      { "<leader>tc", "<cmd>lua _NCDU_TOGGLE()<CR>", desc = "ncdu", nowait = true, remap = false },
      { "<leader>tp", "<cmd>lua _PYTHON_TOGGLE()<CR>", desc = "Python", nowait = true, remap = false },
      { "<leader>tt", "<cmd>lua _HTOP_TOGGLE()<CR>", desc = "htop", nowait = true, remap = false },
      {
        "<leader>th",
        "<cmd>ToggleTerm size=10 direction=horizontal<CR>",
        desc = "Horizontal",
        nowait = true,
        remap = false,
      },
      {
        "<leader>tv",
        "<cmd>ToggleTerm size=80 direction=vertical<CR>",
        desc = "Vertical",
        nowait = true,
        remap = false,
      },

      { "<leader>T", group = "toggle/themes", nowait = true, remap = false },
      { "<leader>Tc", "<cmd>Telescope colorscheme<CR>", desc = "colorscheme", nowait = true, remap = false },
      { "<leader>Tn", "<cmd>set nu!<CR>", desc = "toggle-line-number", nowait = true, remap = false },
      { "<leader>Tr", "<cmd>set rnu!<CR>", desc = "toggle-relative-number", nowait = true, remap = false },

      { "<leader>v", group = "venv", nowait = true, remap = false },
      {
        "<leader>vg",
        "<cmd>lua require('swenv.api').get_current_venv()<CR>",
        desc = "Get venv",
        nowait = true,
        remap = false,
      },
      {
        "<leader>vv",
        "<cmd>lua require('swenv.api').pick_venv()<CR>",
        desc = "Select venv",
        nowait = true,
        remap = false,
      },

      { "<leader>w", group = "windows", nowait = true, remap = false },
      { "<leader>wR", "<C-w>r", desc = "Rotate window backward", nowait = true, remap = false },
      { "<leader>wd", "<cmd>close<CR>", desc = "Close current split", nowait = true, remap = false },
      { "<leader>we", "<C-w>=", desc = "Make splits equal size", nowait = true, remap = false },
      { "<leader>wh", "<C-w>>5", desc = "Split windows width bigger", nowait = true, remap = false },
      { "<leader>wj", "<C-w>+", desc = "Split windows height taller", nowait = true, remap = false },
      { "<leader>wk", "<C-w>-", desc = "Split windows height shorter", nowait = true, remap = false },
      { "<leader>wl", "<C-w><5", desc = "Split windows width smaller", nowait = true, remap = false },
      { "<leader>wm", "<cmd>MaximizerToggle<CR>", desc = "Maximize window", nowait = true, remap = false },
      { "<leader>wr", "<C-w><C-r>", desc = "Rotate window forward", nowait = true, remap = false },
      { "<leader>ws", "<C-w>s", desc = "Split window horizontally", nowait = true, remap = false },
      { "<leader>wv", "<C-w>v", desc = "Split window vertically", nowait = true, remap = false },
      { "<leader>ww", "<C-w>w", desc = "Other window", nowait = true, remap = false },

      { "<leader>x", group = "trouble", nowait = true, remap = false },
      { "<leader>xx", "<cmd>TroubleToggle<CR>", desc = "Open/close trouble list", nowait = true, remap = false },
      {
        "<leader>xw",
        "<cmd>TroubleToggle workspace_diagnostics<CR>",
        desc = "Open trouble workspace diagnostics",
        nowait = true,
        remap = false,
      },
      {
        "<leader>xd",
        "<cmd>TroubleToggle document_diagnostics<CR>",
        desc = "Open trouble document diagnostics",
        nowait = true,
        remap = false,
      },
      {
        "<leader>xq",
        "<cmd>TroubleToggle quickfix<CR>",
        desc = "Open trouble quickfix list",
        nowait = true,
        remap = false,
      },
      {
        "<leader>xl",
        "<cmd>TroubleToggle loclist<CR>",
        desc = "Open trouble location list",
        nowait = true,
        remap = false,
      },
      { "<leader>xt", "<cmd>TodoTrouble<CR>", desc = "Open todos in trouble", nowait = true, remap = false },
    })

    ---@class wk.Opts
    local defaults = {
      ---@type false | "classic" | "modern" | "helix"
      preset = "classic",
      -- Delay before showing the popup. Can be a number or a function that returns a number.
      ---@type number | fun(ctx: { keys: string, mode: string, plugin?: string }):number
      delay = function(ctx)
        return ctx.plugin and 0 or 200
      end,
      ---@param mapping wk.Mapping
      filter = function(mapping)
        -- example to exclude mappings without a description
        -- return mapping.desc and mapping.desc ~= ""
        return true
      end,
      --- You can add any mappings here, or use `require('which-key').add()` later
      ---@type wk.Spec
      spec = {},
      -- show a warning when issues were detected with your mappings
      notify = true,
      -- Which-key automatically sets up triggers for your mappings.
      -- But you can disable this and setup the triggers manually.
      -- Check the docs for more info.
      ---@type wk.Spec
      triggers = {
        { "<auto>", mode = "nxsot" },
      },
      -- Start hidden and wait for a key to be pressed before showing the popup
      -- Only used by enabled xo mapping modes.
      ---@param ctx { mode: string, operator: string }
      defer = function(ctx)
        return ctx.mode == "V" or ctx.mode == "<C-V>"
      end,
      plugins = {
        marks = true, -- shows a list of your marks on ' and `
        registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
        -- the presets plugin, adds help for a bunch of default keybindings in Neovim
        -- No actual key bindings are created
        spelling = {
          enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
          suggestions = 20, -- how many suggestions should be shown in the list?
        },
        presets = {
          operators = true, -- adds help for operators like d, y, ...
          motions = true, -- adds help for motions
          text_objects = true, -- help for text objects triggered after entering an operator
          windows = true, -- default bindings on <c-w>
          nav = true, -- misc bindings to work with windows
          z = true, -- bindings for folds, spelling and others prefixed with z
          g = true, -- bindings for prefixed with g
        },
      },
      ---@type wk.Win.opts
      win = {
        -- don't allow the popup to overlap with the cursor
        no_overlap = true,
        -- width = 1,
        -- height = { min = 4, max = 25 },
        -- col = 0,
        -- row = math.huge,
        -- border = "none",
        padding = { 1, 2 }, -- extra window padding [top/bottom, right/left]
        title = true,
        title_pos = "center",
        zindex = 1000,
        -- Additional vim.wo and vim.bo options
        bo = {},
        wo = {
          -- winblend = 10, -- value between 0-100 0 for fully opaque and 100 for fully transparent
        },
      },
      layout = {
        width = { min = 20 }, -- min and max width of the columns
        spacing = 3, -- spacing between columns
      },
      keys = {
        scroll_down = "<c-d>", -- binding to scroll down inside the popup
        scroll_up = "<c-u>", -- binding to scroll up inside the popup
      },
      ---@type (string|wk.Sorter)[]
      --- Mappings are sorted using configured sorters and natural sort of the keys
      --- Available sorters:
      --- * local: buffer-local mappings first
      --- * order: order of the items (Used by plugins like marks / registers)
      --- * group: groups last
      --- * alphanum: alpha-numerical first
      --- * mod: special modifier keys last
      --- * manual: the order the mappings were added
      --- * case: lower-case first
      sort = { "local", "order", "group", "alphanum", "mod" },
      ---@type number|fun(node: wk.Node):boolean?
      expand = 0, -- expand groups when <= n mappings
      -- expand = function(node)
      --   return not node.desc -- expand all nodes without a description
      -- end,
      -- Functions/Lua Patterns for formatting the labels
      ---@type table<string, ({[1]:string, [2]:string}|fun(str:string):string)[]>
      replace = {
        key = {
          function(key)
            return require("which-key.view").format(key)
          end,
          -- { "<Space>", "SPC" },
        },
        desc = {
          { "<Plug>%(?(.*)%)?", "%1" },
          { "^%+", "" },
          { "<[cC]md>", "" },
          { "<[cC][rR]>", "" },
          { "<[sS]ilent>", "" },
          { "^lua%s+", "" },
          { "^call%s+", "" },
          { "^:%s*", "" },
        },
      },
      icons = {
        breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
        separator = "➜", -- symbol used between a key and it's label
        group = "+", -- symbol prepended to a group
        ellipsis = "…",
        -- set to false to disable all mapping icons,
        -- both those explicitely added in a mapping
        -- and those from rules
        mappings = true,
        --- See `lua/which-key/icons.lua` for more details
        --- Set to `false` to disable keymap icons from rules
        ---@type wk.IconRule[]|false
        rules = {},
        -- use the highlights from mini.icons
        -- When `false`, it will use `WhichKeyIcon` instead
        colors = true,
        -- used by key format
        keys = {
          Up = " ",
          Down = " ",
          Left = " ",
          Right = " ",
          C = "󰘴 ",
          M = "󰘵 ",
          D = "󰘳 ",
          S = "󰘶 ",
          CR = "󰌑 ",
          Esc = "󱊷 ",
          ScrollWheelDown = "󱕐 ",
          ScrollWheelUp = "󱕑 ",
          NL = "󰌑 ",
          BS = "󰁮",
          Space = "󱁐 ",
          Tab = "󰌒 ",
          F1 = "󱊫",
          F2 = "󱊬",
          F3 = "󱊭",
          F4 = "󱊮",
          F5 = "󱊯",
          F6 = "󱊰",
          F7 = "󱊱",
          F8 = "󱊲",
          F9 = "󱊳",
          F10 = "󱊴",
          F11 = "󱊵",
          F12 = "󱊶",
        },
      },
      show_help = true, -- show a help message in the command line for using WhichKey
      show_keys = true, -- show the currently pressed key and its label as a message in the command line
      -- disable WhichKey for certain buf types and file types.
      disable = {
        ft = {},
        bt = {},
      },
      debug = false, -- enable wk.log in the current directory
    }

    wk.setup(defaults)
  end,
}
