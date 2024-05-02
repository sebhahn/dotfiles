return {
  "CRAG666/code_runner.nvim",
  config = function()
    local code_runner = require("code_runner")

    code_runner.setup({
        filetype = {
          java = {
            "cd $dir &&",
            "javac $fileName &&",
            "java $fileNameWithoutExt"
          },
          python = "python3 -u",
          typescript = "deno run",
          rust = {
            "cd $dir &&",
            "rustc $fileName &&",
            "$dir/$fileNameWithoutExt"
          },
          c = function(...)
            c_base = {
              "cd $dir &&",
              "gcc $fileName -o",
              "/tmp/$fileNameWithoutExt",
            }
            local c_exec = {
              "&& /tmp/$fileNameWithoutExt &&",
              "rm /tmp/$fileNameWithoutExt",
            }
            vim.ui.input({ prompt = "Add more args:" }, function(input)
                c_base[4] = input
                vim.print(vim.tbl_extend("force", c_base, c_exec))
                require("code_runner.commands").run_from_fn(vim.list_extend(c_base, c_exec))
            end)
          end,
        },
    })

    vim.keymap.set('n', '<leader>r', '<cmd>RunCode<CR>', { noremap = true, silent = false })
    vim.keymap.set('n', '<leader>rf', '<cmd>RunFile<CR>', { noremap = true, silent = false })
    vim.keymap.set('n', '<leader>rft', '<cmd>RunFile tab<CR>', { noremap = true, silent = false })
    vim.keymap.set('n', '<leader>rp', '<cmd>RunProject<CR>', { noremap = true, silent = false })
    vim.keymap.set('n', '<leader>rc', '<cmd>RunClose<CR>', { noremap = true, silent = false })
    vim.keymap.set('n', '<leader>crf', '<cmd>CRFiletype<CR>', { noremap = true, silent = false })
    vim.keymap.set('n', '<leader>crp', '<cmd>CRProjects<CR>', { noremap = true, silent = false })

  end,
}
