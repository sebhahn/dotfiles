return {
  "CRAG666/code_runner.nvim",
  config = function()
    local code_runner = require("code_runner")
    code_runner.setup({
        focus = false,
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
          tex = function(...)
            require("code_runner.hooks.ui").select {
              Single = function()
                local preview = require "code_runner.hooks.preview_pdf"
                preview.run {
                  command = "tectonic",
                  args = { "$fileName", "--keep-logs", "-o", "/tmp" },
                  preview_cmd = preview_cmd,
                  overwrite_output = "/tmp",
                }
              end,
              Project = function()
                local cr_au = require "code_runner.hooks.autocmd"
                cr_au.stop_job()
                os.execute "tectonic -X build --keep-logs --open &> /dev/null &"
                local fn = function()
                  os.execute "tectonic -X build --keep-logs &> /dev/null &"
                end
                cr_au.create_au_write(fn)
              end,
                                                   }
          end,
          markdown = function(...)
            local hook = require "code_runner.hooks.preview_pdf"
            require("code_runner.hooks.ui").select {
              Normal = function()
                hook.run {
                  command = "pandoc",
                  args = { "$fileName", "-o", "$tmpFile", "-t pdf" },
                  preview_cmd = preview_cmd,
                }
              end,
              Presentation = function()
                hook.run {
                  command = "pandoc",
                  args = { "$fileName", "-o", "$tmpFile", "-t beamer" },
                  preview_cmd = preview_cmd,
                }
              end,
              Eisvogel = function()
                hook.run {
                  command = "bash",
                  args = { "./build.sh" },
                  preview_cmd = preview_cmd,
                  overwrite_output = ".",
                }
              end,
                                                   }
          end,
        },
    })
  end,
}
