return {
  "CRAG666/code_runner.nvim",
  config = function()
    local preview_cmd = "zathura" -- change to your PDF viewer (evince, okular, …)
    local code_runner = require("code_runner")
    code_runner.setup({
        focus = false,
        filetype = {
          java = {
            "cd $dir &&",
            "javac $fileName &&",
            "java $fileNameWithoutExt"
          },
          python = function()
            if vim.fn.filereadable("pyproject.toml") == 1 or vim.fn.filereadable("uv.lock") == 1 then
              return "uv run python3 -u $file"
            end
            local venv = vim.env.VIRTUAL_ENV
            if venv then
              return "cd $dir && " .. venv .. "/bin/python3 -u $fileName"
            end
            return "cd $dir && python3 -u $fileName"
          end,
          typescript = "deno run",
          rust = {
            "cd $dir &&",
            "rustc $fileName &&",
            "$dir/$fileNameWithoutExt"
          },
          c = function(...)
            local c_base = {
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

    vim.api.nvim_create_autocmd("FileType", {
      pattern = "python",
      callback = function()
        vim.keymap.set("n", "<localleader>cc", "<cmd>RunFile<CR>", { buffer = true, desc = "Run file" })
      end,
    })

    -- Track the source line pdb/ipdb is currently stopped on.
    local pdb_ns = vim.api.nvim_create_namespace("pdb_current_line")
    local pdb_mark = { buf = nil, id = nil }
    vim.api.nvim_set_hl(0, "PdbCurrentLine", { link = "Visual", default = true })

    local function clear_pdb_mark()
      if pdb_mark.buf and vim.api.nvim_buf_is_valid(pdb_mark.buf) then
        vim.api.nvim_buf_del_extmark(pdb_mark.buf, pdb_ns, pdb_mark.id)
      end
      pdb_mark = { buf = nil, id = nil }
    end

    -- Find a window showing a normal file buffer (not the runner terminal)
    -- to display the paused source line in, creating a split if needed.
    local function find_source_win(term_win)
      for _, win in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
        if win ~= term_win then
          local buf = vim.api.nvim_win_get_buf(win)
          if vim.bo[buf].buftype == "" and vim.bo[buf].filetype ~= "neo-tree" then
            return win
          end
        end
      end
      local new_win
      vim.api.nvim_win_call(term_win, function()
        vim.cmd("aboveleft new")
        new_win = vim.api.nvim_get_current_win()
      end)
      return new_win
    end

    local function show_pdb_location(term_win, path, lineno)
      -- pdb prints paths relative to the debugged process's cwd
      local abs_path = vim.fn.fnamemodify(path, ":p")
      local src_win = find_source_win(term_win)
      vim.api.nvim_win_call(src_win, function()
        if vim.fn.expand("%:p") ~= abs_path then
          vim.cmd("edit " .. vim.fn.fnameescape(abs_path))
        end
        vim.api.nvim_win_set_cursor(src_win, { lineno, 0 })
        vim.cmd("normal! zz")
      end)
      clear_pdb_mark()
      local buf = vim.api.nvim_win_get_buf(src_win)
      pdb_mark.id = vim.api.nvim_buf_set_extmark(buf, pdb_ns, lineno - 1, 0, {
        line_hl_group = "PdbCurrentLine",
        sign_text = "▶",
        sign_hl_group = "DiagnosticError",
      })
      pdb_mark.buf = buf
    end

    -- Jump into the runner terminal (in insert mode) only when a pdb/ipdb
    -- breakpoint prompt actually shows up, leaving normal runs untouched.
    -- Also track and highlight the source line pdb is paused on, updating
    -- it as you step with n/s/c etc.
    vim.api.nvim_create_autocmd("TermOpen", {
      callback = function(args)
        vim.schedule(function()
          if not vim.api.nvim_buf_is_valid(args.buf) or vim.bo[args.buf].filetype ~= "crunner" then
            return
          end
          vim.api.nvim_buf_attach(args.buf, false, {
            on_lines = function(_, buf, _, firstline, _, new_lastline)
              local lines = vim.api.nvim_buf_get_lines(buf, firstline, new_lastline, false)
              for _, line in ipairs(lines) do
                local path, lnum = line:match("^> (.+)%((%d+)%)")
                if path and lnum then
                  vim.schedule(function()
                    local term_win = vim.fn.bufwinid(buf)
                    if term_win ~= -1 then
                      show_pdb_location(term_win, path, tonumber(lnum))
                    end
                  end)
                elseif line:match("^%(Pdb%)") or line:match("^ipdb>") or line:match("^%(ipdb%)") then
                  vim.schedule(function()
                    local win = vim.fn.bufwinid(buf)
                    if win ~= -1 then
                      vim.api.nvim_set_current_win(win)
                      vim.cmd("startinsert")
                    end
                  end)
                end
              end
            end,
          })
        end)
      end,
    })

    -- "[Process exited]" is shown as virtual text, not a buffer line, so
    -- clear the marker on job exit instead of watching for it via on_lines.
    vim.api.nvim_create_autocmd("TermClose", {
      callback = function(args)
        if vim.bo[args.buf].filetype == "crunner" then
          clear_pdb_mark()
        end
      end,
    })
  end,
}
