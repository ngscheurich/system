-- ===================================================================
--  Code Debugging Tools
-- ===================================================================

return {
  -- =================================================================
  --  nvim-dap
  -- -----------------------------------------------------------------
  --  https://github.com/mfussenegger/nvim-dap
  --  Debug Adapter Protocol client
  -- -----------------------------------------------------------------
  {
    "mfussenegger/nvim-dap",
    config = function()
      local dap = require("dap")

      dap.adapters.lldb = {
        type = "executable",
        command = "/opt/homebrew/opt/llvm/bin/lldb-vscode",
        name = "lldb",
      }

      dap.configurations.rust = {
        {
          name = "Launch",
          type = "lldb",
          request = "launch",
          program = function()
            return vim.fn.input("Path to executable: " .. vim.fn.getcwd() .. "/")
          end,
          cwd = "${workspaceFolder}",
          stopOnEntry = false,
          args = {},
        },
      }
    end,
  },

  -- =================================================================
  --  nvim-dap-ui
  -- -----------------------------------------------------------------
  --  https://github.com/rcarriga/nvim-dap-ui
  --  TUI for nvim-dap
  -- -----------------------------------------------------------------
  {
    "rcarriga/nvim-dap-ui",

    dependencies = {
      "mfussenegger/nvim-dap",
    },

    config = function()
      local dap, dapui = require("dap"), require("dapui")
      local keymap_set = vim.keymap.set

      dapui.setup()

      -- Listeners
      dap.listeners.after.event_initialized["dapui_config"] = function()
        dapui.open()
      end
      dap.listeners.before.event_terminated["dapui_config"] = function()
        dapui.close()
      end
      dap.listeners.before.event_exited["dapui_config"] = function()
        dapui.close()
      end

      -- Keymaps
      keymap_set("n", "<F5>", function()
        require("dap").continue()
      end)
      keymap_set("n", "<F10>", function()
        require("dap").step_over()
      end)
      keymap_set("n", "<F11>", function()
        require("dap").step_into()
      end)
      keymap_set("n", "<F12>", function()
        require("dap").step_out()
      end)
      keymap_set("n", "<Leader>b", function()
        require("dap").toggle_breakpoint()
      end)
      keymap_set("n", "<Leader>B", function()
        require("dap").set_breakpoint()
      end)
      keymap_set("n", "<Leader>lp", function()
        require("dap").set_breakpoint(nil, nil, vim.fn.input("Log point message: "))
      end)
      keymap_set("n", "<Leader>dr", function()
        require("dap").repl.open()
      end)
      keymap_set("n", "<Leader>dl", function()
        require("dap").run_last()
      end)
      keymap_set({ "n", "v" }, "<Leader>dh", function()
        require("dap.ui.widgets").hover()
      end)
      keymap_set({ "n", "v" }, "<Leader>dp", function()
        require("dap.ui.widgets").preview()
      end)
      keymap_set("n", "<Leader>df", function()
        local widgets = require("dap.ui.widgets")
        widgets.centered_float(widgets.frames)
      end)
      keymap_set("n", "<Leader>ds", function()
        local widgets = require("dap.ui.widgets")
        widgets.centered_float(widgets.scopes)
      end)
    end,
  },
}
