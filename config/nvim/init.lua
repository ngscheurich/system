-- [nfnl] Compiled from init.fnl by https://github.com/Olical/nfnl, do not edit.
local conf_dir = "/private/etc/system/config/nvim"
local conf_src = (conf_dir .. "/README.md")
local conf_out = (conf_dir .. "/init.lua")
local user_group = vim.api.nvim_create_augroup("ngs", {})
local function tangle_reload()
  vim.system({"make"}, {cwd = conf_dir}):wait()
  vim.cmd(("source " .. conf_out))
  vim.cmd("redraw")
  return vim.notify("\243\176\145\147 Tangled and reloaded config")
end
vim.api.nvim_create_autocmd({"BufWritePost"}, {pattern = conf_src, group = user_group, callback = tangle_reload})
vim.cmd.set((("packpath^=" .. vim.fn.stdpath("data")) .. "/site"))
vim.cmd("packadd nfnl")
local _local_1_ = require("nfnl.core")
local assoc = _local_1_["assoc"]
local get_in = _local_1_["get-in"]
local merge = _local_1_["merge"]
local reduce = _local_1_["reduce"]
local some = _local_1_["some"]
for k, v in pairs({conceallevel = 2, cursorline = true, fillchars = {vert = "\226\148\130"}, laststatus = 3, listchars = {tab = ">-", eol = "\226\134\181", nbsp = "\226\144\163", trail = "\226\128\167", extends = "\226\159\169", precedes = "\226\159\168"}, number = true, scrolloff = 13, sidescrolloff = 8, signcolumn = "yes", splitbelow = true, splitright = true, termguicolors = true, breakindent = true, expandtab = true, shiftwidth = 2, smartindent = true, softtabstop = 2, tabstop = 2, grepprg = "rg --vimgrep", ignorecase = true, inccommand = "split", smartcase = true, completeopt = {"menu", "menuone", "noinsert"}, pumheight = 10, hidden = true, timeoutlen = 250, undofile = true, updatetime = 250, clipboard = "unnamedplus", showmode = false}) do
  vim.opt[k] = v
end
local function nmap(lhs, rhs, opt)
  return vim.keymap.set("n", lhs, rhs, opt)
end
local function imap(lhs, rhs, opt)
  return vim.keymap.set("i", lhs, rhs, opt)
end
local function tmap(lhs, rhs, opt)
  return vim.keymap.set("t", lhs, rhs, opt)
end
do
  local t = {["<Left>"] = "<C-w>h", ["<Down>"] = "<C-w>j", ["<Up>"] = "<C-w>k", ["<Right>"] = "<C-w>l"}
  for k, v in pairs(t) do
    nmap(k, v)
  end
end
local function toggle_opt(name)
  local on, off = nil
  if (name == "signcolumn") then
    on, off = "yes", "no"
  else
    on, off = true, false
  end
  if (vim.o[name] == on) then
    vim.o[name] = off
    return nil
  else
    vim.o[name] = on
    return nil
  end
end
local function _4_()
  return toggle_opt("number")
end
nmap("<Leader>un", _4_, {desc = "Line numbers"})
local function _5_()
  return toggle_opt("list")
end
nmap("<Leader>uw", _5_, {desc = "Whitespace"})
local function _6_()
  return toggle_opt("cursorline")
end
nmap("<Leader>uc", _6_, {desc = "Cursorline"})
nmap("<Esc>", "<Cmd>nohlsearch<CR>", {desc = "Stop highlighting matches"})
tmap("<Esc><Esc>", "<C-\\><C-n>", {desc = "Exit Terminal mode"})
assoc(vim.g, "mapleader", " ", "maplocalleader", ",")
do
  local lazypath = (vim.fn.stdpath("data") .. "/lazy/lazy.nvim")
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local args = {"git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath}
    local out = vim.fn.system(args)
    if (vim.v.shell_error ~= 0) then
      vim.api.nvim_echo({{"Failed to clone lazy.nvim:\n", "ErrorMsg"}, {out, "WarningMsg"}, {"\nPress any key to exit..."}}, true, {})
      vim.fn.getchar()
      os.exit(1)
    else
    end
  else
  end
  vim.opt.rtp:prepend(lazypath)
end
local function spec(plugin, tbl)
  if (tbl == nil) then
    return {plugin}
  else
    local _ = tbl
    return assoc(tbl, 1, plugin)
  end
end
local function lazy_key(desc, lhs, rhs)
  return {lhs, rhs, desc = desc}
end
do
  local lazy = require("lazy")
  local function _10_()
    return Snacks.picker.git_files({layout = "ivy"})
  end
  local function _11_()
    return Snacks.picker.grep({layout = "ivy"})
  end
  local function _12_()
    return Snacks.picker.lines({layout = "ivy"})
  end
  local function _13_()
    return Snacks.picker.buffers({layout = "select"})
  end
  local function _14_()
    return Snacks.picker.smart({layout = "ivy"})
  end
  local function _15_()
    return Snacks.picker.resume()
  end
  local function _16_()
    return Snacks.picker.files()
  end
  local function _17_()
    return Snacks.picker.git_files()
  end
  local function _18_()
    return Snacks.picker.projects()
  end
  local function _19_()
    return Snacks.picker.recent()
  end
  local function _20_()
    return Snacks.picker.autocmds()
  end
  local function _21_()
    return Snacks.picker.grep_buffers()
  end
  local function _22_()
    return Snacks.picker.command_history()
  end
  local function _23_()
    return Snacks.picker.commands()
  end
  local function _24_()
    return Snacks.picker.diagnostics()
  end
  local function _25_()
    return Snacks.picker.diagnostics_buffer()
  end
  local function _26_()
    return Snacks.picker.grep()
  end
  local function _27_()
    return Snacks.picker.help()
  end
  local function _28_()
    return Snacks.picker.highlights()
  end
  local function _29_()
    return Snacks.picker.icons()
  end
  local function _30_()
    return Snacks.picker.jumps()
  end
  local function _31_()
    return Snacks.picker.keymaps()
  end
  local function _32_()
    return Snacks.picker.lines()
  end
  local function _33_()
    return Snacks.picker.loclist()
  end
  local function _34_()
    return Snacks.picker.man()
  end
  local function _35_()
    return Snacks.picker.marks()
  end
  local function _36_()
    return Snacks.picker.notifications()
  end
  local function _37_()
    return Snacks.picker.qflist()
  end
  local function _38_()
    return Snacks.picker.registers()
  end
  local function _39_()
    return Snacks.picker.search_history()
  end
  local function _40_()
    return Snacks.picker.undo()
  end
  local function _41_()
    return Snacks.picker.grep_word()
  end
  local function _42_()
    if Snacks.indent.enabled then
      return Snacks.indent.disable()
    else
      return Snacks.indent.enable()
    end
  end
  local function _44_()
    return Snacks.gitbrowse()
  end
  local function _45_()
    local leap = require("leap")
    return leap.add_default_mappings()
  end
  local function _46_()
    local _let_47_ = require("aerial")
    local setup = _let_47_["setup"]
    local function _48_(b)
      nmap("{", "<Cmd>AerialPrev<CR>", {buffer = b})
      return nmap("}", "<Cmd>AerialNext<CR>", {buffer = b})
    end
    setup({on_attach = _48_})
    return nmap("<Leader>o", "<Cmd>AerialToggle!<CR>", {desc = "Outline"})
  end
  local function _49_()
    return nmap("<Leader>e", "<Cmd>Neotree reveal<CR>", {desc = "Explore"})
  end
  local function _50_()
    local oil = require("oil")
    local detail = false
    local function toggle_detail()
      detail = not detail
      if detail then
        return oil.set_columns({"icon", "permissions", "size", "mtime"})
      else
        return oil.set_columns({"icon"})
      end
    end
    oil.setup({default_file_explorer = true, keymaps = {gd = {desc = "Toggle detail view", callback = toggle_detail}}})
    return nmap("-", "<Cmd>Oil<CR>", {desc = "Open parent directory"})
  end
  local function _52_()
    local sessions = require("mini.sessions")
    sessions.setup()
    local function session_name()
      local n = string.gsub(vim.fn.getcwd(), "/", "_")
      return n
    end
    local function _53_()
      return sessions.write(session_name())
    end
    nmap("<Leader>Sw", _53_, {desc = "Write"})
    local function _54_()
      return sessions.read(session_name())
    end
    nmap("<Leader>Sr", _54_, {desc = "Read"})
    local function _55_()
      return sessions.select()
    end
    return nmap("<Leader>Ss", _55_, {desc = "Read"})
  end
  local function _56_()
    vim.g["slime_target"] = "tmux"
    return nil
  end
  local function _57_(_, opts)
    local _let_58_ = require("nvim-treesitter.configs")
    local setup = _let_58_["setup"]
    return setup(opts)
  end
  local function _59_()
    local lc = require("lspconfig")
    local servers = {bashls = {}, lexical = {cmd = {(vim.env.HOME .. "/Projects/lexical/_build/dev/package/lexical/bin/start_lexical.sh")}}, gdscript = {}, gopls = {}, lua_ls = {}, nil_ls = {}, rust_analyzer = {}, ts_ls = {}}
    for s, c in pairs(servers) do
      get_in(lc, {s, "setup"})(c)
    end
    return nil
  end
  local function _60_(_, opts)
    local cmp = require("blink.cmp")
    cmp.setup(opts)
    local function _61_()
      return cmp.show({providers = {"buffer"}})
    end
    return imap("<C-n>", _61_)
  end
  local function _62_()
    do
      local _let_63_ = require("catppuccin")
      local setup = _let_63_["setup"]
      setup({color_overrides = {mocha = {rosewater = "#ab7e8a", flamingo = "#a3685a", pink = "#b294bb", mauve = "#c07d90", red = "#cc6566", maroon = "#d57d62", peach = "#de935f", yellow = "#f0c674", green = "#b6bd68", teal = "#9fbd8f", sky = "#8abeb7", sapphire = "#85b0bc", blue = "#82a2be", lavender = "#a3a7c2", text = "#c4c8c6", subtext1 = "#b5b7b4", subtext0 = "#969896", overlay2 = "#838585", overlay1 = "#717374", overlay0 = "#5e6063", surface2 = "#4a4e52", surface1 = "#373b41", surface0 = "#282a2e", base = "#1d1f21", mantle = "#151718", crust = "#0e0f10"}}, integrations = {aerial = true, blink_cmp = true}})
    end
    return vim.cmd.colorscheme("catppuccin")
  end
  local function _64_(_, opts)
    local function toggle_dropbar()
      if (vim.o.winbar == "") then
        vim.o["winbar"] = "%{%v:lua.dropbar()%}"
        return nil
      else
        vim.o["winbar"] = ""
        return nil
      end
    end
    nmap("<Leader>ub", toggle_dropbar, {desc = "Breadcrumbs"})
    local _let_66_ = require("dropbar")
    local setup = _let_66_["setup"]
    return setup(opts)
  end
  local function _67_()
    local gs = require("gitsigns")
    gs.setup({signs = {add = {text = "\226\148\131"}, change = {text = "\226\148\131"}, changedelete = {text = "\226\148\131"}, delete = {text = "\226\148\131"}, topdelete = {text = "\226\148\131"}, untracked = {text = "\226\148\135"}}})
    nmap("<Leader>gb", gs.toggle_current_line_blame, {desc = "Line blame (toggle)"})
    nmap("<Leader>gd", gs.toggle_deleted, {desc = "Deleted (toggle)"})
    nmap("<Leader>gh", gs.toggle_linehl, {desc = "Line highlight (toggle)"})
    nmap("<Leader>gp", gs.preview_hunk, {desc = "Preview hunk"})
    nmap("<Leader>gp", gs.reset_hunk, {desc = "Reset hunk"})
    nmap("[h", gs.prev_hunk, {desc = "Previous hunk"})
    return nmap("]h", gs.next_hunk, {desc = "Next hunk"})
  end
  local function _68_()
    local lint = require("lint")
    lint["linters_by_ft"] = {sh = {"shellcheck"}, sql = {"sqlfluff"}}
    return nil
  end
  local function _69_()
    local editor = require("markview.extras.editor")
    return editor.setup()
  end
  local function _70_()
    vim.g["conjure#client#fennel#aniseed#deprecation_warning"] = false
    return nil
  end
  local function _71_()
    vim.g["db_ui_use_nerd_fonts"] = 1
    return nil
  end
  lazy.setup({checker = {enabled = true}, dev = {path = "~/Projects"}, install = {colorscheme = {"catppuccin"}}, performance = {rtp = {disabled_plugins = {"gzip", "netrwPlugin", "tarPlugin", "tohtml", "tutor", "zipPlugin"}}}, spec = {spec("Olical/nfnl", {ft = "fennel"}), spec("ngscheurich/srcedit", {dev = true, opts = {}}), spec("folke/snacks.nvim", {priority = 1000, opts = {utils = {}, bigfile = {}, quickfile = {}, scratch = {}, image = {}, picker = {}, notifier = {}, scroll = {}, statuscolumn = {}, input = {}, indent = {only_scope = true, only_current = true, enabled = false}, gitbrowse = {}}, keys = {lazy_key("Files", "<C-f>", _10_), lazy_key("Grep", "<C-g>", _11_), lazy_key("Lines", "<C-_>", _12_), lazy_key("Buffers", "<C-Space>", _13_), lazy_key("Find Files (Smart)", "<Leader><Leader>", _14_), lazy_key("Resume Picker", "<Leader>r", _15_), lazy_key("Files", "<Leader>ff", _16_), lazy_key("Git Files", "<Leader>fg", _17_), lazy_key("Projects", "<Leader>fp", _18_), lazy_key("Recent Files", "<Leader>fr", _19_), lazy_key("Autocommands", "<Leader>sa", _20_), lazy_key("Buffers", "<Leader>sB", _21_), lazy_key("Command History", "<Leader>s:", _22_), lazy_key("Commands", "<Leader>sc", _23_), lazy_key("Diagnostics", "<Leader>sd", _24_), lazy_key("Diagnostics (Buffer)", "<Leader>sD", _25_), lazy_key("Grep", "<Leader>sg", _26_), lazy_key("Help Pages", "<Leader>sh", _27_), lazy_key("Highlights", "<Leader>sH", _28_), lazy_key("Icons", "<Leader>si", _29_), lazy_key("Jumps", "<Leader>sj", _30_), lazy_key("Keymaps", "<Leader>sk", _31_), lazy_key("Lines (Buffer)", "<Leader>sb", _32_), lazy_key("Location List", "<Leader>sl", _33_), lazy_key("Man Pages", "<Leader>sM", _34_), lazy_key("Marks", "<Leader>sm", _35_), lazy_key("Notification History", "<Leader>sn", _36_), lazy_key("Quickfix List", "<Leader>sq", _37_), lazy_key("Registers", "<Leader>s\"", _38_), lazy_key("Search History", "<Leader>s/", _39_), lazy_key("Undo History", "<Leader>su", _40_), lazy_key("Word", "<Leader>sw", _41_), lazy_key("Indent Guides", "<Leader>ui", _42_), lazy_key("View on Remote", "<Leader>gB", _44_)}, lazy = false}), spec("ggandor/leap.nvim", {config = _45_}), spec("echasnovski/mini.align", {version = "*", config = true}), spec("echasnovski/mini.pairs", {version = "*", config = true}), spec("echasnovski/mini.splitjoin", {version = "*", config = true}), spec("echasnovski/mini.surround", {version = "*", config = true, opts = {mappings = {add = "Za", delete = "Zd", find = "Zf", find_left = "ZF", highlight = "Zh", replace = "Zr", update_n_lines = "Zn"}}}), spec("stevearc/aerial.nvim", {config = _46_}), spec("nvim-neo-tree/neo-tree.nvim", {branch = "v3.x", dependencies = {"nvim-lua/plenary.nvim", "MunifTanjim/nui.nvim"}, config = _49_}), spec("stevearc/oil.nvim", {config = _50_}), spec("echasnovski/mini.bracketed", {version = "*", config = true}), spec("echasnovski/mini.clue", {version = "*", opts = {triggers = {{mode = "n", keys = "<Leader>"}, {mode = "n", keys = "g"}, {mode = "x", keys = "g"}, {mode = "n", keys = "z"}}, clues = {{mode = "n", keys = "<Leader>f", desc = "+Find"}, {mode = "n", keys = "<Leader>g", desc = "+Git"}, {mode = "n", keys = "<Leader>s", desc = "+Search"}, {mode = "n", keys = "<Leader>S", desc = "+Sessions"}, {mode = "n", keys = "<Leader>u", desc = "+UI Toggles"}}}}), spec("echasnovski/mini.sessions", {version = "*", config = _52_}), spec("tpope/vim-rsi"), spec("jpalardy/vim-slime", {config = _56_}), spec("tpope/vim-projectionist"), spec("nvim-treesitter/nvim-treesitter", {opts = {highlight = {enable = true}, indent = {enable = true}, ensure_installed = {"bash", "css", "elixir", "erlang", "gdscript", "go", "graphql", "heex", "html", "http", "javascript", "json", "kdl", "lua", "nix", "rust", "scss", "sql", "svelte", "typescript", "xml", "yaml"}}, config = _57_}), spec("neovim/nvim-lspconfig", {config = _59_}), spec("saghen/blink.cmp", {version = "0.12.4", opts = {keymap = {preset = "super-tab"}, sources = {default = {"lsp", "path", "snippets"}}, cmdline = {enabled = false}, completion = {ghost_text = {enabled = true}, menu = {border = "none", auto_show = false}, documentation = {window = {border = "single"}, auto_show = true, auto_show_delay_ms = 500}}}, config = _60_}), spec("catppuccin/nvim", {name = "catppuccin", priority = 1000, config = _62_, lazy = false}), spec("Bekaboo/dropbar.nvim", {opts = {bar = {enable = false}}, config = _64_}), spec("echasnovski/mini.icons", {version = "*", config = true}), spec("catgoose/nvim-colorizer.lua", {event = "BufReadPre", opts = {user_default_options = {names = false}}}), spec("petertriho/nvim-scrollbar", {config = true}), spec("j-hui/fidget.nvim", {config = true}), spec("b0o/incline.nvim", {config = true}), spec("sphamba/smear-cursor.nvim", {config = true}), spec("akinsho/bufferline.nvim", {version = "*", after = "catppuccin", opts = {options = {mode = "tabs", indicator = {icon = "\226\148\131 "}, offsets = {{filetype = "neo-tree", text = "\238\175\149 Explorer", text_align = "left", separator = false}}, always_show_bufferline = false}}}), spec("lewis6991/gitsigns.nvim", {config = _67_}), spec("rebelot/heirline.nvim"), spec("stevearc/conform.nvim", {opts = {formatters_by_ft = {css = {"prettier"}, html = {"prettier"}, javascript = {"prettier"}, json = {"prettier"}, lua = {"stylua"}, scss = {"prettier"}, sql = {"sleek"}, typescript = {"prettier"}}, format_on_save = {timeout_ms = 500, lsp_fallback = true}}}), spec("mfussenegger/nvim-lint", {config = _68_}), spec("OXY2DEV/markview.nvim", {config = _69_, lazy = false}), spec("zbirenbaum/copilot.lua", {cmd = "Copilot", event = "InsertEnter", opts = {suggestion = {keymap = {accept = "<Tab>", next = "<C-n>", prev = "<C-p>", dismiss = "<C-q>"}}}, cond = false}), spec("olimorris/codecompanion.nvim", {config = true, dependencies = {"nvim-lua/plenary.nvim", "nvim-treesitter/nvim-treesitter"}, strategies = {chat = {adapter = "anthropic"}, inline = {adapter = "anthropic"}}, cond = false}), spec("mistweaverco/kulala.nvim", {ft = {"http", "rest"}, opts = {global_keymaps = true}}), spec("Olical/conjure", {config = _70_}), spec("brianhuster/live-preview.nvim"), spec("kristijanhusak/vim-dadbod-ui", {dependencies = {{"tpope/vim-dadbod", lazy = true}, {"kristijanhusak/vim-dadbod-completion", ft = {"sql", "mysql", "plsql"}, lazy = true}}, cmd = {"DBUI", "DBUIToggle", "DBUIAddConnection", "DBUIFindBuffer"}, init = _71_})}})
end
return vim.cmd("so ~/.config/nvim/scratch.lua")
