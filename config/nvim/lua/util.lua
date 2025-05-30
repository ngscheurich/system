local M = {}

---Toggle a Neovim option.
---@param name string
---@return nil
M.toggle_opt = function(name)
	local on, off

	if name == "signcolumn" then
		on, off = "yes", "no"
	else
		on, off = true, false
	end

	if vim.o[name] == on then
		vim.o[name] = off
	else
		vim.o[name] = on
	end
end

---Get the `attr` of the given Neovim highlight group.
---@param name string
---@param attr string
---@return string
M.get_hl_attr = function(name, attr)
	local id = vim.fn.hlID(name)
	return vim.fn.synIDattr(id, attr)
end

--Switch to a different colorscheme.
--@return nil
-- M.switch_colors = function ()
--   local colorschemes = {"wave", "dragon", "lotus"}
--  vim.ui.select(themes, function (theme)
--     package.loaded[""]
--  end)
-- end

return M
