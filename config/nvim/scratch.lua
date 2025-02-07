-- [nfnl] Compiled from scratch.fnl by https://github.com/Olical/nfnl, do not edit.
local _local_1_ = require("nfnl.module")
local autoload = _local_1_["autoload"]
local core = autoload("nfnl.core")
local tbl = {x = 1}
tbl = core.merge(tbl, {y = 2})
local function _2_(x)
  return vim.print(x)
end
return core["run!"](_2_, pairs({x = 1, y = 2}))
