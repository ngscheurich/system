vim.lsp.config["lexical"] = {
	cmd = { vim.env.HOME .. "/Projects/lexical/_build/dev/package/lexical/bin/start_lexical.sh" },
	filetypes = { "elixir", "heex", ".git" },
	root_markers = { "mix.exs" },
}
