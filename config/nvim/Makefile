pack_dir := $$XDG_DATA_HOME/nvim/site/pack/ngs/opt
lmt_dir := /tmp/lmt

all: tangle compile
init: install_lmt install_nfnl tangle compile

tangle:
	lmt README.md

compile:
	# TODO This needs to happen without messing with packages
	nvim --headless +"packadd nfnl" +"lua require('nfnl.api')['compile-all-files']()" +"q!" -es

install_lmt:
	git clone git@github.com:driusan/lmt $(lmt_dir)
	cd $(lmt_dir) && \
	go mod init driusan/lmt && \
	go build -o $$HOME/.local/bin/lmt

install_nfnl:
	mkdir -p $(pack_dir)
	git clone git@github.com:Olical/nfnl.git $(pack_dir)/nfnl

clean:
	rm -rf $(pack_dir)
	rm -rf $(lmt_dir)
	rm -rf *.lua lua
