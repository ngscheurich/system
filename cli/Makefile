all: clean build install

build:
	cargo build --release

install:
	cp target/release/grim /usr/local/bin

clean:
ifneq ("$(wildcard /usr/local/bin/grim)", "")
	rm /usr/local/bin/grim
endif
