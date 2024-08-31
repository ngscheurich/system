all: clean build install

build:
	go build

install:
	mv ./grim /usr/local/bin

clean:
ifneq ("$(wildcard /usr/local/bin/grim)", "")
	rm /usr/local/bin/grim
endif
