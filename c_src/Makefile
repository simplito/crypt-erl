CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

C_SRC_OUTPUT ?= $(BASEDIR)/priv/crypt_helper

$(C_SRC_OUTPUT): crypt_helper.cc
	$(CXX) -o $@ $^ -lcrypt
