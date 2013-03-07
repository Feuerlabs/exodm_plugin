## Copyright (c) 2013 Feuerlabs Inc. All Rights Reserved
REBAR=$(shell which rebar || echo ./rebar)

.PHONY: all compile clean doc

DIRS=src

all: deps compile

compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

doc:
	$(REBAR) get-deps doc
