BOFFO_APPS:=$(shell ls apps)
DEVSCRIPT_FILE:=dev.sh
ARGS_FILE:=.erl_args

all: deps devscript
	./rebar compile

deps:
	./rebar get-deps

edoc:
	./rebar doc

test: args_file
	ERL_FLAGS="-args_file $(ARGS_FILE) -sname boffo_test" ./rebar skip_deps=true eunit

args_file:
	@rm -f $(ARGS_FILE)
	@for dep in $$(ls deps); do                                   \
	  echo "-pa \"deps/$$dep/ebin\""             >> $(ARGS_FILE); \
	done
	@for app in $(BOFFO_APPS); do                                 \
	  echo "-pa apps/$$app/ebin"                 >> $(ARGS_FILE); \
	  echo "-eval \"application:load($$app).\""  >> $(ARGS_FILE); \
	  echo "-eval \"application:start($$app).\"" >> $(ARGS_FILE); \
	done

devscript: args_file
	@echo "#!/usr/bin/env sh"             > $(DEVSCRIPT_FILE)
	@echo "exec erl \\"                  >> $(DEVSCRIPT_FILE)
	@echo "  -args_file $(ARGS_FILE) \\" >> $(DEVSCRIPT_FILE)
	@echo "  -sname boffo_dev"           >> $(DEVSCRIPT_FILE)
	@chmod +x $(DEVSCRIPT_FILE)

clean:
	./rebar clean
	rm -f $(ARGS_FILE)
	rm -f $(DEVSCRIPT_FILE)
