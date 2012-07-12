BOFFO_PREFIX:=boffo_
BOFFO_SUB_APPS:=auth frontend game mgr user
BOFFO_APPS:=$(addprefix $(BOFFO_PREFIX), $(BOFFO_SUB_APPS))
DEVSCRIPT_FILE:=dev.sh

all: deps devscript
	@./rebar compile

deps:
	@./rebar get-deps

edoc:
	@./rebar doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@./rebar skip_deps=true eunit

dialyzer:
	@$(REBAR) dialyze

devscript:
	echo "#!/usr/bin/env sh"                             > $(DEVSCRIPT_FILE)
	echo "exec erl \\"                                  >> $(DEVSCRIPT_FILE)
	echo "  -pa ebin \\"					            >> $(DEVSCRIPT_FILE);
	echo "  -pa deps/*/ebin \\"			                >> $(DEVSCRIPT_FILE);
	echo "  -eval \"application:load(boffo).\" \\"	    >> $(DEVSCRIPT_FILE);
	echo "  -eval \"application:start(boffo).\" \\"     >> $(DEVSCRIPT_FILE);
	for app in  $(addprefix $(BOFFO_PREFIX), $(BOFFO_SUB_APPS)) $(BOFFO_APPS); do \
	  echo "  -pa apps/$$app/ebin \\"                   >> $(DEVSCRIPT_FILE); \
      echo "  -eval \"application:load($$app).\" \\"    >> $(DEVSCRIPT_FILE); \
      echo "  -eval \"application:start($$app).\" \\"   >> $(DEVSCRIPT_FILE); \
	done
	echo "  -sname boffo_dev"                           >> $(DEVSCRIPT_FILE)
	chmod +x $(DEVSCRIPT_FILE)

clean:
	./rebar clean
	rm -f $(DEVSCRIPT_FILE)
