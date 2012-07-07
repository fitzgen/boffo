BOFFO_PREFIX:=boffo_
BOFFO_SUB_APPS:=auth frontend game mgr user feeds
BOFFO_APPS:=$(addprefix $(BOFFO_PREFIX), $(BOFFO_SUB_APPS)) boffo
DEVSCRIPT_FILE:=dev.sh

all: devscript
	for app in $(BOFFO_APPS); do \
	  $(MAKE) --directory=$$app $(TARGET); \
	done

devscript:
	echo "#!/usr/bin/env sh"                                 > $(DEVSCRIPT_FILE)
	echo "exec erl \\"										>> $(DEVSCRIPT_FILE)
	for app in  $(addprefix $(BOFFO_PREFIX), $(BOFFO_SUB_APPS)) $(BOFFO_APPS); do \
	  echo "  -pa $$app/ebin \\"						>> $(DEVSCRIPT_FILE); \
	  echo "  -pa $$app/deps/*/ebin \\"				>> $(DEVSCRIPT_FILE); \
	  echo "  -eval \"application:start($$app).\" \\"	>> $(DEVSCRIPT_FILE); \
	done
	echo "  -sname boffo_dev"								>> $(DEVSCRIPT_FILE)
	chmod +x $(DEVSCRIPT_FILE)

clean:
	$(MAKE) TARGET=clean
	rm -f $(DEVSCRIPT_FILE)
