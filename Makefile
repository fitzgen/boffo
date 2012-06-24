BOFFO_APPS:=auth frontend game mgr user
DEVSCRIPT_FILE:=dev.sh

all: $(BOFFO_APPS) devscript

$(BOFFO_APPS):
	$(MAKE) --directory=boffo_$@ $(TARGET)

devscript:
	echo "#!/usr/bin/env sh"                                 > $(DEVSCRIPT_FILE)
	echo "exec erl \\"										>> $(DEVSCRIPT_FILE)
	for app in $(BOFFO_APPS); do \
	  echo "  -pa boffo_$$app/ebin \\"						>> $(DEVSCRIPT_FILE); \
	  echo "  -pa boffo_$$app/deps/*/ebin \\"				>> $(DEVSCRIPT_FILE); \
	  echo "  -eval \"application:start(boffo_$$app).\" \\"	>> $(DEVSCRIPT_FILE); \
	done
	echo "  -sname boffo_dev"								>> $(DEVSCRIPT_FILE)
	chmod +x $(DEVSCRIPT_FILE)

clean:
	$(MAKE) TARGET=clean
	rm -f $(DEVSCRIPT_FILE)
