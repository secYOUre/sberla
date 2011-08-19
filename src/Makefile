include ../support/include.mk

include ../support/vsn.mk

VSN=$(SBERLA_VSN)


APP_FILE = sberla.app
APP_SRC = $(APP_FILE).src
APP_TARGET = $(EBIN_DIR)/$(APP_FILE)


all: $(EBIN_FILES) $(APP_TARGET)

$(APP_TARGET): $(APP_SRC) ../support/vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES) $(APP_TARGET)
