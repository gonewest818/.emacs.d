export EMACS ?= emacs

.PHONY: clean init native-compile

# anything git may ignore can also be deleted...
GITIGNORE = $(shell cat .gitignore)

# ...except these files need to be preserved
PRESERVE = dev/ .agent-shell/ etc/config-for-df

default:
	@echo "Usage:"
	@echo "    make clean      # remove temporary files"
	@echo "    make init       # install packages from repos"
	@echo "    make native-compile # ahead-of-time native compile ELPA packages"

clean:
	rm -rf $(filter-out $(PRESERVE), $(GITIGNORE))

init:
	$(EMACS) --batch \
		 --eval='(setq native-comp-jit-compilation nil native-comp-enable-subr-trampolines nil)' \
		 -l early-init.el -l init.el \
                 --eval="(message \"*****\n%s\n*****\n\" (emacs-version))"

native-compile:
	$(EMACS) --batch -l early-init.el -l init.el -l etc/native-compile-packages.el
