export EMACS ?= emacs

.PHONY: clean init

# anything git may ignore can also be deleted...
GITIGNORE = $(shell cat .gitignore)

# ...except these files need to be preserved
PRESERVE = dev/

default:
	@echo "Usage:"
	@echo "    make clean      # remove temporary files"
	@echo "    make init       # install packages from repos"

clean:
	rm -rf $(filter-out $(PRESERVE), $(GITIGNORE))

init:
	$(EMACS) --batch -l early-init.el -l init.el  \
                 --eval="(message \"*****\n%s\n*****\n\" (emacs-version))"
