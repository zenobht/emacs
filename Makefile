emacs ?= emacs

BASEDIR := $(shell pwd)

profile:
	$(emacs) -nw -Q -l profile-dotemacs/profile-dotemacs.el \
	--eval "(setq profile-dotemacs-file \
        (setq load-file-name \"$(abspath init.el)\"))" \
	-f profile-dotemacs

install: upgrade
		make run

# bare:
# 	$(emacs) -Q -l etc/bareinit.el

pull:
	echo "-*- mode: compilation -*-"
	git pull 2>&1

upgrade: pull
	cd $(BASEDIR) && $(emacs) -batch -l packages.el 2>&1

up: upgrade
	$(emacs) -Q -l init.el

run:
	$(emacs) -Q -l init.el
