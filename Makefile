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

upgrade:
	cd $(BASEDIR) && $(emacs) -batch -l config/packages.el 2>&1 | tee -a ~/Library/Logs/emacs.log

up: upgrade
	$(emacs) -Q -l init.el

run:
	$(emacs) -Q -l init.el
