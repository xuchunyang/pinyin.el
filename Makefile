EMACS ?= emacs

.PHONY: all
all:
	@printf "* Checking Emacs Version...\n"
	@$(EMACS) --version | head -1
	@printf "\n* Byte-Compiling pinyin.el...\n"
	${EMACS} -Q --batch -L . -f batch-byte-compile pinyin.el
	@printf "\n* Testing...\n"
	${EMACS} -Q --batch -L . -l pinyin-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f pinyin.elc

xuchunyang:
	@for cmd in emacs emacs-24.4.2 emacs-24.5.2 emacs-25.1.1 emacs-25.3.1; do \
	    make EMACS=$$cmd ;\
	done
