EMACS ?= emacs

compile:
	${EMACS} -Q --batch -L . -f batch-byte-compile pinyin.el

test:
	${EMACS} -Q --batch -L . -l pinyin-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f pinyin.elc
