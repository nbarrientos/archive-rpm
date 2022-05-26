test:
	emacs -Q --batch -L .. --eval "(progn\
	(load-file \"archive-rpm-tests.el\")\
	(ert-run-tests-batch-and-exit))"

test-interactive:
	emacs -Q -L .. --eval "(progn\
	(load-file \"archive-rpm-tests.el\")\
	(ert t))"

byte-compile:
	emacs -Q --batch -L .. -l archive-rpm-byte-compile.el 2>&1 \
	| grep -E "([Ee]rror|[Ww]arning):" && exit 1 || exit 0
