.PHONY: test
test: test/amb.scm test/coroutine.scm

.PHONY: test/amb.scm
test/amb.scm:
	gosh -A. ./test/amb.scm

.PHONY: test/coroutine.scm
test/coroutine.scm:
	gosh -A. ./test/coroutine.scm
