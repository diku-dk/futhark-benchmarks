FUTHARK?=futhark

TESTS=*_tests.fut

.PHONY: test
test: $(TESTS)
	$(FUTHARK) test --pass-compiler-option=--Werror $^

.PHONY: doc
doc:
	$(FUTHARK) doc -o doc .

.PHONY: clean
clean:
	rm -rf *~ linalg lu lup nmf qr perm dpsolve *.c *_tests *.actual *.expected doc
