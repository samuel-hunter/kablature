LISP ?= sbcl

build: all

all:
	$(LISP) --load kablature.asd \
			--eval '(ql:quickload :kablature)' \
			--eval '(asdf:make :kablature)' \
			--eval '(quit)'
