LISP ?= sbcl

PREFIX ?= /usr/local

SOURCES=$(wildcard src/*.lisp)
EXAMPLE_SOURCES=$(wildcard examples/*.lisp)
EXAMPLE_TARGETS=$(patsubst examples/%.lisp,examples/%.svg,$(EXAMPLE_SOURCES))

default: kablature
build: kablature
all: default examples

kablature: $(SOURCES)
	$(LISP) --load kablature.asd \
			--eval '(ql:quickload :kablature)' \
			--eval '(asdf:make :kablature)' \
			--eval '(quit)'

examples: $(EXAMPLE_TARGETS)

examples/%.svg: examples/%.lisp kablature
	./kablature -o $@ $<

clean:
	$(RM) kablature
	$(RM) $(EXAMPLE_TARGETS)

install:
	install -t $(PREFIX)/bin kablature
