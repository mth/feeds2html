# aptitude install libghc-feed-dev libghc-xss-sanitize-dev libghc-http-date-dev libghc-sha-dev

all: feeds2html feeds2html.1

feeds2html: feeds2html.hs
	ghc -o $@ -W -O2 -XBangPatterns -fspec-constr-count=5 \
		--make $+
	strip --strip-all $@

feeds2html.1: README.rst
	rst2man $< $@

clean:
	rm -f feeds2html feeds2html.1 *.hi *.o
