rss2html: rss2html.hs
	ghc -o $@ -threaded -W -O2 -XBangPatterns -fspec-constr-count=5 \
		--make $+
	strip --strip-all $@

clean:
	rm -f rss2html *.hi *.o
