test: Example.hs Scanner.hs
	ghc --make Example.hs -o test

Scanner.hs: Scanner.x
	alex -o Scanner.hs -g Scanner.x

clean:
	rm -f test Example.hi Example.o Scanner.hi Scanner.o Scanner.hs
