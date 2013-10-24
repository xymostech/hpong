main: $(wildcard *.hs)
	ghc -o $@ $^
