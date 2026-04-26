GHC			:=	ghc
GHCFLAGS	:=	-isrc -dynamic
SOURCES		:=	$(wildcard *.hs)

check: $(SOURCES)
	$(GHC) $(GHCFLAGS) Main.hs -o $(@)

clean:
	rm -rf *.o *.hi check

.PHONY: clean
