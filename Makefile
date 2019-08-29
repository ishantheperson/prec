GHC = ghc 
BUILDDIR = build 
GHCFLAGS = -W 
GHCBUILDFLAGS = -isrc -odir $(BUILDDIR) -hidir $(BUILDDIR)

SOURCE = $(shell find src/ -name "*.hs")

EXECUTABLE = prec
MAIN = Main.hs 

all: $(EXECUTABLE) doc 

$(EXECUTABLE): $(SOURCE)
	$(GHC) $(GHCFLAGS) $(GHCBUILDFLAGS) $(MAIN) -o $(EXECUTABLE)

doc: $(SOURCE)
	haddock --html -o doc $(SOURCE)

clean: 
	rm -rf build/* $(EXECUTABLE) doc
