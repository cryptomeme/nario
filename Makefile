
PROJECT = nario

SRCS = $(wildcard *.hs)

all:	$(PROJECT).exe

run:
	$(PROJECT).exe

$(PROJECT).exe:	$(SRCS)
	ghc -o $(PROJECT) --make -O $(SRCS)

clean:
	rm -f *.o
	rm -f *.hi
	rm -f *.manifest
	rm -f *.exe

doc:
	haddock -h -o man -l C:\\ghc\\haddock-2.0.0.0 -B c:\\ghc\\ghc-6.8.2 *.hs


imgs:
	runghc -itool tool/listup-imgs.hs data/img > Images.hs
