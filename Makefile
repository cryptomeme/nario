
PROJECT = nario

SRCS = $(wildcard *.hs) $(wildcard Actor/*.hs)
OBJS = $(subst .hs,.o,$(SRCS)) $(subst .hs,.hi,$(SRCS))

all:	$(PROJECT).exe

run:
	$(PROJECT).exe

$(PROJECT).exe:	$(SRCS)
	ghc -o $(PROJECT) --make -O $(SRCS)

clean:
	rm -f $(OBJS)
	rm -f *.manifest
	rm -f *.exe
	rm -f *.hi-boot *.o-boot

doc:
	haddock -h -o man -l C:\\ghc\\haddock-2.0.0.0 -B c:\\ghc\\ghc-6.8.2 *.hs


imgs:
	runghc -itool tool/listup-imgs.hs data/img > Images.hs

count:
	@echo $(SRCS) | xargs -n1 echo | wc | gawk '{print $$1 " files";}'
	@cat $(SRCS) | wc | gawk '{print $$1 " lines";}'
