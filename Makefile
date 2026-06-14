
PROJECT = nario

SRCS = $(wildcard *.hs) $(wildcard Actor/*.hs)
OBJDIR = obj
OBJS = $(addprefix $(OBJDIR)/,$(subst .hs,.o,$(SRCS)))

CSRCS = $(wildcard *.c)

SDL_CFLAGS := $(shell sdl-config --cflags 2>/dev/null)
GHCOPT = -O -no-hs-main -odir $(OBJDIR) -hidir $(OBJDIR) -stubdir $(OBJDIR) -I$(OBJDIR) -optc-I/opt/homebrew/include $(SDL_CFLAGS)
LINKOPT = -optl-L/opt/homebrew/lib -optl-lSDLmain -optl-lSDL -framework Cocoa

all:	$(PROJECT).exe

run:
	./$(PROJECT).exe

$(PROJECT).exe:	objs
	ghc --make -o $(PROJECT).exe $(GHCOPT) $(LINKOPT) $(SRCS) $(CSRCS)

objs:	$(OBJDIR) $(SRCS)
	ghc -c --make $(GHCOPT) $(SRCS)

$(OBJDIR):
	mkdir $(OBJDIR)

clean:
	rm -rf $(OBJDIR)
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
