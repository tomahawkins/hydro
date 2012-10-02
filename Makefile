.PHONY: all
all: Hull.stl

Hull.stl: Hull.hs
	runhaskell -W Hull.hs

.PHONY: clean
clean:
	-rm *.stl
	-rm *.svg

