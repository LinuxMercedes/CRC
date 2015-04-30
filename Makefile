all:
	ghc --make generate.hs crc.hs
	ghc --make alter.hs crc.hs
	ghc --make check.hs crc.hs

clean:
	-rm *.o
	-rm *.hi
	-rm generate alter check

