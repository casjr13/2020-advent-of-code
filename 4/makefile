all: clean part1

clean: clParseLib clPart1 clPart2

clParseLib:
	rm -f Parselib.hi Parselib.o

clPart1:
	rm -f part1 part1.hi part1.o

part1:
	ghc part1.hs

clPart2:
	rm -f part2 part2.hi part2.o

part2:
	ghc part2.hs