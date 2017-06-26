#
#
#

clean:
	rm -f *.hi
	rm -f *.o
	rm -f *.info
	rm -f javaish_tok.hs
	rm -f javaish_parse.hs
	rm -f type_checker
	rm -f interpreter
	rm -f *.javaish.c
	rm -f *.javaish.exe

javaish_tok.hs: javaish_tok.x
	alex javaish_tok.x

javaish_parse.hs: javaish_parse.y
	happy -i javaish_parse.y

type_checker: type_checker.hs javaish_parse.hs javaish_tok.hs javaish_utils.hs
	ghc type_checker.hs javaish_parse.hs javaish_tok.hs javaish_utils.hs

interpreter: interpreter.hs javaish_parse.hs javaish_tok.hs javaish_utils.hs
	ghc interpreter.hs javaish_parse.hs javaish_tok.hs javaish_utils.hs

compiler: compiler.hs type_checker.hs javaish_parse.hs javaish_tok.hs javaish_utils.hs
	ghc compiler.hs type_checker.hs javaish_parse.hs javaish_tok.hs javaish_utils.hs

%.javaish.c : %.javaish
	./compiler < $< > $@

%.javaish.exe : %.javaish.c
	gcc -g -I . -o $@ $< runtime_system.c

testinterp: interpreter
	./interpreter
