all:
	bnfc FUN.cf
	happy -gca ParFUN.y
	alex -g LexFUN.x
	ghc --make Interpreter.hs lab4.hs -o lab4

clean:
	-rm -f lab4
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocFUN.ps

distclean: clean
		-rm -f DocFUN.* LexFUN.* ParFUN.* LayoutFUN.* SkelFUN.* PrintFUN.* TestFUN.* AbsFUN.* TestFUN  FUN.dtd XMLFUN.*

