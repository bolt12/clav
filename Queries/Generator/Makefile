all: QGen

Tokens.hs : Tokens.x
	alex Tokens.x

Grammar.hs : Grammar.y
	happy Grammar.y
    
QGen : Tokens.hs Grammar.hs QGen.hs
	ghc --make -XTemplateHaskell QGen
    
clean:
	rm -f QGen Grammar.hs Tokens.hs *.o *.hi *.dyn_hi *.dyn_o
    
