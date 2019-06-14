
## Gerador de queries

O QGen é uma prova de conceito que visa, a partir de uma especificação de um
predicado em Alloy, gerar a query SPARQL correspondente.

Neste momento o QGen suporta apenas um sub conjunto da sintaxe Alloy. Não
suporta setas (emparelhamento), a composição de relações (R.S) o converso de relações ('~'), a inclusão (in), e as
implicações devem ser desambiguadas à esquerda ((A) => B) assim como todas as expressões sujeitas a um operador binário.


### Como funciona

O QGen recorre às bibliotecas Haskell Happy + Alex para gerar, apartir de uma gramática,
o parser do sub set Alloy e, após efetuar o parsing do invariante nega-o e, com o
resultado, apresenta a query SPARQL resultante.

São utilizados os nomes das relações presentes nos invariantes sendo que uma ideia para o futuro seria a de ser possível, a partir de um ficheiro de configuração, fazer a correspondência entre as relações em Alloy e as relações em RDF. Um exemplo mais concreto e comum seria o de funções inversas com nomes diferentes, uma vez que em Alloy é comum o uso do operador '~'.

### Gerar query

**Dependências:**

- QuickCheck
- recursion-schemes

Após instaladas as depêndencias, para instalar o QGen basta fazer `make` na diretoria onde se encontram os
ficheiros.

Uma vez instalado:

`./QGen < invariante.als`

Onde 'invariante.als' é um ficheiro com o invariante a traduzir.

#### Exemplo

`> ./QGen < exemplo.als `

```
Invariante lido:
Invariante lido:
"all c:Classe_N3 | (some c.temFilho) implies (no c.temDF) and (no c.temPCA)"

Invariante negado:
"some c:Classe_N3 | (some c.temFilho) and (some c.temDF) or (some c.temPCA)"

Query gerada:
PREFIX : <http://jcr.di.uminho.pt/m51-clav#> 
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#> 
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> 

SELECT * WHERE {


	 ?c rdf:type :Classe_N3 . 

	FILTER ( 
		EXISTS { 	
		 ?c :temFilho ?blG . 
		} 

	 && (
		EXISTS { 	
		 ?c :temDF ?76V . 
		} 

	 || 
		EXISTS { 	
		 ?c :temPCA ?FIn . 
		} 
) 	)


}

```

Como é possível verificar a query gerada permite ao processador SPARQL encontrar as instâncias que não verificam o invariante.

### WIP

Neste momento o QGen é apenas uma prova de conceito e apenas é capaz de gerar queries apartir de invariantes bastante simples e que sejam escritos apenas no subset suportado. No entanto, é suficiente para provar que é possível gerar queries SPARQL a partir de uma especificação Alloy. Estas queries são traduções corretas uma vez que ambas as ferramentas (SPARQL e Alloy) são baseadas em lógica de primeira ordem logo, partilham, à partida de uma representação base.

Talvez no futuro este gerador consiga ser desenvolvido de forma a completar toda a sintaxe Alloy.
