
## Gerador de queries

O QGen é uma prova de conceito que visa, a partir de uma especificação de um
predicado em Alloy, gerar a query SPARQL correspondente.

Neste momento o QGen suporta apenas um sub conjunto da sintaxe Alloy. Não
suporta setas (emparelhamento), nem o converso de relações ('~') e as
implicações devem ser desambiguadas à esquerda ((A) => B) .


### Como funciona

O QGen recorre às bibliotecas Haskell Happy + Alex para gerar, apartir de uma gramática,
o parser do sub set Alloy e, após efetuar o parsing do invariante nega-o e com o
resultado, apresenta a query SPARQL resultante.

#### Gerar query

Para instalar o QGen basta fazer `make` na diretoria onde se encontram os
ficheiros.

Uma vez instalado:

`./QGen < invariante.als`

Onde 'invariante.als' é um ficheiro com o invariante a traduzir.

#### Exemplo

`> ./QGen < exemplo.als `

```
Invariante lido:
"all disj c1,c2:Classe_N3,ti:TermoIndice | ti in c1.temTI implies ti not in c2.temTI"
Invariante negado:
"some disj c1,c2:Classe_N3,ti:TermoIndice | ti not in c1.temTI and ti in c2.temTI"
```
