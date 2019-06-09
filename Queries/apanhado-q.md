
## SPARQL Protocol and RDF Query Language

### Palavras-chave e conceitos


- **PREFIX** - Renomeia para um nome mais curto o namespace da informação
- **SELECT** - Seleciona as variáveis da query a mostrar no ecrã (projeção)
- **WHERE** - Especifica a informação a retirar do dataset
- **Variable** - Guarda informação para uso futuro, quer para SELECTionar ou dentro da secção WHERE

PREFIX pode ser visto como um guia para o processador SPARQL encontrar a informação, análogo a um 'include' ou a um 'import' nas linguagens de programação.

### Primeira query

```SPARQL
PREFIX GOT: <https://tutorial.linked.data.world/d/sparqltutorial/>

SELECT ?ID ?FName

WHERE {
    ?person GOT:col-got-id ?ID .
    ?person GOT:col-got-fname ?FName .
}
```

#### Primeira linha - PREFIX

```
PREFIX GOT: <https://tutorial.linked.data.world/d/sparqltutorial/>
```

Esta linha define a abreviatura GOT: para se referir a https://tutorial.linked.data.world/d/sparqltutorial/ (análogo a declarar uma variável que se refere a uma strign). O URI refere-se a informação sobre os recursos e o seu propósito é o de identificar de forma única um pedaço de informação.

#### Próxima linha - SELECT

```SPARQL
SELECT ?ID ?FNAME
```

Simplesmente indica ao processador SPARQL que informação mostrar, neste caso o valor das propriedades ID e FName.

Porquê os pontos de interrogação?


Em SPARQL, os pontos de interrogação denotam variáveis, que se comportam como as variáveis de uma linguagem de programação. Guardam valores para uso futuro. Então, `SELECT ?ID ?FName` diz essencialmente, "Vamos procurar algum valor para guardar em ?ID e ?FName, e depois vamos mostrar esses valores."

#### Finalmente - a secção WHERE:

```SPARQL
WHERE
{
    ?person GOT:col-got-id ?ID .
    ?person GOT:col-got-fname ?FName .
}
```

A secção WHERE diz ao processador que valores de propriedades meter nas variáveis que estão a ser SELECTionadas e como ir buscar essa informação. O processador SPARQL faz pattern matching com declarações na secção WHERE. As queries na secção WHERE seguem a mesma estrutura de um ficheiro .ttl.

```
{ resource propertyName propertyValue . }
```

Na nossa query usamos duas frases para preencher as variáveis ?ID e ?FName. Ambas as frases na query começam com ?person, outra variável, mas ?person não é projetada portanto não irá aparecer no ecrã; apenas é utilizada para guardar informação dentro da secção WHERE.

Por outras palavras, o processador SPARQL procura as entradas na base de dados que fazem pattern match com as querys definidas na secção WHERE e guarda os valores nas variáveis. A primeira linha da secção WHERE diz, "Encontra-me pessoas que tenham IDs" e a segunda linha diz, "Para cada uma dessas pessoas, encontra-me o seu FName se tiverem.". Existe uma conjunção implicita entre cada query na secção WHERE.

##### NOTA

Recursos e nomes de propriedade têm que pertencer a namespaces específicos para que o processador SPARL saiba onde procurar, especialmente quando juntando dois datasets.

##### In-depth

curioso como é que a secção WHERE funciona? Segue-se a baixo o guia passo-a-passo:

- A primeira frase `?person GOT:col-got-id ?ID .` indica ao processador SPARQL para procurar o recurso que tenha a propriedade ID e guarda esse valor em ?ID.

- O processador SPARQL guarda o recurso encontrado à variável ?person durante o resto das queries.

- A próxima frase `?person GOT:col-got-fname ?FName .` diz ao processador SPARQL para verificar se o recurso guardado em ?person tem a propriedade GOT:col-got-fname, e caso tenha, guarda em ?FName.

- Assim que todas as frases da query tenham corrido, o processador SPARQL reitera para cada recurso no dataset, guardando todos os valores de ID e Fname nas variáveis ?ID e ?FName.

## Geração de Queries SPARQL

A geração de queries SPARQL consiste em traduzir os invariantes, especificados em Alloy, na negativa para a linguagem SPARQL para ser possível descobrir que instâncias é que não estão de acordo com os invariantes e corrigi-las.

[**Inv1:**](https://github.com/bolt12/clav#inv1)

Se uma Classe_N1 pertence a uma LC/TS, consequentemente os seus filhos,netos,etc.. tambem têm de pertencer

- Alloy

```Alloy
all c:Classe_N1,lc:ListaConsolidada | lc = c.pertenceLC =>
	(all cf:c.temFilho | lc = cf.pertenceLC) and
	(all cf:c.temFilho.temFilho | lc = cf.pertenceLC) and
    (all cf:c.temFilho.temFilho.temFilho | lc = cf.pertenceLC)
```

- SPARQL

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where { 
    ?c1 :rdf:type :Classe_N1 .
    ?c1 :pertenceLC ?lc1 .
    
    ?c2 :rdf:type :Classe_N2 .
    ?c2 :temPai ?c1 .
    ?c2 :pertenceLC ?lc2 .
    
    ?c3 :rdf:type :Classe_N3 .
    ?c3 :temPai ?c2 .
    ?c3 :pertenceLC ?lc3 .
    
    ?c4 :rdf:type :Classe_N4 .
    ?c4 :temPai ?c3 .
    ?c4 :pertenceLC ?lc4 .
    
    FILTER (
        ?c1 != ?c2 
        && ?c2 != ?c3
        && ?c3 != ?c4
        && (?lc1 != ?lc2
            || ?lc2 != ?lc3
            || ?lc3 != ?lc4)
    )
    
}
```

Alterar a relação `pertenceLC` para `pertenceTS` para verificar esse caso.

[**Inv24:**](https://github.com/bolt12/clav#inv24)

- Alloy

```Alloy
all disj c1,c2:Classe | all n:NotaAplicacao | n in c1.temNotaAplicacao => n not in c2.temNotaAplicacao

all disj c1,c2:Classe | all n:NotaExclusao | n in c1.temNotaExclusao => n not in c2.temNotaExclusao

all disj c1,c2:Classe | all n:ExemploNotaAplicacao | n in c1.temExemploNA => n not in c2.temExemploNA
```

- SPARQL

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
select * where { 
	?c1 :temNotaAplicacao ?o.
    ?c2 :temNotaAplicacao ?o.
    FILTER (
    	?c1 != ?c2
    )
}
```

Alterar a relação `temNotaAplicacao` para as outras.

[**Inv32:**](https://github.com/bolt12/clav#inv32)

Se um PN é (por ordem de prioridade):
    - eComplementarDe   -> DF é de conservaçao
    - eSinteseDe        -> DF é de conservação
    - eSintetizadoPor   -> DF é de eliminação
    - nenhuma das acima -> DF é NE (Não especificado)

- Alloy:

```Alloy
all c:Classe_N3 | no c.temFilho => {
	!(some c.eComplementarDe) and (some c.eSinteseDe) => c.temDF in Conservacao
}
all c:Classe_N3 | no c.temFilho => {
	!(some c.eComplementarDe) and (some c.eSinteseDe) => c.temDF in Conservacao
}
all c:Classe_N3 | no c.temFilho => {
	!(some c.eComplementarDe) and !(some c.eSinteseDe) and (some c.eSintetizadoPor) => c.temDF in Eliminacao
}
all c:Classe_N3 | no c.temFilho => {
	!(some c.eComplementarDe) and !(some c.eSinteseDe) and !(some c.eSintetizadoPor) => c.temDF in NE
}
```

- SPARQL:

```SPARQL
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
select ?s where {
?s :eComplementarDe ?o .
?s :temDF ?df.
?df :dfValor ?dfv.

FILTER (?dfv = "E" || ?dfv = "NE")
}
```

```SPARQL
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
select ?s where {
?s :eSinteseDe ?o .
?s :temDF ?df.
?df :dfValor ?dfv.

FILTER (?dfv = "E" || ?dfv = "NE")
}
```

```SPARQL
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
select ?s where {
?s :eSintetizadoPor ?o .
?s :temDF ?df.
?df :dfValor ?dfv.

FILTER (?dfv = "C" || ?dfv = "NE")
}
```

```SPARQL
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
select ?s where {
	?s :rdf:type :Classe_N3 .
	minus {
		?s :eComplementarDe ?a .
	}
	minus {
		?s :eSinteseDe ?b .
	}
	minus {
		?s :eSintetizadoPor ?c .
	}
	?s :temDF ?df.
	?df :dfValor ?dfv.
	FILTER (?dfv = "C" || ?dfv = "E")
}
```