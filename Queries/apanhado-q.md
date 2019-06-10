
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

### Notas uteis

- Lei de DeMorgan: 
    - -(P \/ Q) <=> -P /\ -Q
    - -(P /\ Q) <=> -P \/ -Q

- Implicação:
    - A -> B <=> - A \/ B
    - -(A -> B) <=> A /\ -B

### Traduções

[**Inv1:**](https://github.com/bolt12/clav#inv1)

Se uma Classe_N1 pertence a uma LC/TS, consequentemente os seus filhos,netos,etc.. tambem têm de pertencer.

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
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where { 
    ?c1 rdf:type :Classe_N1 .
    ?c1 :pertenceLC ?lc1 .
    
    ?c2 rdf:type :Classe_N2 .
    ?c2 :temPai ?c1 .
    ?c2 :pertenceLC ?lc2 .
    
    ?c3 rdf:type :Classe_N3 .
    ?c3 :temPai ?c2 .
    ?c3 :pertenceLC ?lc3 .
    
    ?c4 rdf:type :Classe_N4 .
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

[**Inv2**](https://github.com/bolt12/clav#inv2)

Se uma Classe_N1 *não* pertence a uma LC/TS, consequentemente os seus filhos,netos,etc.. tambem não pertencem.

- Alloy

```Alloy
all c:Classe_N1 | no c.pertenceLC =>
	(all cf:c.temFilho | no cf.pertenceLC) and
	(all cf:c.temFilho.temFilho | no cf.pertenceLC) and
	(all cf:c.temFilho.temFilho.temFilho | no cf.pertenceLC)
```

- SPARQL

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where { 
    ?c1 rdf:type :Classe_N1 .
    
    minus {
    	?c1 :pertenceLC ?lc1 .
    }
    
    ?c2 rdf:type :Classe_N2 .
    ?c2 :temPai ?c1 .
    
    ?c3 rdf:type :Classe_N3 .
    ?c3 :temPai ?c2 .
    
    ?c4 rdf:type :Classe_N4 .
    ?c4 :temPai ?c3 .
    
    FILTER (
      ?c1 != ?c2 
      && ?c2 != ?c3
      && ?c3 != ?c4 &&
      (EXISTS { ?c2 :pertenceLC ?lc2 . }
      || EXISTS { ?c3 :pertenceLC ?lc3 . }
      || EXISTS { ?c4 :pertenceLC ?lc4 . })
    )
    
}
```

[**Inv3**](https://github.com/bolt12/clav#inv3)

As relações `temDF` e `temPCA`, não existem numa classe 3 se esta tiver filhos.

- Alloy

```Alloy
all c:Classe_N3 | some c.temFilho => no c.temDF and no c.temPCA
```

- SPARQL

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where {
   ?c rdf:type :Classe_N3 .
   ?c :temFilho ?cf .

    FILTER (
        EXISTS { ?c :temDF ?df . } ||
        EXISTS { ?c :temPCA ?pca . }
    )
}
```

[**Inv4**](https://github.com/bolt12/clav#inv4)

As relações `temDF` e `temPCA`, existem numa classe 3 se esta não tiver filhos.

- Alloy

```Alloy
all c:Classe_N3 | no c.temFilho => one c.temDF and one c.temPCA
```

- SPARQL

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where {
   ?c rdf:type :Classe_N3 .
   
   FILTER (
      NOT EXISTS { ?c :temFilho ?cf . } &&
      (NOT EXISTS { ?c :temDF ?df . } ||
      NOT EXISTS { ?c :temPCA ?pca . })
   )
}
```

[**Inv6:**](https://github.com/bolt12/clav#inv6)

Apenas se desdobram devido a um PCA distinto ou DF distinto.

- Alloy

```Alloy
all c:Classe_N3 | #c.temFilho > 1 => (#c.temFilho.temDF > 1) or
				     (#c.temFilho.temPCA > 1 
                                     and (all p1,p2:c.temFilho.temPCA | p1.valor != p2.valor))
```

- SPARQL

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where {
    ?c rdf:type :Classe_N3 .
    ?c :temFilho ?cf1 .
    ?c :temFilho ?cf2 .

    ?cf1 :temDF ?df1 .
    ?cf2 :temDF ?df2 .
    ?df1 :dfValor ?dfv1 .
    ?df2 :dfValor ?dfv2 .

    ?cf1 :temPCA ?pca1 .
    ?cf2 :temPCA ?pca2 .
    ?pca1 :pcaValor ?pcav1 .
    ?pca2 :pcaValor ?pcav2 .

    FILTER (
      ?cf1 != ?cf2
      && ?dfv1 = ?dfv2
      && ?pcav1 = ?pcav2
    )
}
```

[**Inv7:**](https://github.com/bolt12/clav#inv7)

Caso o motivo de desdobramento seja PCA distinto:

     - Caso o DF seja distinto tem que haver uma relação de sintese entre as classes 4 filhas

- Alloy

```Alloy
all c:Classe_N3 | #c.temFilho > 1
                  and (#c.temFilho.temPCA > 1)
                  and (#c.temFilho.temDF > 1)
                    => (all disj c1,c2:c.temFilho |
                      c1->c2 in eSinteseDe <=>
                      (c1.temDF in Conservacao) and (c2.temDF in Eliminacao))
  }
```

- SPARQL

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where { 
    ?c rdf:type :Classe_N3 .
    ?c :temFilho ?cf1 .
    ?c :temFilho ?cf2 .

    ?cf1 :temPCA ?pca1 .
    ?cf2 :temPCA ?pca2 .
    ?pca1 :pcaValor ?pcav1 .
    ?pca2 :pcaValor ?pcav2 .

    ?cf1 :temDF ?df1 .
    ?cf2 :temDF ?df2 .
    ?df1 :dfValor ?dfv1 .
    ?df2 :dfValor ?dfv2 .
    

    FILTER (
      ?cf1 != ?cf2
      && ?pcav1 != ?pcav2
      && ?dfv1 != ?dfv2
      && NOT EXISTS { ?cf1 :eSinteseDe ?cf2 . }
    )
}
```

[**Inv8:**](https://github.com/bolt12/clav#inv8)

Caso o motivo de desdobramento seja DF distinto:

     - Tem que haver uma relação de sintese entre as classes 4 filhas
     
     - O PCA é igual

- Alloy

```Alloy
all c:Classe_N3 | #c.temFilho > 1 
                  and (c.temFilho.temDF not in Eliminacao) 
                  and (c.temFilho.temDF not in Conservacao) =>
                    one c.temFilho.temPCA.valor 
                    and (all disj c1,c2:c.temFilho {
                        (c1.temDF in Conservacao) and (c2.temDF in Eliminacao) 
                          => c1->c2 in eSinteseDe
                        })
```

- SPARQL

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where { 
    ?c rdf:type :Classe_N3 .
    ?c :temFilho ?cf1 .
    ?c :temFilho ?cf2 .

    ?cf1 :temPCA ?pca1 .
    ?cf2 :temPCA ?pca2 .
    ?pca1 :pcaValor ?pcav1 .
    ?pca2 :pcaValor ?pcav2 .

    ?cf1 :temDF ?df1 .
    ?cf2 :temDF ?df2 .
    ?df1 :dfValor ?dfv1 .
    ?df2 :dfValor ?dfv2 .
    

    FILTER (
      ?cf1 != ?cf2
      && ?dfv1 != ?dfv2
      && (
      NOT EXISTS { ?cf1 :eSinteseDe ?cf2 . }
      || ?pcav1 != ?pcav2)
    )
}
```

[**Inv9:**](https://github.com/bolt12/clav#inv9)

Os termos de indice vão para os 4ºs niveis.

- Alloy

```Alloy
all c:Classe_N3,t:c.temTI | some c.temFilho => t in c.temFilho.temTI
```

- SPARQL

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where { 
    ?c rdf:type :Classe_N3 .
    ?c :temFilho ?cf .
    ?c :temTI ?ti .
   	
    FILTER (
        NOT EXISTS { ?cf :temTI ?ti . }
	)
}
```

[**Inv10:**](https://github.com/bolt12/clav#inv10)

Se um PN (Classe 3) for complementar de outro que se desdobra ao 4o nível, é necessário,
   com base no critério de complementaridade informacional, a relação manter-se ao 3o nível.
   Pelo menos um dos 4os níveis deve ser de conservação.

- Alloy

```Alloy
all disj c1,c2:Classe_N3 | c1 in c2.eComplementarDe 
                           and some c2.temFilho 
                            => some c3:c2.temFilho | 
                              c3.temDF in Conservacao
```

- SPARQL

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where {
    ?c1 rdf:type :Classe_N3 .
    ?c2 rdf:type :Classe_N3 .

    ?c1 :eComplementarDe ?c2 .
    ?c2 :temFilho ?c2f .

    ?c2f :temDF ?df .
    ?df :dfValor ?dfv .

    FILTER (
      ?c1 != ?c2
      && ?dfv != "C"
    )
}
```

[**Inv11:**](https://github.com/bolt12/clav#inv11)

Um processo só tem participantes se for transveral.

- Alloy

```Alloy
all c:Classe_N3 | False = c.processoTransversal 
                    => no (c.temParticipanteComunicador
                        + c.temParticipanteIniciador
                        + c.temParticipanteApreciador
                        + c.temParticipanteDecisor
                        + c.temParticipanteAssessor
                        + c.temParticipanteExecutor)
```

- SPARQL

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where {
    ?c rdf:type :Classe_N3 .
    ?c :processoTransversal "N" .
    
    FILTER (
        EXISTS { ?c :temParticipanteComunicador ?ca . }
        ||
        EXISTS { ?c :temParticipanteIniciador ?ca . }
        ||
        EXISTS { ?c :temParticipanteApreciador ?ca . }
        ||
        EXISTS { ?c :temParticipanteDecisor ?ca . }
        ||
        EXISTS { ?c :temParticipanteAssessor ?ca . }
        ||
        EXISTS { ?c :temParticipanteExecutor ?ca . }
    )
}
```

[**Inv12:**](https://github.com/bolt12/clav#inv12)

As relações `eComplementarDe` e `eCruzadoCom` são simétricas.

- Alloy

```Alloy
Symmetric[eComplementarDe]

Symmetric[eCruzadoCom]
```

- SPARQL

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where {
    ?c1 :eComplementarDe ?c2 .
    ?c3 :eCruzadoCom ?c4 .

    FILTER (
        ?c1 != ?c2 
        && ?c3 != ?c4
        && NOT EXISTS { ?c2 :eComplementarDe ?c2 . }
        && NOT EXISTS { ?c4 :eCruzadoCom ?c3 . }
    )
}
```

[**Inv13:**](https://github.com/bolt12/clav#inv13)

Na relação temRelProc um PN não se relaciona com ele próprio.

- Alloy

```Alloy
all c:Classe_N3 | c->c not in eAntecessorDe
all c:Classe_N3 | c->c not in eComplementarDe
all c:Classe_N3 | c->c not in eCruzadoCom
all c:Classe_N3 | c->c not in eSinteseDe
all c:Classe_N3 | c->c not in eSintetizadoPor
all c:Classe_N4 | c->c not in eSinteseDe
all c:Classe_N4 | c->c not in eSintetizadoPor
all c:Classe_N3 | c->c not in eSucessorDe
all c:Classe_N3 | c->c not in eSuplementoDe
all c:Classe_N3 | c->c not in eSuplementoPara
```

- SPARQL

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where {
    ?c1 rdf:type :Classe_N3 .
    ?c2 rdf:type :Classe_N4.

    FILTER (
      EXISTS { ?c1 :eAntecessorDe ?c1 . }
      ||
      EXISTS { ?c1 :eComplementarDe ?c1 . }
      ||
      EXISTS { ?c1 :eCruzadoCom ?c1 . }
      ||
      EXISTS { ?c1 :eSinteseDe ?c1 . }
      ||
      EXISTS { ?c1 :eSintetizadoPor ?c1 . }
      ||
      EXISTS { ?c2 :eSinteseDe ?c2 . }
      ||
      EXISTS { ?c2 :eSintetizadoPor ?c2 . }
      ||
      EXISTS { ?c1 :eSucessorDe ?c1 . }
      ||
      EXISTS { ?c1 :eSuplementoDe ?c1 . }
      ||
      EXISTS { ?c1 :eSuplementoPara ?c1 . }
    )

}
```

[**Inv14:**](https://github.com/bolt12/clav#inv14)

As relações `eSinteseDe`, `eSucessorDe` e `eSuplementoDe` são antisimétricas.


- Alloy

```Alloy
Antisymmetric[Classe_N3<:eSinteseDe, Classe_N3]
Antisymmetric[Classe_N3<:eSintetizadoPor, Classe_N3]
Antisymmetric[Classe_N4<:eSinteseDe, Classe_N4]
Antisymmetric[Classe_N4<:eSintetizadoPor, Classe_N4]

Antisymmetric[eSucessorDe, Classe_N3]
Antisymmetric[eAntecessorDe, Classe_N3]

Antisymmetric[eSuplementoDe, Classe_N3]
Antisymmetric[eSuplementoPara, Classe_N3]
```

- SPARQL

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where {
    ?c1 :eSinteseDe ?c2 .
    ?c2 :eSinteseDe ?c1 .

    ?c3 :eSintetizadoPor ?c4 .
    ?c4 :eSintetizadoPor ?c3 .

    ?c5 :eSucessorDe ?c6.
    ?c6 :eSucessorDe ?c5.

    ?c7 :eSuplementoDe ?c8.
    ?c8 :eSuplementoDe ?c7.

    ?c9 :eSuplementoPara ?c10.
    ?c10 :eSuplementoDe ?c9.

    FILTER (
        ?c1 != ?c2
        && ?c3 != ?c4
        && ?c5 != ?c6
        && ?c7 != ?c8
        && ?c9 != ?c10
    )

}
```


[**Inv15:**](https://github.com/bolt12/clav#inv15)

Um PN só pode ter uma relação com outro PN.

- Alloy

```Alloy
all c1,c2:Classe_N3 | c1->c2 in eAntecessorDe => c1->c2 not in
  eComplementarDe + eCruzadoCom + eSinteseDe + eSintetizadoPor + eSucessorDe + eSuplementoDe + eSuplementoPara
or
all c1,c2:Classe_N3 | c1->c2 in eComplementarDe => c1->c2 not in
  eAntecessorDe + eCruzadoCom + eSinteseDe + eSintetizadoPor + eSucessorDe + eSuplementoDe + eSuplementoPara
or
all c1,c2:Classe_N3 | c1->c2 in eCruzadoCom => c1->c2 not in
  eAntecessorDe + eComplementarDe + eSinteseDe + eSintetizadoPor + eSucessorDe + eSuplementoDe + eSuplementoPara
or
all c1,c2:Classe_N3 | c1->c2 in eSinteseDe => c1->c2 not in
  eAntecessorDe + eComplementarDe + eCruzadoCom + eSintetizadoPor + eSucessorDe + eSuplementoDe + eSuplementoPara
or
all c1,c2:Classe_N3 | c1->c2 in eSintetizadoPor => c1->c2 not in
  eAntecessorDe + eComplementarDe + eCruzadoCom + eSinteseDe + eSucessorDe + eSuplementoDe + eSuplementoPara
or
all c1,c2:Classe_N3 | c1->c2 in eSucessorDe => c1->c2 not in
  eAntecessorDe + eComplementarDe + eCruzadoCom + eSinteseDe + eSintetizadoPor + eSuplementoDe + eSuplementoPara
or
all c1,c2:Classe_N3 | c1->c2 in eSuplementoDe => c1->c2 not in
  eAntecessorDe + eComplementarDe + eCruzadoCom + eSinteseDe + eSintetizadoPor + eSucessorDe + eSuplementoPara
or
all c1,c2:Classe_N3 | c1->c2 in eSuplementoPara => c1->c2 not in
  eAntecessorDe + eComplementarDe + eCruzadoCom + eSinteseDe + eSintetizadoPor + eSucessorDe + eSuplementoDe
```

- SPARQL

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where { 
  ?c1 rdf:type :Classe_N3 .
  ?c2 rdf:type :Classe_N3 .
  ?c1 :eAntecessorDe ?c2 .

  FILTER (
    ?c1 != ?c2
    &&
    (
    EXISTS { ?c1 :eComplementarDe ?c2 . }
    ||
    EXISTS { ?c1 :eCruzadoCom ?c2 . }
    ||
    EXISTS { ?c1 :eSinteseDe ?c2 . }
    ||
    EXISTS { ?c1 :eSintetizadoPor ?c2 . }
    ||
    EXISTS { ?c1 :eSucessorDe ?c2 . }
    ||
    EXISTS { ?c1 :eSuplementoDe ?c2 . }
    ||
    EXISTS { ?c1 :eSuplementoPara ?c2 . }
    )
  )

}
```

Mais 7 queries SPARQL com permutações semelhantes às do Alloy.

[**Inv16:**](https://github.com/bolt12/clav#inv16)

Se um PN se desdobra em 4ºs nivéis os Termos de Indice passam para os filhos.

- Alloy

```Alloy
all c:Classe_N3 | some c.temFilho => no c.temTI
```

- SPARQL

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where { 
    ?c rdf:type :Classe_N3 .
    ?c :temFilho ?cf .


    FILTER (
        EXISTS { ?c :temTI ?ti .}
    )
}
```

[**Inv17:**](https://github.com/bolt12/clav#inv17)

Os Termos de Índice de um PN não podem existir em mais nenhuma classe 3.

- Alloy

```Alloy
all disj c1,c2:Classe_N3,ti:TermoIndice | ti in c1.temTI => ti not in c2.temTI
```

- SPARQL

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where { 
    ?c1 rdf:type :Classe_N3 .
    ?c2 rdf:type :Classe_N3 .

    ?c1 :temTI ?ti .

    FILTER (
      ?c1 != ?c2
      && EXISTS { ?c2 :temTI ?ti . }
    )
}
```

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
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
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
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where {
	?c rdf:type :Classe_N3 .
	?c :eComplementarDe ?o .
	?c :temDF ?df.
	?df :dfValor ?dfv.
	
	minus {
        	?c :temFilho ?f .
    	}

	FILTER (?dfv != "C")
}
```

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where {
	?c rdf:type :Classe_N3 .
	?c :eSinteseDe ?ca .
	?c :temDF ?df.
	?df :dfValor ?dfv.
	
	minus {
	        ?c :temFilho ?cf .
		?c :eComplementarDe ?cb .
	}

	FILTER (?dfv != "C")
}
```

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where {
	?c rdf:type :Classe_N3 .
	?c :eSintetizadoPor ?ca .
	?c :temDF ?df.
	?df :dfValor ?dfv.

	minus {
        	?c :temFilho ?f .
		?c :eComplementarDe ?cb .
		?c :eSinteseDe ?cc .
	}

	FILTER (?dfv != "E")
}
```

```SPARQL
PREFIX : <http://jcr.di.uminho.pt/m51-clav#>
PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
select * where {
	?c rdf:type :Classe_N3 .
	?c :temDF ?df.
	?df :dfValor ?dfv.
	minus {
        	?c :temFilho ?cf .
		?c :eComplementarDe ?ca .
		?c :eSintetizadoPor ?cb .
		?c :eSinteseDe ?cc .
	}
	
	FILTER (?dfv != "NE")
}
```
