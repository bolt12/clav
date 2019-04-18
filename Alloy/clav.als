open util/boolean
open RelCalc

/* Atributo Composto */
abstract sig CriterioJustificacao {
	critTemLegAssoc: set Legislacao,
	critTemProcRel: set Classe_N3
}
  sig CriterioJustificacaoComplementaridadeInfo extends CriterioJustificacao {}
  sig CriterioJustificacaoDensidadeInfo extends CriterioJustificacao {}
  sig CriterioJustificacaoGestionario extends CriterioJustificacao {}
  sig CriterioJustificacaoLegal extends CriterioJustificacao {}
  sig CriterioJustificacaoUtilidadeAdministrativa extends CriterioJustificacao {}

abstract sig DestinoFinal {
	temJustificacao: one Justificacao
}
sig Eliminacao, Conservacao, NE extends DestinoFinal {} /* dfValor :: DF -> String */

sig ExemploNotaAplicacao {}

/* Justificacao */
abstract sig Justificacao {
	temCriterio: some CriterioJustificacao
}
  sig JustificacaoDF extends Justificacao {}
  sig JustificacaoPCA extends Justificacao {}
/* ----- */

sig NotaAplicacao {
	naPertenceClasse: one Classe
}

sig NotaExclusao {
	nePertenceClasse: one Classe,
	usarClasse: set Classe
}

sig PCA {
	temJustificacao: one Justificacao,
	valor: one Int
}
/* ----- */

/* Classe */
abstract sig Classe {
	pertenceLC: lone ListaConsolidada,
	temNotaAplicacao: set NotaAplicacao,
	temNotaExclusao: set NotaExclusao,
	pertenceTS: lone TabelaSelecao,
	temExemploNA: set ExemploNotaAplicacao
}

sig Classe_N1 extends Classe {
	temFilho: one Classe_N2,
}
sig Classe_N2 extends Classe {
	temPai: one Classe_N1,
	temFilho: set Classe_N3,
}

sig Classe_N3 extends Classe {
	temPai: one Classe_N2,
	temDono: some EntidadeResponsavel, 
	temFilho: set Classe_N4,
	temTI: set TermoIndice,
	/* temRelProc */
    eAntecessorDe: lone Classe_N3,
    eComplementarDe: set Classe_N3, 
    eCruzadoCom: set Classe_N3,
    eSinteseDe: set Classe_N3,
    eSintetizadoPor: set Classe_N3,
    eSucessorDe: lone Classe_N3,
    eSuplementoDe: set Classe_N3,
    eSuplementoPara: set Classe_N3,
	/* ----- */
    /* temParticipante */
    temParticipanteComunicador: set EntidadeResponsavel,
    temParticipanteIniciador: set EntidadeResponsavel,
    temParticipanteApreciador: set EntidadeResponsavel,
    temParticipanteDecisor: set EntidadeResponsavel,
    temParticipanteAssessor: set EntidadeResponsavel,
    temParticipanteExecutor: set EntidadeResponsavel,
	/* ----- */
    temLegislacao: set Legislacao,
    temDF: lone DestinoFinal,
    temPCA: lone PCA,

	/* Relações de atributo */
	processoTransversal: one Bool
	/* ----- */
}

sig Classe_N4 extends Classe {
	temPai: one Classe_N3,
	temTI: set TermoIndice, // Partilha entre irmaos -- VER CAMPOS
	/* temRelProc */
	eSinteseDe: set Classe_N4,
    eSintetizadoPor: set Classe_N4,
	/* ----- */
	temDF: one DestinoFinal,
	temPCA: one PCA
}

/* Factos Classe_N4 */
fact classe_n4 {
	no Classe_N4.temNotaAplicacao
	no Classe_N4.temNotaExclusao
	no Classe_N4.temExemploNA
}
/* ----- */

abstract sig EntidadeResponsavel {
	eDonoProcesso: set Classe_N3,
 	/* participaEm */
    participaEmComunicando: set Classe_N3,
    participaEmIniciando: set Classe_N3,
    participaEmApreciando: set Classe_N3,
    participaEmDecidindo: set Classe_N3,
    participaEmAssessorando: set Classe_N3,
    participaEmExecutando: set Classe_N3,
	/* ----- */
}

sig Entidade extends EntidadeResponsavel {
	pertenceTipologiaEnt: set TipologiaEntidade
}

sig TipologiaEntidade extends EntidadeResponsavel {
	contemEntidade: some Entidade
}

sig Legislacao {
	temEntidadeResponsavel: set EntidadeResponsavel
}

/* ReferencialClassificativo */
abstract sig ReferencialClassificativo {
	temClasse: set Classe
}
  sig ListaConsolidada extends ReferencialClassificativo {}
  sig TabelaSelecao extends ReferencialClassificativo {}
/* ----- */

sig TermoIndice {
	estaAssocClasse: one Classe_N3 + Classe_N4
}

/* FACTOS */
fact {
	/* Relações inversas */
	-- pertenceLC
	pertenceLC in ~temClasse

	-- temFilho
	~(Classe_N1<:temFilho) = (Classe_N2<:temPai)
	~(Classe_N2<:temFilho) = (Classe_N3<:temPai)
	~(Classe_N3<:temFilho) = (Classe_N4<:temPai)

	-- temNotaAplicacao
	~temNotaAplicacao in naPertenceClasse

	-- temNotaExclusão
	~temNotaExclusao in nePertenceClasse

	-- temDono
	~eDonoProcesso in temDono

	-- temTI
	~(Classe_N3<:temTI) in estaAssocClasse
	~(Classe_N4<:temTI) in estaAssocClasse
	
	-- temRelProc
	  -- eAntecessorDe/eSucessorDe
	  ~eAntecessorDe = eSucessorDe
      -- eSinteseDe/eSintetizadoPor
	  ~(Classe_N3<:eSinteseDe) = Classe_N3<:eSintetizadoPor
	  ~(Classe_N4<:eSinteseDe) = Classe_N4<:eSintetizadoPor
      -- eSuplementoDe/eSuplementoPara
	  ~eSuplementoDe = eSuplementoPara

	-- temParticipante
      -- temParticipanteComunicador
	  ~participaEmComunicando in temParticipanteComunicador
      -- temParticipanteIniciador
      ~participaEmIniciando in temParticipanteIniciador
      -- temParticipanteApreciador
      ~participaEmApreciando in temParticipanteApreciador
      -- temParticipanteDecisor
      ~participaEmDecidindo in temParticipanteDecisor
      -- temParticipanteAssessor
      ~participaEmAssessorando in temParticipanteAssessor
      -- temParticipanteExecutor
      ~participaEmExecutando in temParticipanteExecutor

	-- temLegislacao VER NO FUTURO

	-- contemEntidade
	~contemEntidade = pertenceTipologiaEnt
	/* ----- */
}

/* INVARIANTES */

/* Se uma Classe_N1 pertence a uma LC/TS, consequentemente os seus filhos,netos,etc..
   tambem têm de pertencer
*/
pred inv1 {
	all c:Classe_N1,lc:ListaConsolidada | lc = c.pertenceLC =>
		(all cf:c.temFilho | lc = cf.pertenceLC) and
		(all cf:c.temFilho.temFilho | lc = cf.pertenceLC) and
	    (all cf:c.temFilho.temFilho.temFilho | lc = cf.pertenceLC)

	all c:Classe_N1,ts:TabelaSelecao | ts = c.pertenceTS =>
		(all cf:c.temFilho | ts = cf.pertenceTS) and
		(all cf:c.temFilho.temFilho | ts = cf.pertenceTS) and
	    (all cf:c.temFilho.temFilho.temFilho | ts = cf.pertenceTS)
}

/* Se uma Classe_N1 não pertence a uma LC/TS, consequentemente os seus filhos,netos,etc.. tambem não pertencem */
pred inv2 {
	all c:Classe_N1 | no c.pertenceLC =>
		(all cf:c.temFilho | no cf.pertenceLC) and
		(all cf:c.temFilho.temFilho | no cf.pertenceLC) and
	    (all cf:c.temFilho.temFilho.temFilho | no cf.pertenceLC)

	all c:Classe_N1 | no c.pertenceTS =>
		(all cf:c.temFilho | no cf.pertenceTS) and
		(all cf:c.temFilho.temFilho | no cf.pertenceTS) and
	    (all cf:c.temFilho.temFilho.temFilho | no cf.pertenceTS)
}

/* As relações temDF e temPCA, não existem numa classe 3 se esta tiver filhos. */
pred inv3 {
	all c:Classe_N3 | some c.temFilho => no c.temDF and no c.temPCA
}

/* As relações temDF e temPCA, existem numa classe 3 se esta não tiver filhos. */
pred inv4 {
	all c:Classe_N3 | no c.temFilho => one c.temDF and one c.temPCA
}

/* Nas classes 3 com desdobramento */

/* Os 4ºs niveis herdam as legislações existentes no 3º nivel (subconjunto), quer para o PCA quer parao DF. */
pred inv5 {
	all c:Classe_N3 | some c.temFilho => {
	(c.temFilho.temPCA.temJustificacao.temCriterio.critTemLegAssoc 
	 + c.temFilho.temDF.temJustificacao.temCriterio.critTemLegAssoc) in c.temLegislacao
	}
}

/* Apenas se desdobram devido a um PCA distinto ou DF distinto */
pred inv6 {
	all c:Classe_N3 | #c.temFilho > 1 => (#c.temFilho.temDF > 1) or (#c.temFilho.temPCA > 1)
}

/* Caso o motivo de desdobramento seja PCA distinto:
     - Caso o DF seja distinto tem que haver uma relação de sintese entre as classes 4 filhas
*/
pred inv7 {
	all c:Classe_N3 | #c.temFilho > 1 and (#c.temFilho.temPCA > 1) => (all disj c1,c2:c.temFilho |
					  c1->c2 in eSinteseDe <=> 
					  (c1.temDF in Conservacao) and (c2.temDF in Eliminacao))
	}

/* Caso o motivo de desdobramento seja DF distinto:
     - Tem que haver uma relação de sintese entre as classes 4 filhas
	 - O PCA é igual
*/
pred inv8 {
	all c:Classe_N3 | #c.temFilho > 1 and 
					  (c.temFilho.temDF not in Eliminacao) and
					  (c.temFilho.temDF not in Conservacao) =>
					  one c.temFilho.temPCA.valor and
					  (all disj c1,c2:c.temFilho {
 					  	(c1.temDF in Conservacao) and (c2.temDF in Eliminacao) =>
					  	c1->c2 in eSinteseDe
					  })
}

/* Os termos de indice vão para os 4ºs niveis */
pred inv9 {
	all c:Classe_N3,t:c.temTI | some c.temFilho => t in c.temFilho.temTI
}

/* Se um PN (Classe 3) for complementar de outro que se desdobra ao 4o nível, é necessário, 
   com base no critério de complementaridade informacional, a relação manter-se ao 3o nível. 
   Pelo menos um dos 4os níveis deve ser de conservação. */
pred inv10 {
	all disj c1,c2:Classe_N3 | c1 in c2.eComplementarDe and some c2.temFilho => some c3:c2.temFilho | c3.temDF in Conservacao
}

/* Um processo só tem participantes se for transveral */
pred inv11 {
	all c:Classe_N3 | False = c.processoTransversal => no 
		(c.temParticipanteComunicador 
		+ c.temParticipanteIniciador
		+ c.temParticipanteApreciador
		+ c.temParticipanteDecisor
    	+ c.temParticipanteAssessor
    	+ c.temParticipanteExecutor)
}

/* As relações eComplementarDe e eCruzadoCom são simétricas. */
pred inv12 {
	Symmetric[eComplementarDe]
	Symmetric[eCruzadoCom]
}

/* Na relação temRelProc um PN não se relaciona com ele próprio */
pred inv13 {
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
}

/* As relações eSinteseDe, eSucessorDe e eSuplementoDe são assimétricas. */
pred inv14 {
	Antisymmetric[Classe_N3<:eSinteseDe, Classe_N3]
	Antisymmetric[Classe_N3<:eSintetizadoPor, Classe_N3]
	Antisymmetric[Classe_N4<:eSinteseDe, Classe_N4]
	Antisymmetric[Classe_N4<:eSintetizadoPor, Classe_N4]

	Antisymmetric[eSucessorDe, Classe_N3]
	Antisymmetric[eAntecessorDe, Classe_N3]

	Antisymmetric[eSuplementoDe, Classe_N3]
	Antisymmetric[eSuplementoPara, Classe_N3]
}

/* Um PN só pode ter uma relação com outro PN */
pred inv15 {
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
}

/* Se um PN se desdobra em 4ºs nivéis os Termos de Indice passam para os filhos */
pred inv16 {
	all c:Classe_N3 | some c.temFilho => no c.temTI
}

/* Os Termos de Índice de um PN não podem existir em mais nenhuma classe 3 */
pred inv17 {
	all disj c1,c2:Classe_N3,ti:TermoIndice | ti in c1.temTI => ti not in c2.temTI
}

/* 2 DFs nao podem ter a mesma instancia de Justificacao */
pred inv18 {
	all disj d1,d2:DestinoFinal | d1.temJustificacao != d2.temJustificacao
}

/* 2 DFs nao podem ter a mesma instancia de Justificacao */
pred inv19 {
	all disj pca1,pca2:PCA | pca1.temJustificacao != pca2.temJustificacao
}

/* 2 PNs nao podem ter a mesma instancia de DF */
pred inv20 {
	all disj c1,c2:Classe_N3 {
		c1.temDF != c2.temDF
	}
	all disj c1,c2:Classe_N4 {
		c1.temDF != c2.temDF
	}
	all disj c1:Classe_N3,c2:Classe_N4 {
		c1.temDF != c2.temDF
	}
}

/* 2 PNs nao podem ter a mesma instancia de PCA */
pred inv21 {
	all disj c1,c2:Classe_N3 {
		c1.temPCA != c2.temPCA
	}
	all disj c1,c2:Classe_N4 {
		c1.temPCA != c2.temPCA
	}
	all disj c1:Classe_N3,c2:Classe_N4 {
		c1.temPCA != c2.temPCA
	}
}

/* Cada Justificacao tem no maximo 3 CriterioJustificacao diferentes */ 
pred inv22 {
	all j:Justificacao | #j.temCriterio <= 3
}

/* 2 Justificações nao podem ter a mesma instancia de CriterioJustificacao */
pred inv23 {
	all disj j1,j2:Justificacao | all c:CriterioJustificacao | c in j1.temCriterio => c not in j2.temCriterio
}

/* 2 Classes nao podem ter a mesma instancia NotaAplicacao
   2 Classes nao podem ter a mesma instancia NotaExclusao
   2 Classes nao podem ter a mesma instancia ExemploNotaAplicacao
*/
pred inv24 {
	all disj c1,c2:Classe | all n:NotaAplicacao | n in c1.temNotaAplicacao => n not in c2.temNotaAplicacao
	all disj c1,c2:Classe | all n:NotaExclusao | n in c1.temNotaExclusao => n not in c2.temNotaExclusao
	all disj c1,c2:Classe | all n:ExemploNotaAplicacao | n in c1.temExemploNA => n not in c2.temExemploNA
}

/* 2 Classes N3 nao podem ter os mesmos filhos */
pred inv25 {
	all disj c1,c2:Classe_N3 | all c3:Classe_N4 | c3 in c1.temFilho => c3 not in c2.temFilho
}

/* 2 Classe_N3 nao podem ter o mesmo TI */
pred inv26 {
	all disj c1,c2:Classe_N3 | all t:TermoIndice | t in c1.temTI => t not in c2.temTI
}

/* UM DF, na sua justificação, apenas deve ter uma instancia de JustificacaoDF */
pred inv27 {
	all df:DestinoFinal | df.temJustificacao in JustificacaoDF
}

/* UM PCA, na sua justificação, apenas deve ter uma instancia de JustificacaoPCA */
pred inv28 {
	all pca:PCA | pca.temJustificacao in JustificacaoPCA
}

/* Quando o PN (sem filhos) em causa `eSuplementoPara` outro, 
   deve ser acrescentado um critério de utilidade administrativa na justificação do respetivo PCA
*/
pred inv29 {
	all c:Classe_N3 | no c.temFilho and some c.eSuplementoPara => one crit:CriterioJustificacaoUtilidadeAdministrativa {
		crit in c.temPCA.temJustificacao.temCriterio and crit.critTemProcRel = c.eSuplementoPara
	}
}

/* Um DF, na sua justificação, deverá conter apenas critérios de densidade informacional, complementaridade informacional e legal */
pred inv30 {
	all df:DestinoFinal | all crit:df.temJustificacao.temCriterio {
					 		(crit in CriterioJustificacaoDensidadeInfo) or
							(crit in CriterioJustificacaoComplementaridadeInfo) or
							(crit in CriterioJustificacaoLegal)
						}
}

/* Um PCA, na sua justificação, deverá conter apenas critérios gestionários, utilidade administrativa e legal */
pred inv31 {
	all pca:PCA | all crit:pca.temJustificacao.temCriterio {
					 		(crit in CriterioJustificacaoGestionario) or
							(crit in CriterioJustificacaoUtilidadeAdministrativa) or
							(crit in CriterioJustificacaoLegal)
						}
}

/* 
Se um PN é (por ordem de prioridade):
    - eComplementarDe   -> DF é de conservaçao
    - eSinteseDe        -> DF é de conservação
    - eSintetizadoPor   -> DF é de eliminação
    - nenhuma das acima -> DF é NE (Não especificado)
*/
pred inv32 {
	all c:Classe_N3 | no c.temFilho => {
		some c.eComplementarDe => c.temDF in Conservacao
		!(some c.eComplementarDe) and (some c.eSinteseDe) => c.temDF in Conservacao
		!((!some c.eComplementarDe) and (some c.eSinteseDe)) and (some c.eSintetizadoPor) => c.temDF in Eliminacao
		!(!((!some c.eComplementarDe) and (some c.eSinteseDe)) and (some c.eSintetizadoPor)) => c.temDF in NE
	}
}

/* Quando o PN (com filhos) em causa `eSuplementoPara` outro, 
   deve ser acrescentado um critério de utilidade administrativa na 
   justificação PCA dos filhos em que os processos relacionados com
   o critério podem ser distintos. 
*/
pred inv33 {
	all c:Classe_N3 | some c.temFilho and some c.eSuplementoPara => 
		all c2:c.temFilho {
			one crit:CriterioJustificacaoUtilidadeAdministrativa {
				crit in c2.temPCA.temJustificacao.temCriterio
				crit.critTemProcRel in c.eSuplementoPara
			}
		}
}

/* Todos os processos relacionados (sem filhos) por uma relação de síntese deverão
   estar relacionados com o critério de densidade informacional da
   respetiva justificação.
*/
pred inv34 {
	all c:Classe_N3 | no c.temFilho and (some c.eSinteseDe) => one crit:CriterioJustificacaoDensidadeInfo {
		crit in c.temDF.temJustificacao.temCriterio and crit.critTemProcRel = c.eSinteseDe
	}
	all c:Classe_N3 | no c.temFilho and (some c.eSintetizadoPor) => one crit:CriterioJustificacaoDensidadeInfo {
		crit in c.temDF.temJustificacao.temCriterio and crit.critTemProcRel = c.eSintetizadoPor
	}
}

/* Todos os processos relacionados (com filhos) por uma relação de síntese deverão
   estar relacionados com o critério de densidade informacional da
   respetiva justificação.
*/
pred inv35 {
	all c:Classe_N3 | some c.temFilho and (some c.eSinteseDe) => 
		all c2:c.temFilho {
			one crit:CriterioJustificacaoDensidadeInfo {
				crit in c2.temDF.temJustificacao.temCriterio and crit.critTemProcRel = c.eSinteseDe
			}
		}
	all c:Classe_N3 | some c.temFilho and (some c.eSintetizadoPor) => 
		all c2:c.temFilho {
			one crit:CriterioJustificacaoDensidadeInfo {
				crit in c2.temDF.temJustificacao.temCriterio and crit.critTemProcRel = c.eSintetizadoPor
			}
		}
}

/*
	Todos os processos relacionados (sem filhos) pela relação eComplementarDe,
	devem estar relacionados com o critério de complementaridade
	informacional da respetiva justificação.
*/
pred inv36 {
	all c:Classe_N3 | no c.temFilho and (some c.eComplementarDe) => one crit: CriterioJustificacaoComplementaridadeInfo {
		crit in c.temDF.temJustificacao.temCriterio and crit.critTemProcRel = c.eComplementarDe
	}
	all c:Classe_N3 | no c.temFilho and (some eComplementarDe.c) => one crit: CriterioJustificacaoComplementaridadeInfo {
		crit in c.temDF.temJustificacao.temCriterio and crit.critTemProcRel = eComplementarDe.c
	}
}

/*
	Todos os processos relacionados (com filhos) pela relação eComplementarDe,
	os filhos devem estar relacionados com o critério de complementaridade
	informacional da respetiva justificação.
*/
pred inv37 {
	all c:Classe_N3 | some c.temFilho and (some c.eComplementarDe) => 
		all c2:c.temFilho {
			one crit: CriterioJustificacaoComplementaridadeInfo {
				crit in c2.temDF.temJustificacao.temCriterio and crit.critTemProcRel = c.eComplementarDe
			}
		}
	all c:Classe_N3 | some c.temFilho and (some eComplementarDe.c) => 
		all c2:c.temFilho {
			one crit: CriterioJustificacaoComplementaridadeInfo {
				crit in c2.temDF.temJustificacao.temCriterio and crit.critTemProcRel = eComplementarDe.c
			}
		}
}

/*
	Cada justificação tem no máximo 1 Criterio de cada tipo.
*/
pred inv38 {
	all j:Justificacao {
		lone crit1:CriterioJustificacaoComplementaridadeInfo | crit1 in j.temCriterio
  		lone crit2:CriterioJustificacaoDensidadeInfo | crit2 in j.temCriterio
  		lone crit3:CriterioJustificacaoGestionario | crit3 in j.temCriterio
  		lone crit4:CriterioJustificacaoLegal | crit4 in j.temCriterio
  		lone crit5:CriterioJustificacaoUtilidadeAdministrativa | crit5 in j.temCriterio
	}
}

/* RUN */
run {
	 Classe_N1->ListaConsolidada in Classe_N1<:pertenceLC
	 --some eSuplementoPara
	 --some (Classe_N3<:eSinteseDe)
	 --some eComplementarDe
	 --some eAntecessorDe
	 --some eCruzadoCom
	 inv1
	 inv2
	 inv3
	 inv4
	 inv5
	 inv6
	 inv7
	 inv8
	 inv9
	 inv10
	 inv11
	 inv12
	 inv13
	 inv14
	 inv15
	 inv16
	 inv17
	 inv18
	 inv19
	 inv20
	 inv21
	 inv22
	 inv23
	 inv24
	 inv25
	 inv26
	 inv27
	 inv28
	 inv29
	 inv30
	 inv31
	 --inv32
	 inv33
	 inv34
	 inv35
	 inv36
	 inv37
	 inv38
} for 4 but exactly 1 ListaConsolidada, exactly 1 Classe_N1, exactly 1 Classe_N2, exactly 1 Classe_N3, exactly 2 Classe_N4
