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
sig Eliminacao, Conservacao extends DestinoFinal {} /* dfValor :: DF -> String */

sig ExemploNotaAplicacao {}

/* Justificacao */
abstract sig Justificacao {
	temCriterio: some CriterioJustificacao
}
  sig JustificacaoDF extends Justificacao {}
  sig JustificacaoPCA extends Justificacao {}
/* ----- */

sig NotaAplicacao {
	naPertenceClasse: one Classe_N1 + Classe_N2 + Classe_N3
}
sig NotaExclusao {
	nePertenceClasse: one Classe_N1 + Classe_N2 + Classe_N3,
	usarClasse: set Classe_N1 + Classe_N2 + Classe_N3 + Classe_N4
}
sig PCA {
	temJustificacao: one Justificacao
}
/* ----- */

/* Classe */
sig Classe_N1 {
	pertenceLC: lone ListaConsolidada,
	temFilho: one Classe_N2,
	temNotaAplicacao: set NotaAplicacao,
	temNotaExclusao: set NotaExclusao,
	pertenceTS: lone TabelaSelecao,
	temExemploNA: set ExemploNotaAplicacao
}
sig Classe_N2 {
	pertenceLC: lone ListaConsolidada,
	temPai: one Classe_N1,
	temFilho: set Classe_N3,
	temNotaAplicacao: set NotaAplicacao,
	temNotaExclusao: set NotaExclusao,
	pertenceTS: lone TabelaSelecao,
	temExemploNA: set ExemploNotaAplicacao
}
sig Classe_N3 {
	pertenceLC: lone ListaConsolidada,
	temPai: one Classe_N2,
	temDono: some Entidade + TipologiaEntidade, 
	temFilho: set Classe_N4,
	temNotaAplicacao: set NotaAplicacao,
	temNotaExclusao: set NotaExclusao,
	pertenceTS: lone TabelaSelecao,
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
    temParticipanteComunicador: set Entidade + TipologiaEntidade,
    temParticipanteIniciador: set Entidade + TipologiaEntidade,
    temParticipanteApreciador: set Entidade + TipologiaEntidade,
    temParticipanteDecisor: set Entidade + TipologiaEntidade,
    temParticipanteAssessor: set Entidade + TipologiaEntidade,
    temParticipanteExecutor: set Entidade + TipologiaEntidade,
	/* ----- */
    temLegislacao: set Legislacao,
    temDF: lone DestinoFinal,
    temPCA: lone PCA,
    temExemploNA: set ExemploNotaAplicacao,

	/* Relações de atributo */
	processoTransversal: one Bool
	/* ----- */
}
sig Classe_N4 {
	pertenceLC : lone ListaConsolidada,
	temPai: one Classe_N3,
	pertenceTS: lone TabelaSelecao,
	temTI: set TermoIndice, // Partilha entre irmaos -- VER CAMPOS
	/* temRelProc */
	eSinteseDe: set Classe_N4,
    eSintetizadoPor: set Classe_N4,
	temDF: one DestinoFinal,
	temPCA: one PCA,
	temLegislacao: Legislacao /* VER NO FUTURO */
}
/* ----- */

sig Entidade  {
	eDonoProcesso: set Classe_N3,
 	/* participaEm */
    participaEmComunicando: set Classe_N3,
    participaEmIniciando: set Classe_N3,
    participaEmApreciando: set Classe_N3,
    participaEmDecidindo: set Classe_N3,
    participaEmAssessorando: set Classe_N3,
    participaEmExecutando: set Classe_N3,
	/* ----- */
	pertenceTipologiaEnt: set TipologiaEntidade
}
sig Legislacao {
	temEntidadeResponsavel: set Entidade + TipologiaEntidade
}

/* ReferencialClassificativo */
abstract sig ReferencialClassificativo {
	temClasse: set Classe_N1 + Classe_N2 + Classe_N3 + Classe_N4
}
  sig ListaConsolidada extends ReferencialClassificativo {}
  sig TabelaSelecao extends ReferencialClassificativo {}
/* ----- */

sig TermoIndice {
	estaAssocClasse: one Classe_N3 + Classe_N4
}
sig TipologiaEntidade {
	eDonoProcesso: set Classe_N3,
    /* participaEm */
    participaEmComunicando: set Classe_N3,
    participaEmIniciando: set Classe_N3,
    participaEmApreciando: set Classe_N3,
    participaEmDecidindo: set Classe_N3,
    participaEmAssessorando: set Classe_N3,
    participaEmExecutando: set Classe_N3,
	/* ----- */
	contemEntidade: some Entidade
}

/* FACTOS */
fact {
	/* Relações inversas */
	-- pertenceLC
	(Classe_N1<:pertenceLC) in ~temClasse
	(Classe_N2<:pertenceLC) in ~temClasse
	(Classe_N3<:pertenceLC) in ~temClasse
	(Classe_N4<:pertenceLC) in ~temClasse

	-- temFilho
	~(Classe_N1<:temFilho) = (Classe_N2<:temPai)
	~(Classe_N2<:temFilho) = (Classe_N3<:temPai)
	~(Classe_N3<:temFilho) = (Classe_N4<:temPai)

	-- temNotaAplicacao
	~(Classe_N1<:temNotaAplicacao) in naPertenceClasse
	~(Classe_N2<:temNotaAplicacao) in naPertenceClasse
	~(Classe_N3<:temNotaAplicacao) in naPertenceClasse

	-- temNotaExclusão
	~(Classe_N1<:temNotaExclusao) in nePertenceClasse
	~(Classe_N2<:temNotaExclusao) in nePertenceClasse
	~(Classe_N3<:temNotaExclusao) in nePertenceClasse

	-- temDono
	~(Entidade<:eDonoProcesso) in temDono
	~(Entidade<:eDonoProcesso) in temDono

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
	  ~(Entidade<:participaEmComunicando) in temParticipanteComunicador
	  ~(TipologiaEntidade<:participaEmComunicando) in temParticipanteComunicador
      -- temParticipanteIniciador
      ~(Entidade<:participaEmIniciando) in temParticipanteIniciador
	  ~(TipologiaEntidade<:participaEmIniciando) in temParticipanteIniciador
      -- temParticipanteApreciador
      ~(Entidade<:participaEmApreciando) in temParticipanteApreciador
	  ~(TipologiaEntidade<:participaEmApreciando) in temParticipanteApreciador
      -- temParticipanteDecisor
      ~(Entidade<:participaEmDecidindo) in temParticipanteDecisor
	  ~(TipologiaEntidade<:participaEmDecidindo) in temParticipanteDecisor
      -- temParticipanteAssessor
      ~(Entidade<:participaEmAssessorando) in temParticipanteAssessor
	  ~(TipologiaEntidade<:participaEmAssessorando) in temParticipanteAssessor
      -- temParticipanteExecutor
      ~(Entidade<:participaEmExecutando) in temParticipanteExecutor
	  ~(TipologiaEntidade<:participaEmExecutando) in temParticipanteExecutor

	-- temLegislacao VER NO FUTURO

	-- contemEntidade
	~contemEntidade = pertenceTipologiaEnt
	/* ----- */
}

/* INVARIANTES */

/* Se uma Classe_N1 pertence a uma LC/TS, consequentemente os seus filhos,netos,etc.. tambem têm de pertencer */
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
pred inv7 {
	all c:Classe_N3 | #c.temFilho > 1 and (#c.temFilho.temPCA > 1) => (#c.temFilho.temDF > 1) => relacaosintese
} */

/* Caso o motivo de desdobramento seja DF distinto:
     - Tem que haver uma relação de sintese entre as classes 4 filhas
	 - O PCA é igual
pred inv8 {
	all c:Classe_N3 | #c.temFilho > 1 and (#c.temFilho.temDF > 1) => (all disj c2,c3:c.temFilho {
		(c2.temPCA in Eliminacao and c3.temPCA in Eliminacao) or (c2.temPCA in Conservacao and c3.temPCA in Conservacao) and relacaosintese
	})
} */

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
	not Symmetric[Classe_N3<:eSinteseDe]
	not Symmetric[Classe_N3<:eSintetizadoPor]
	not Symmetric[Classe_N4<:eSinteseDe]
	not Symmetric[Classe_N4<:eSintetizadoPor]

	not Symmetric[eSucessorDe]
	not Symmetric[eAntecessorDe]

	not Symmetric[eSuplementoDe]
	not Symmetric[eSuplementoPara]
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

/* 2 PNs nao podem ter a mesma instancia de DF */
pred inv19 {
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
pred inv20 {
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
pred inv21 {
	all j:Justificacao | #j.temCriterio <= 3
}

/* 2 Justificações nao podem ter a mesma instancia de CriterioJustificacao */
pred inv22 {
	all disj j1,j2:Justificacao | all c:CriterioJustificacao | c in j1.temCriterio => c not in j2.temCriterio
}

/* 2 Classe_N1 nao podem ter a mesma instancia NotaAplicacao
   2 Classe_N1 nao podem ter a mesma instancia NotaExclusao
   2 Classe_N1 nao podem ter a mesma instancia ExemploNotaAplicacao
*/
pred inv23 {
	all disj c1,c2:Classe_N1 | all n:NotaAplicacao | n in c1.temNotaAplicacao => n not in c2.temNotaAplicacao
	all disj c1,c2:Classe_N1 | all n:NotaExclusao | n in c1.temNotaExclusao => n not in c2.temNotaExclusao
	all disj c1,c2:Classe_N1 | all n:ExemploNotaAplicacao | n in c1.temExemploNA => n not in c2.temExemploNA
}

/* 2 Classe_N2 nao podem ter a mesma instancia NotaAplicacao
   2 Classe_N2 nao podem ter a mesma instancia NotaExclusao
   2 Classe_N2 nao podem ter a mesma instancia ExemploNotaAplicacao
*/
pred inv24 {
	all disj c1,c2:Classe_N2 | all n:NotaAplicacao | n in c1.temNotaAplicacao => n not in c2.temNotaAplicacao
	all disj c1,c2:Classe_N2 | all n:NotaExclusao | n in c1.temNotaExclusao => n not in c2.temNotaExclusao
	all disj c1,c2:Classe_N2 | all n:ExemploNotaAplicacao | n in c1.temExemploNA => n not in c2.temExemploNA
}

/* 2 Classe_N3 nao podem ter a mesma instancia NotaAplicacao
   2 Classe_N3 nao podem ter a mesma instancia NotaExclusao
   2 Classe_N3 nao podem ter a mesma instancia ExemploNotaAplicacao
*/
pred inv25 {
	all disj c1,c2:Classe_N3 | all n:NotaAplicacao | n in c1.temNotaAplicacao => n not in c2.temNotaAplicacao
	all disj c1,c2:Classe_N3 | all n:NotaExclusao | n in c1.temNotaExclusao => n not in c2.temNotaExclusao
	all disj c1,c2:Classe_N3 | all n:ExemploNotaAplicacao | n in c1.temExemploNA => n not in c2.temExemploNA
}

/* 2 Classes N3 nao podem ter os mesmos filhos */
pred inv26 {
	all disj c1,c2:Classe_N3 | all c3:Classe_N4 | c3 in c1.temFilho => c3 not in c2.temFilho
}

/* 2 Classe_N3 nao podem ter o mesmo TI */
pred inv27 {
	all disj c1,c2:Classe_N3 | all t:TermoIndice | t in c1.temTI => t not in c2.temTI
}

/* RUN */
run {
	 Classe_N1->ListaConsolidada in Classe_N1<:pertenceLC
	 -- False = Classe_N3.processoTransversal
	 inv1
	 inv2
	 inv3
	 inv4
	 inv5
	 inv6
	 --inv7
	 --inv8
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
} for 3 but exactly 1 ListaConsolidada, 1 Classe_N1, 1 Classe_N2, 2 Classe_N3, exactly 2 Classe_N4
