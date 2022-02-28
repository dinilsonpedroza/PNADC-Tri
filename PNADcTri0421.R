##########################################################################################
################ Taxa de desocupação BR e UFs ############################################
##########################################################################################


### Carregando pacotes

library(PNADcIBGE)
library(convey)
library(survey)

### Selecionando as variáveis

#### Variáveis: UF (ex. Pernambuco = 26); Capital (ex. Recife = 26); RM_RIDE (ex. RMR = 26)
#### VD4001(condição na força de trabalho); VD4002(condição de ocupação); VD4004A(subocupação habitual)

variaveis_selecionadas <- c("UF", "Capital", "RM_RIDE", "VD4001", "VD4002", "VD4004A")

### Coletando os dados por raspagem, com a função get_pnadc do pacote PNADcIBGE.

PNADcBR <- get_pnadc(year = 2021, quarter = 4, vars = variaveis_selecionadas)



### Com dados baixados no PC (off line) com a função read_pnadc ############################################
### Testando ####################################################################################


dadosPNADc <- read_pnadc(microdata="PNADC_042021.txt", input_txt="input_PNADC_trimestral.txt")

### Criando um data frame com uma coluna
VD4010 <- data.frame(dadosPNADc$VD4010) ###Deu certo.
head(VD4010)

### Transformando em design
dadosPNADc <- pnadc_design(data_pnadc=dadosPNADc)

### Testando
totalrenda1 <- svytotal(x=~VD4020, design=dadosPNADc, na.rm=TRUE)
rendamédia1<-svymean(~VD4020, dadosPNADc, na.rm=T)
mediarendaM <- svymean(x=~VD4020, design=subset(dadosPNADc, V2007=="2"), na.rm=TRUE)

### Criando uma PNADc para o setor de transportes
PNADcTRA1<- subset(dadosPNADc, VD4010=="05")
mediarendaTRA1<-svymean(~VD4020, PNADcTRA1, na.rm = T)
mediarendaTRA1

### Criando uma PNADc para o setor de alojamentos
PNADcALO1 <-subset(dadosPNADc, VD4010=="06")
mediarendaALO1<-svymean(~VD4020, PNADcALO1, na.rm = T)
mediarendaALO1

### Rendas médias de transportes e alojamentos calculadas mais diretamente
mediarendaTRA2<-svymean(~VD4020, design=subset(dadosPNADc, VD4010=="05"), na.rm = T)
mediarendaTRA2
mediarendaALO2<-svymean(~VD4020, design=subset(dadosPNADc, VD4010=="06"), na.rm = T)
mediarendaALO2

### Conferindo o método off line com o de raspagem, usando o setor de APU 
mediarendaAPU1<-svymean(~VD4020, design = subset(dadosPNADc, VD4010=="08"), na.rm = T)
mediarendaAPU1
mediarendaAPU ### valores bateram.


### Calculando a taxa de desemprego do Brasil.

txdbr <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", PNADcBR, na.rm = T)

txdbr

### Taxa de desemprego do Brasil ficou em 11,75% (confirmado).


### Taxa de desemprego de Pernambuco


PNADcPE <- subset(PNADcBR, UF=="Pernambuco") ### Montando a PNADc de PE.

txdpe <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", PNADcPE, na.rm = T)

txdpe

### Taxa de desemprego de Pernambuco ficou me 17,14% (confirmado).


### Taxa de desemprego do Rio de Janeiro

PNADcRJ <- subset(PNADcBR, UF=="Rio de Janeiro") ### Montando a PNADc do Rio de Janeiro.

txdrj <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", PNADcRJ, na.rm = T)

txdrj

### Taxa de desemprego do Rio de Janeiro ficou em 14,19% (confirmado).
### Taxa de desemprego da Região Metropolitana do Recife.

PNADcRMRec <- subset(PNADcPE, RM_RIDE=="Região Metropolitana de Recife (PE)")

txdrmrec <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", PNADcRMRec, na.rm = T)

txdrmrec

### Taxa de desemprego na região metropolitana do Recife ficou em 17,73%.

### Taxa de desemprego na Região Metropolitana do Rio de Janeiro.

PNADcRMRJ <- subset(PNADcRJ, RM_RIDE=="Região Metropolitana de Rio de Janeiro (RJ)")

txdrmrj <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", PNADcRMRJ, na.rm = T)

txdrmrj

### Taxa de desemprego da região metropolitana do Rio de Janeiro ficou em 14,18%.


##################################################################################################
############## Explorando os dados ###############################################################
##################################################################################################

### Variáveis:
### V2007 = Sexo (1 = Homem, 2 = Mulher)
### V2009 = Idade do morador.
### V2010 = Cor ou raça.
### VD3004 = Nível de instrução.
### VD4001 = Condição em relação à força de trabalho (1 = na força, 2 = fora).
### VD4002 = Condição de ocupação (1 = ocupada, 2 = desocupada)
### VD4004A = Subocupação habitual.
### VD4005 = Pessoas desalentadas.
### VD4010 = Grupamentos de atividade (ex.: 11 = Serviços domésticos).
### VD4012 = Contribuição previdência (1 = contribuinte; 2 = não).
### VD4020 = Rendimento mensal efetivo.
### Resumo: "V2007", "V2009", "V2010", "VD3004", "VD4001", "VD4001", "VD4002", "VD4004A", "VD4005", "VD4010","VD4012", "VD4020"


### Selecionando as variáveis e pegando os dados (raspagem)

variaveis_selecionadas <- c("UF", "Capital", "RM_RIDE","V2007", "V2009", "V2010", "VD3004", "VD4001", "VD4001", "VD4002", "VD4004A", "VD4005", "VD4010","VD4012", "VD4020")

PNADcBR421 <- get_pnadc(year = 2021, quarter = 4, vars = variaveis_selecionadas)


### Renda total Brasil
totalrenda <- svytotal(~VD4020, PNADcBR421, na.rm = T)
totalrenda

### Total por sexo
totalsexo <- svytotal(~V2007, PNADcBR421, na.rm = T)
totalsexo

### Cruzando sexo e raça
totalsexoEraca <- svytotal( ~interaction(V2007, V2010), PNADcBR421,na.rm = T)
totalsexoEraca

### Renda média Brasil
mediarenda <- svymean(~VD4020, PNADcBR421, na.rm = T)
mediarenda

### Proporção sexo
propsexo <- svymean(~V2007, PNADcBR421, na.rm = T)
propsexo

### Proporção raça
propraça <- svymean(~V2010, PNADcBR421, na.rm = T)
propsexo

### Quantis de renda
quantisrenda <- svyquantile(~VD4020, PNADcBR421, quantiles = c(.1,.25,.5,.75,.9, .99), na.rm = T)
quantisrenda

### Trabalhando com a função subset

### Renda média de mulheres no Brasil
mediarendaM <- svymean(~VD4020, subset(PNADcBR421, V2007 == "Mulher")  , na.rm = T)
mediarendaM
### Renda média de homens no Brasil
mediarendaH <- svymean(~VD4020, subset(PNADcBR421, V2007 == "Homem")  , na.rm = T)
mediarendaH
### Taxa de desocupação de mulheres
txdesocupM <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", 
                        subset(PNADcBR421, V2007 == "Mulher") , na.rm = T)
txdesocupM
### Taxa de desocupação de homens
txdesocupH <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", 
                       subset(PNADcBR421, V2007 == "Homem") , na.rm = T)
txdesocupH

### Quantis de renda de homens
quantisrendaH <- svyquantile(~VD4020, subset(PNADcBR421, V2007 == "Homem"), quantiles = c(.1,.25,.5,.75,.9, .99), na.rm = T)
quantisrendaH

### Taxa de desocupação de pessoas com 25 anos ou mais no Brasil

txdesocup25 <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", 
                        subset(PNADcBR421, V2009>=25) , na.rm = T)
txdesocup25

### Taxa de desocupação de jovens entre 20 e 30 anos no Brasil
txdesocup20_30 <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", 
                        subset(PNADcBR421, V2009>=25&V2009<=30) , na.rm = T)
txdesocup20_30

### Taxa de desocupação de mulheres pretas entre 20 e 30 anos no Brasil

txdesocupMP20_30 <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", 
                           subset(PNADcBR421, V2007 == "Mulher" & V2010 == "Preta" & V2009 >= 20 & V2009 <=30 ) , na.rm = T)
txdesocupMP20_30

### Taxa de desocupação de mulheres brancas entre 20 e 30 anos no Brasil
txdesocupMB20_30 <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", 
                             subset(PNADcBR421, V2007 == "Mulher" & V2010 == "Branca" & V2009 >= 20 & V2009 <=30 ) , na.rm = T)
txdesocupMB20_30

### Taxa de desocupação de mulheres brancas acima de 30 anos no Brasil
txdesocupMB20_30b <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", 
                             subset(PNADcBR421, V2007 == "Mulher" & V2010 == "Branca" & V2009 >= 20) , na.rm = T)
txdesocupMB20_30b


### Renda média do homem branco no Brasil
mediarendaHB <- svymean(~VD4020, subset(PNADcBR421, V2007 == "Homem" & V2010 == "Branca")  , na.rm = T)
mediarendaHB

### Renda média do homem preto no Brasil
mediarendaHP <- svymean(~VD4020, subset(PNADcBR421, V2007 == "Homem" & V2010 == "Preta")  , na.rm = T)
mediarendaHP

### Renda média da mulher preta no Brasil
mediarendaMP <- svymean(~VD4020, subset(PNADcBR421, V2007 == "Mulher" & V2010 == "Preta")  , na.rm = T)
mediarendaMP

### A renda média da mulher preta equivale a 46% da renda do homem branco. Um homem branco ganha 2,16 vezes o que ganha a mulher preta.

3511.5/1627.1

### PNADc de PE
PNADcPE <- subset(PNADcBR421, UF=="Pernambuco")

### Renda média da mulher preta em PE

mediarendaMPPE <- svymean(~VD4020, subset(PNADcPE, V2007 == "Mulher" & V2010 == "Preta")  , na.rm = T)
mediarendaMPPE

### PNADc de SP
PNADcSP <- subset(PNADcBR421, UF=="São Paulo")
mediarendaHBSP <- svymean(~VD4020, subset(PNADcSP, V2007 == "Homem" & V2010 == "Branca")  , na.rm = T)
mediarendaHBSP

4039.1/1176.3

#################################################################################################
################### Grupamentos de atividades ###################################################
#################################################################################################

### PNADc da Agricultura ########################################################################
PNADcAGR <- subset(PNADcBR421, VD4010 =="Agricultura, pecuária, produção florestal, pesca e aquicultura")
### Renda média da Agricultura
mediarendaAGR<-svymean(~VD4020, PNADcAGR, na.rm = T)
mediarendaAGR

### PNADc da Indústria ##########################################################################
PNADcIND <- subset(PNADcBR421, VD4010 =="Indústria geral")
### Renda média da Indústria
mediarendaIND<-svymean(~VD4020, PNADcIND, na.rm = T)
mediarendaIND

### PNADc da Construção #########################################################################
PNADcCIV <- subset(PNADcBR421, VD4010 =="Construção")
### Renda média da Construção Civil
mediarendaCIV<-svymean(~VD4020, PNADcCIV, na.rm = T)
mediarendaCIV

### PNADc da Comércio ###########################################################################
PNADcCOM <- subset(PNADcBR421, VD4010 =="Comércio, reparação de veículos automotores e motocicletas")
### Renda média do Comércio
mediarendaCOM<-svymean(~VD4020, PNADcCOM, na.rm = T)
mediarendaCOM

### PNADc do Transporte #########################################################################
PNADcTRA<- subset(PNADcBR421, VD4010 =="05")
### Renda média do Transporte
mediarendaTRA<-svymean(~VD4020, PNADcTRA, na.rm = T)
mediarendaTRA ### Erro!!!

### PNADc do Alojamento #########################################################################
PNADcALO<- subset(PNADcBR421, VD4010 =="Alojamento e alimentação")
### Renda média do Alojamento
mediarendaALO<-svymean(~VD4020, PNADcALO, na.rm = T)
mediarendaALO ### Erro!!!

### PNADc da informação #########################################################################
PNADcSIF <- subset(PNADcBR421, VD4010 =="Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas")
### Renda média da Informação
mediarendaSIF<-svymean(~VD4020, PNADcSIF, na.rm = T)
mediarendaSIF

### PNADc da Administração Pública ##############################################################
PNADcAPU <- subset(PNADcBR421, VD4010 =="Administração pública, defesa e seguridade social ")
### Renda média APU
mediarendaAPU<-svymean(~VD4020, PNADcAPU, na.rm = T)
mediarendaAPU

### PNADc da Educação ###########################################################################
PNADcEDU <- subset(PNADcBR421, VD4010 =="Educação, saúde humana e serviços sociais")
### Renda média da Educação
mediarendaEDU<-svymean(~VD4020, PNADcEDU, na.rm = T)
mediarendaEDU

### PNADc de Outros Serviços ####################################################################
PNADcOUS<-subset(PNADcBR421, VD4010 =="Educação, saúde humana e serviços sociais")
### Renda média de Outros Serviços
mediarendaOUS<-svymean(~VD4020, PNADcOUS, na.rm = T)
mediarendaOUS

### PNADc do Emprego Doméstico ##################################################################
PNADcEDO <- subset(PNADcBR421, VD4010=="Serviços domésticos")
### Renda média do Emprego Doméstico
mediarendaEDO<-svymean(~VD4020, PNADcEDO, na.rm = T)
mediarendaEDO

### Calculando tudo de uma vez
### Raspagem
mediaRendaG<-svyby(formula=~VD4020, by=~VD4010, design=PNADcBR421, FUN=svymean, na.rm=TRUE)
mediaRendaG
### Off line
mediaRendaG1 <- svyby(formula=~VD4020, by=~VD4010, design=dadosPNADc, FUN=svymean, na.rm=TRUE)
mediaRendaG1 ### Valores batem. Melhor raspagem porque já trás os nomes das variáveis.

dfg<-as.data.frame(mediaRendaG)
dfg
Atividade<-dfg[,1]
RM<-dfg[,2]

dfg1<-data.frame(Atividade, RM)
dfg1

write.csv(dfg1, "rendaatividade") ### Salvando em tabela csv.
#################################################################################################
################## Detalhando setores ###########################################################
#################################################################################################



### Detalhando o emprego doméstico ##################################################################
PNADcEDO <- subset(PNADcBR421, VD4010=="Serviços domésticos")
mediarendaEDO<-svymean(~VD4020, PNADcEDO, na.rm = T)
mediarendaEDO
### Renda total EDO
rendatotalEDO <-svytotal(~VD4020, PNADcEDO, na.rm=T)
rendatotalEDO
### Pessoas por sexo EDO
totalsexoEDO <- svytotal(~V2007, PNADcEDO, na.rm = T)
totalsexoEDO
### Pessoas por raça EDO
totalraçaEDO <- svytotal(~V2010, PNADcEDO, na.rm = T)
totalraçaEDO
### Proporção sexo na EDO
propsexoEDO <- svymean(~V2007, PNADcEDO, na.rm = T)
propsexoEDO
### Proporção raça na EDO
propraçaEDO <- svymean(~V2010,  PNADcEDO, na.rm = T)
propraçaEDO
### Quantis de renda EDO
quantisrendaEDO <- svyquantile(~VD4020, PNADcEDO, quantiles = c(.1,.25,.5,.75,.9, .99), na.rm = T)
quantisrendaEDO
### Grau de instrução EDO
instrEDO<-svymean(~VD3004, PNADcEDO, na.rm = T)
instrEDO

### Detalhando a administração pública ##############################################################
PNADcAPU <- subset(PNADcBR421, VD4010 =="Administração pública, defesa e seguridade social ")
### Renda média APU
mediarendaAPU<-svymean(~VD4020, PNADcAPU, na.rm = T)
mediarendaAPU
### Renda total APU
rendatotalAPU <-svytotal(~VD4020, PNADcAPU, na.rm=T)
rendatotalAPU
### Pessoas por sexo  APU
totalsexoAPU <- svytotal(~V2007, PNADcAPU, na.rm = T)
totalsexoAPU
### Pessoas por raça APU
totalraçaAPU <- svytotal(~V2010, PNADcAPU, na.rm = T)
totalraçaAPU
### Proporção sexo na APU
propsexoAPU <- svymean(~V2007, PNADcAPU, na.rm = T)
propsexoAPU
### Proporção raça na APU
propraçaAPU <- svymean(~V2010,  PNADcAPU, na.rm = T)
propraçaAPU
### Quantis de renda APU
quantisrendaAPU <- svyquantile(~VD4020, PNADcAPU, quantiles = c(.1,.25,.5,.75,.9, .99), na.rm = T)
quantisrendaAPU
### Grau de instrução APU
instrAPU<-svymean(~VD3004, PNADcAPU, na.rm = T)
instrAPU

### Regressão logística
modeloLog <- svyglm(VD4010=="11"~V2007+V2010, design=dadosPNADc, family="binomial")
modeloLog


#################################################################################################
#################### Taxa de desocupação por UF #################################################
#################################################################################################


desocupUF <- svyby(~VD4002 == "Pessoas desocupadas",
                   ~UF ,PNADcBR421, svytotal, na.rm = T)
desocupUF
class(desocupUF)
desuf<- as.data.frame(desocupUF) ### Criando um data frame.
desuf ### Notem que a coluna do data frame que nos interessa é a terceira.
class(desuf)
desuf[,3]

pessoasftUF <- svyby(~VD4001 == "Pessoas na força de trabalho",
                     ~UF , PNADcBR421, svytotal, na.rm = T) ### Pessoas na FT por UF.

pftuf<-as.data.frame(pessoasftUF) ### Transformando em data frame.

pftuf ### A coluna que nos interessa é a terceira.
pftuf[,3]

txdesuf=desuf[,3]/pftuf[,3] ### Calculando a taxa de desocupação por UF.
txdesuf
### Vamos agora montar um data frame com os resultados.


UF<- desuf[,1] ### Coluna com os nomes dos estados.
DFTXDESUF<-paste(UF," ",txdesuf) ### Usando a função paste.

DFTXDESUF

txu = data.frame(UF, txdesuf) ### Usando a função data frame (mais legal!).
txu

write.csv(txu, "txdescsunicap") ### Salvando em tabela csv.
