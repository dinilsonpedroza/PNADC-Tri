getwd() ### Vendo qual o nosso diretório de trabalho.



### Vamos calcular a taxa de desocupação do Brasil  e de Pernambuco.

### Carregando os pacotes necessários

library(PNADcIBGE)
library(convey)
library(survey)


### Escolhendo as variáveis que serão trabalhadas

#### Variáveis: UF (ex. Pernambuco = 26); Capital (ex. Recife = 26); RM_RIDE (ex. RMR = 26)
#### VD4001(condição na força de trabalho); VD4002(condição de ocupação); VD4004A(subocupação habitual)

### Coletando os dados por raspagem, com a função get_pnadc do pacote PNADcIBGE.

variaveis_selecionadas <- c("UF", "Capital", "RM_RIDE", "VD4001", "VD4002", "VD4004A")

PNADcBR <- get_pnadc(year = 2021, quarter = 3, vars = variaveis_selecionadas)
class(PNADcBR)
### Calculando a taxa de desemprego do Brasil.

txdesocup <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", PNADcBR, na.rm = T)
txdesocup

### Taxa de desemprego no Basil é 12,64% [confirmado].


### Taxa de desemprego de PE.


PNADcPE <- subset(PNADcBR, UF=="Pernambuco") ### Montando a PNADc de PE.

txdesocupPE <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", PNADcPE, na.rm = T)
txdesocupPE

### Taxa de desemprego em Pernambuco é 19,28% [confirmado].


### Taxa de desemprego na Bahia.

PNADcBA <- subset(PNADcBR, UF=="Bahia") ### Montando a PNADc da Bahia.

txdesocupBA <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", PNADcBA, na.rm = T)
txdesocupBA

### Taxa de desemprego na Bahia é 18,73%.


### Taxa de desemprego da RMR.

PNADcRMR <- subset(PNADcPE, RM_RIDE=="Região Metropolitana de Recife (PE)")
txdesocupRMR <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", PNADcRMR, na.rm = T)
txdesocupRMR

### Taxa de desemprego na RMR é de 20,21% [não confirmado].

#################################################################################################
#################### Taxa de desocupação por UF #################################################
#################################################################################################



desocupUF <- svyby(~VD4002 == "Pessoas desocupadas",
                   ~UF , PNADcBR, svytotal, na.rm = T) ### Desocupados por UF.


desuf<- as.data.frame(desocupUF) ### Criando um data frame.
class(desuf)
desuf ### Notem que a coluna do data frame que nos interessa é a terceira.

desuf[,3]

pessoasftUF <- svyby(~VD4001 == "Pessoas na força de trabalho",
                     ~UF , PNADcBR, svytotal, na.rm = T) ### Pessoas na FT por UF.

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





################### Sem design ###################

PNADcBRS <- get_pnadc(year = 2021, quarter = 3, vars = variaveis_selecionadas, design = F)
class(PNADcBRS)
