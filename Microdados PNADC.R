getwd()

###### instalando o pacote ############################################

install.packages("PNADcIBGE")
library(PNADcIBGE)

###### lendo arquivo baixado no pc (dados brutos) #####################

dados_pnadc <- read_pnadc("PNADC_032018.txt", "Input_PNADC_trimestral.txt")

###### lendo por web scraping (já como survey) ################

dadosPNADc <- get_pnadc(year = 2018, quarter = 3, vars=c("VD4001","VD4002"))
install.packages("tibble")
dadosPNADc

##### lendo por web scraping (survey, variáveis) ########################

variaveis_selecionadas <- c("UF", "V2007", "V2009", "V2010", "V3007", "VD3001",
                            "VD4001", "VD4002", "VD4019", "VD4020", "VD4035")
dadosPNADc <- get_pnadc(year = 2018, quarter = 3, vars = variaveis_selecionadas)

##### usando o survey ###################################################

library(survey)

###### rendas total e média ############################################

totalrenda <- svytotal(~VD4020, dadosPNADc, na.rm = T)
totalrenda
mediarenda <- svymean(~VD4020, dadosPNADc, na.rm = T)
mediarenda
totalrenda <- svytotal(~VD4019, dadosPNADc, na.rm = T)
totalrenda
mediarenda <- svymean(~VD4019, dadosPNADc, na.rm = T)
mediarenda

###### renda média por UF ##############################################

mediaRendaUF <- svyby(~VD4019, ~UF, dadosPNADc, svymean, na.rm = T)
mediaRendaUF

###### salvando em csv #################################################

write.table(mediaRendaUF, "c:/R/mediaRendaUF.csv", sep="\t")

###### taxa de desocupação #############################################

txdesocup <- svyratio(~VD4002 == "Pessoas desocupadas",
                      ~VD4001 == "Pessoas na força de trabalho", dadosPNADc, na.rm = T)
txdesocup

###### dados por sexo e raça ###########################################

totalsexo <- svytotal(~V2007, dadosPNADc, na.rm = T)
totalsexo
totalsexoraca <- svytotal(~V2007 + V2010, dadosPNADc, na.rm = T)
totalsexoraca
totalsexoEraca <- svytotal(~ interaction(V2007, V2010), dadosPNADc, na.rm = T)
totalsexoEraca

####### salvando tabelas em csv #########################################

sink("totalsexoEraca.csv")
print(totalsexoEraca)
sink()
write.table(totalsexoEraca, "c:/R/totalsexoEraca.csv", sep="\t")

####### usando o convey ################################################
install.packages("convey")
library(convey)
dadosPNADc2 <- convey_prep(dadosPNADc)
giniHab <- svygini(~VD4019, dadosPNADc2, na.rm  =  TRUE)
giniHab
giniUF <- svyby(~VD4020, by = ~UF, dadosPNADc2, svygini, na.rm  =  TRUE)
giniUF
write.table(giniUF, "c:/R/giniUF.csv", sep="\t")

####### proporções de sexo ##############################################

propsexo <- svymean(~V2007, dadosPNADc, na.rm = T)
propsexo

####### proporções de raça ##############################################

propraca <- svymean(~V2010, dadosPNADc, na.rm = T)
propraca

####### proporções de sexo e raça #######################################

propsexoraca <- svymean(~V2007 + V2010, dadosPNADc, na.rm = T)
propsexoraca
propsexoEraca <- svymean(~ interaction(V2007, V2010), dadosPNADc, na.rm = T)
propsexoEraca
write.table(propsexoEraca, "c:/R/propsexoEraca.csv", sep="\t")

####### quantis e medianas ##############################################

medianarenda <- svyquantile(~VD4019, dadosPNADc, quantiles = .5, na.rm = T)
medianarenda
quantisrenda <- svyquantile(~VD4019, dadosPNADc, quantiles = c(.1,.25,.5,.75,.9, .99),
                            na.rm = T)
quantisrenda2 <- svyquantile(~VD4020, dadosPNADc, quantiles = c(.1,.25,.5,.75,.9, .99),
                            na.rm = T)
quantisrenda
quantisrenda2

####### estimativas por domínio ##########################################

####### renda média de homens e mulheres #################################

mediarendaM <- svymean(~VD4019, subset(dadosPNADc, V2007 == "Mulher")  , na.rm = T)
mediarendaH <- svymean(~VD4019, subset(dadosPNADc, V2007 == "Homem")  , na.rm = T)
mediarendaM
mediarendaH
1911.7/2457.6

######## taxa de desemprego por faixa etária #############################

txdesocup25 <- svyratio(~VD4002 == "Pessoas desocupadas",
                        ~VD4001 == "Pessoas na força de trabalho", 
                        subset(dadosPNADc, V2009<=25) , na.rm = T)

txdesocup25
txdesocup30 <- svyratio(~VD4002 == "Pessoas desocupadas",
                        ~VD4001 == "Pessoas na força de trabalho", 
                        subset(dadosPNADc, V2009<=30) , na.rm = T)
txdesocup30

######### múltiplas condições #############################################

nivelinstrHN30 <- svymean(~VD3001, subset(dadosPNADc, 
                                          V2007 == "Homem" & V2010 == "Preta" & 
                                            V2009 > 30), na.rm = T)
nivelinstrHN30
nivelinstrHB30 <- svymean(~VD3001, subset(dadosPNADc, 
                                          V2007 == "Homem" & V2010 == "Branca" & 
                                            V2009 > 30), na.rm = T)
nivelinstrHB30

########## PNADC separadas ################################################

########## PNADC só de mulheres ###########################################

dadosPNADc_mulheres <- subset(dadosPNADc, V2007 == "Mulher")
mediarendaPNADCM <- svymean(~VD4019, dadosPNADc_mulheres, na.rm = T)
mediarendaPNADCM <-subset(dadosPNADc, V2010 == "Preta")

########## PNADC por raça #################################################

dadosPNADC_Pretos<-subset(dadosPNADc, V2010 == "Preta")
dadosPNADc_Brancos <-subset(dadosPNADc, V2010 == "Branca")
dadosPNADc_Pardos <-subset(dadosPNADc, V2010 == "Parda")
dadosPNADc_Amarelos<-subset(dadosPNADc, V2010 == "Amarela")
mediarendaPNADCN <- svymean(~VD4019, dadosPNADc_Pretos, na.rm = T)
mediarendaPNADCB <- svymean(~VD4019, dadosPNADc_Brancos, na.rm = T)
mediarendaPNADCP <- svymean(~VD4019, dadosPNADc_Pardos, na.rm = T)
mediarendaPNADCA <- svymean(~VD4019, dadosPNADc_Amarelos, na.rm = T)
mediarendaPNADCN
mediarendaPNADCB
1606.2/2903.9
mediarendaPNADCP
mediarendaPNADCA
2903.9/4113.4

##### PNADC só de mulheres negras e PNADC só de mulheres brancas ########
dadosPNADc_mulheresN <- subset(dadosPNADc, V2007 == "Mulher"& V2010=="Preta")
dadosPNADc_mulheresB <- subset(dadosPNADc, V2007 == "Mulher"& V2010=="Branca")
mediarendaPNADCMN <- svymean(~VD4019, dadosPNADc_mulheresN, na.rm = T)
mediarendaPNADCMB <- svymean(~VD4019, dadosPNADc_mulheresB, na.rm = T)
mediarendaPNADCMN
mediarendaPNADCMB
1411.7/2421.5

######ESTOU EM Estimação para Vários Domínios###################

freqSexoInstr <- svyby(~V2007, ~VD3001, dadosPNADc, svymean, na.rm = T)
freqSexoInstr

freqInstrSexo <- svyby(~VD3001, ~V2007, dadosPNADc, svymean, na.rm = T)
freqInstrSexo

mediaRendaUF <- svyby(~VD4020, ~UF, dadosPNADc, svymean, na.rm = T)
mediaRendaUF
write.table(mediaRendaUF, "c:/R/mediaRendaUF.csv", sep="\t")
#### resultado: esses números batem exatamente com os divulgados no SIDRA ########

#### calculando a taxa de desocupação por UF #####################################

desocupUF <- svyby(~VD4002 == "Pessoas desocupadas",
                        ~UF , dadosPNADc, svytotal, na.rm = T)

desocupUF

write.table(desocupUF, "c:/R/desocupaUF.csv", sep="\t")

pessoasftUF <- svyby(~VD4001 == "Pessoas na força de trabalho",
                     ~UF , dadosPNADc, svytotal, na.rm = T)

pessoasftUF

write.table(pessoasftUF, "c:/R/pessoasfotUF.csv", sep="\t")


#### resultado: divisão de uma coluna por outra bate exatamente com o que foi publicado ##

