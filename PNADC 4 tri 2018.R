getwd()
############# baixando toda pnadc na �rea de trabalho ###################

dados_pnadc <- read_pnadc("PNADC_042018.txt", "Input_PNADC_trimestral.txt")

############# atribuindo r�tulos as vari�veis brutas ####################

dados_pnadc <- pnadc_labeller(dados_pnadc, 
                              "dicionario_das_variaveis_PNAD_Continua_microdados.xls")

############## criando o objeto do plano amostral complexo ##############

dados_Pnadc <- pnadc_design(dados_pnadc)

############## produzindo dados com esse objeto #########################

totalrenda <- svytotal(~VD4020, dados_Pnadc, na.rm = T)
totalrenda

totalsexo <- svytotal(~V2007, dados_Pnadc, na.rm = T)
totalsexo

totalsexoraca <- svytotal(~V2007 + V2010, dados_Pnadc, na.rm = T)
totalsexoraca

############### cruzando sexo e ra�a #####################################

totalsexoEraca <- svytotal(~ interaction(V2007, V2010), dados_Pnadc, na.rm = T)
totalsexoEraca

############### m�dias e propor��es ######################################

mediarenda <- svymean(~VD4020, dados_Pnadc, na.rm = T)
mediarenda

propsexo <- svymean(~V2007, dados_Pnadc, na.rm = T)
propsexo

propsexoraca <- svymean(~V2007 + V2010, dados_Pnadc, na.rm = T)
propsexoraca

################ estimando raz�es #########################################
################ taxa de desocupa��o ######################################

txdesocup <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na for�a de trabalho", 
                      dados_Pnadc, na.rm = T)

txdesocup

################# medianas e quantis #######################################

medianarenda <- svyquantile(~VD4020, dados_Pnadc, quantiles = .5, na.rm = T)
medianarenda

quantisrenda <- svyquantile(~VD4020, dados_Pnadc, quantiles = c(.1,.25,.5,.75,.9), na.rm = T)
quantisrenda

################## usando a fun��o subset ###################################

mediarendaM <- svymean(~VD4020, subset(dados_Pnadc, V2007 == "Mulher")  , na.rm = T)
mediarendaM

mediarendaH <- svymean(~VD4020, subset(dados_Pnadc, V2007 == "Homem")  , na.rm = T)
mediarendaH

mediarendaHpr<- svymean(~VD4017, subset(dados_Pnadc, V2007 == "Homem")  , na.rm = T)
mediarendaHpr
mediarendaHprhb<- svymean(~VD4016, subset(dados_Pnadc, V2007 == "Homem")  , na.rm = T)
mediarendaHprhb

txdesocup25 <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na for�a de trabalho", 
                        subset(dados_Pnadc, V2009>=25) , na.rm = T)
txdesocup25

nivelinstrHP30 <- svymean(~VD3001, subset(dados_Pnadc, 
                                          V2007 == "Homem" & V2010 == "Parda" & V2009 > 30), 
                          na.rm = T)
nivelinstrHP30

nivelinstrMP30 <- svymean(~VD3001, subset(dados_Pnadc, 
                                          V2007 == "Mulher" & V2010 == "Parda" & V2009 > 30), 
                          na.rm = T)
nivelinstrMP30

################### PNADC das mulheres ###############################################

dadosPNADc_mulheres <- subset(dados_Pnadc, V2007 == "Mulher")
mediarendamulheres<-svymean(~VD4020, dadosPNADc_mulheres, na.rm = T)
mediarendamulheres
################### v�rios dom�nios simult�neamente ##################################

freqSexoInstr <- svyby(~V2007, ~VD3001, dados_Pnadc, svymean, na.rm = T)
freqSexoInstr

freqInstrSexo <- svyby(~VD3001, ~V2007, dados_Pnadc, svymean, na.rm = T)
freqInstrSexo

#################### renda m�dia por UF ##############################################

mediaRendaUF <- svyby(~VD4020, ~UF, dados_Pnadc, svymean, na.rm = T)
mediaRendaUF


txdesocupSexoRaca <- svyby(~VD4002 == "Pessoas desocupadas", ~interaction(V2007,V2010), 
                           dados_Pnadc, svyratio, 
                           denominator = ~VD4001 == "Pessoas na for�a de trabalho",
                           na.rm = T, vartype = "cv")
txdesocupSexoRaca

write.table(txdesocupSexoRaca, "c:/R/txdesocupSexoRaca.csv", sep="\t")

#################### tentando para PE ################################################

dados_Pnadc_PE <- subset(dados_Pnadc, UF == "Pernambuco")
mediarendaPE<-svymean(~VD4020, dados_Pnadc_PE, na.rm = T)
mediarendaPE
dados_Pnadc_PE
totalrendaPE <- svytotal(~VD4020, dados_Pnadc_PE, na.rm = T)
totalrendaPE
txdesocupSexoRacaPE <- svyby(~VD4002 == "Pessoas desocupadas", ~interaction(V2007,V2010), 
                             dados_Pnadc_PE, svyratio, 
                           denominator = ~VD4001 == "Pessoas na for�a de trabalho",
                           na.rm = T, vartype = "cv")
txdesocupSexoRacaPE

write.table(txdesocupSexoRacaPE, "c:/R/txdesocupSexoRacaPE.csv", sep="\t")


dados_Pnadc_Recife <- subset(dados_Pnadc, Capital == "Munic�pio de Recife (PE)")
mediarendaRec<-svymean(~VD4020, dados_Pnadc_Recife, na.rm = T)
mediarendaRec
txdesocupSexoRacaRec <- svyby(~VD4002 == "Pessoas desocupadas", ~interaction(V2007,V2010), 
                              dados_Pnadc_Recife, svyratio, 
                             denominator = ~VD4001 == "Pessoas na for�a de trabalho",
                             na.rm = T, vartype = "cv")
write.table(txdesocupSexoRacaRec, "c:/R/txdesocupSexoRacaRec.csv", sep="\t")
txdesocupSexoRacaRec

############ PNADs por UF da Regi�o ########################################
dados_Pnadc_PE <- subset(dados_Pnadc, UF == "Pernambuco")
dados_Pnadc_BA <- subset(dados_Pnadc, UF == "Bahia")
dados_Pnadc_CE <- subset(dados_Pnadc, UF == "Cear�")
dados_Pnadc_RN <- subset(dados_Pnadc, UF == "Rio Grande do Norte")
dados_Pnadc_SE <- subset(dados_Pnadc, UF == "Sergipe")
dados_Pnadc_PB <- subset(dados_Pnadc, UF == "Para�ba")
dados_Pnadc_PI <- subset(dados_Pnadc, UF == "Piau�")
dados_Pnadc_MA <- subset(dados_Pnadc, UF == "Maranh�o")
dados_Pnadc_AL <- subset(dados_Pnadc, UF == "Alagoas")
dados_Pnadc_AL
############ conferindo #####################################################

mediarendaPE<-svymean(~VD4020, dados_Pnadc_PE, na.rm = T)
mediarendaPE
mediarendaAL<-svymean(~VD4020, dados_Pnadc_AL, na.rm = T)
mediarendaAL

############ estat�sticas ###################################################
############ renda m�dia h x m ##############################################

mediarendaM <- svymean(~VD4020, subset(dados_Pnadc, V2007 == "Mulher")  , na.rm = T)
mediarendaM

mediarendaH <- svymean(~VD4020, subset(dados_Pnadc, V2007 == "Homem")  , na.rm = T)
mediarendaH

mediarendaMPE <- svymean(~VD4020, subset(dados_Pnadc_PE, V2007 == "Mulher")  , na.rm = T)
mediarendaMPE

mediarendaHPE <- svymean(~VD4020, subset(dados_Pnadc_PE, V2007 == "Homem")  , na.rm = T)
mediarendaHPE

mediarendaMBA <- svymean(~VD4020, subset(dados_Pnadc_BA, V2007 == "Mulher")  , na.rm = T)
mediarendaMBA

mediarendaHBA <- svymean(~VD4020, subset(dados_Pnadc_BA, V2007 == "Homem")  , na.rm = T)
mediarendaHBA

mediarendaMCE <- svymean(~VD4020, subset(dados_Pnadc_CE, V2007 == "Mulher")  , na.rm = T)
mediarendaMCE

mediarendaHCE <- svymean(~VD4020, subset(dados_Pnadc_CE, V2007 == "Homem")  , na.rm = T)
mediarendaHCE

mediarendaMRN <- svymean(~VD4020, subset(dados_Pnadc_RN, V2007 == "Mulher")  , na.rm = T)
mediarendaMRN

mediarendaHRN <- svymean(~VD4020, subset(dados_Pnadc_RN, V2007 == "Homem")  , na.rm = T)
mediarendaHRN

mediarendaMSE <- svymean(~VD4020, subset(dados_Pnadc_SE, V2007 == "Mulher")  , na.rm = T)
mediarendaMSE

mediarendaHSE <- svymean(~VD4020, subset(dados_Pnadc_SE, V2007 == "Homem")  , na.rm = T)
mediarendaHSE

mediarendaMPB <- svymean(~VD4020, subset(dados_Pnadc_PB, V2007 == "Mulher")  , na.rm = T)
mediarendaMPB

mediarendaHPB <- svymean(~VD4020, subset(dados_Pnadc_PB, V2007 == "Homem")  , na.rm = T)
mediarendaHPB

mediarendaMPI <- svymean(~VD4020, subset(dados_Pnadc_PI, V2007 == "Mulher")  , na.rm = T)
mediarendaMPI

mediarendaHPI <- svymean(~VD4020, subset(dados_Pnadc_PI, V2007 == "Homem")  , na.rm = T)

mediarendaMMA <- svymean(~VD4020, subset(dados_Pnadc_MA, V2007 == "Mulher")  , na.rm = T)
mediarendaMMA

mediarendaHMA <- svymean(~VD4020, subset(dados_Pnadc_MA, V2007 == "Homem")  , na.rm = T)
mediarendaHMA

mediarendaMAL <- svymean(~VD4020, subset(dados_Pnadc_AL, V2007 == "Mulher")  , na.rm = T)
mediarendaMAL

mediarendaHAL <- svymean(~VD4020, subset(dados_Pnadc_AL, V2007 == "Homem")  , na.rm = T)
mediarendaHAL

############ taxa de desocuapa��o, sexo, ra�a, por UF ###########################################

txdesocupSexoRaca <- svyby(~VD4002 == "Pessoas desocupadas", ~interaction(V2007,V2010), 
                           dados_Pnadc, svyratio, 
                           denominator = ~VD4001 == "Pessoas na for�a de trabalho",
                           na.rm = T)


write.table(txdesocupSexoRaca, "c:/R/txdesocupSexoRaca.csv", sep="\t")

txdesocupSexoRacaPE <- svyby(~VD4002 == "Pessoas desocupadas", ~interaction(V2007,V2010), 
                             dados_Pnadc_PE, svyratio, 
                             denominator = ~VD4001 == "Pessoas na for�a de trabalho",
                             na.rm = T)
txdesocupSexoRacaBA <- svyby(~VD4002 == "Pessoas desocupadas", ~interaction(V2007,V2010), 
                             dados_Pnadc_BA, svyratio, 
                             denominator = ~VD4001 == "Pessoas na for�a de trabalho",
                             na.rm = T)
txdesocupSexoRacaCE <- svyby(~VD4002 == "Pessoas desocupadas", ~interaction(V2007,V2010), 
                             dados_Pnadc_CE, svyratio, 
                             denominator = ~VD4001 == "Pessoas na for�a de trabalho",
                             na.rm = T)
txdesocupSexoRacaRN <- svyby(~VD4002 == "Pessoas desocupadas", ~interaction(V2007,V2010), 
                             dados_Pnadc_RN, svyratio, 
                             denominator = ~VD4001 == "Pessoas na for�a de trabalho",
                             na.rm = T)
txdesocupSexoRacaSE <- svyby(~VD4002 == "Pessoas desocupadas", ~interaction(V2007,V2010), 
                             dados_Pnadc_SE, svyratio, 
                             denominator = ~VD4001 == "Pessoas na for�a de trabalho",
                             na.rm = T)
txdesocupSexoRacaPB <- svyby(~VD4002 == "Pessoas desocupadas", ~interaction(V2007,V2010), 
                             dados_Pnadc_PB, svyratio, 
                             denominator = ~VD4001 == "Pessoas na for�a de trabalho",
                             na.rm = T)
txdesocupSexoRacaPI <- svyby(~VD4002 == "Pessoas desocupadas", ~interaction(V2007,V2010), 
                             dados_Pnadc_PI, svyratio, 
                             denominator = ~VD4001 == "Pessoas na for�a de trabalho",
                             na.rm = T)
txdesocupSexoRacaMA <- svyby(~VD4002 == "Pessoas desocupadas", ~interaction(V2007,V2010), 
                             dados_Pnadc_MA, svyratio, 
                             denominator = ~VD4001 == "Pessoas na for�a de trabalho",
                             na.rm = T)
txdesocupSexoRacaAL <- svyby(~VD4002 == "Pessoas desocupadas", ~interaction(V2007,V2010), 
                             dados_Pnadc_AL, svyratio, 
                             denominator = ~VD4001 == "Pessoas na for�a de trabalho",
                             na.rm = T)

write.table(txdesocupSexoRacaPE, "c:/R/txdesocupSexoRacaPE.csv", sep="\t")
write.table(txdesocupSexoRacaBA, "c:/R/txdesocupSexoRacaBA.csv", sep="\t")
write.table(txdesocupSexoRacaCE, "c:/R/txdesocupSexoRacaCE.csv", sep="\t")
write.table(txdesocupSexoRacaRN, "c:/R/txdesocupSexoRacaRN.csv", sep="\t")
write.table(txdesocupSexoRacaSE, "c:/R/txdesocupSexoRacaSE.csv", sep="\t")
write.table(txdesocupSexoRacaPB, "c:/R/txdesocupSexoRacaPB.csv", sep="\t")
write.table(txdesocupSexoRacaPI, "c:/R/txdesocupSexoRacaPI.csv", sep="\t")
write.table(txdesocupSexoRacaMA, "c:/R/txdesocupSexoRacaMA.csv", sep="\t")
write.table(txdesocupSexoRacaAL, "c:/R/txdesocupSexoRacaAL.csv", sep="\t")

################ instru��o por sexo e ra�a, por UF #######################

freqSexoInstr <- svyby(~VD3001, ~V2007, dados_Pnadc, svymean, na.rm = T)
freqSexoInstrPE <- svyby(~VD3001, ~V2007, dados_Pnadc_PE, svymean, na.rm = T)
freqSexoInstrBA <- svyby(~VD3001, ~V2007, dados_Pnadc_BA, svymean, na.rm = T)
freqSexoInstrCE <- svyby(~VD3001, ~V2007, dados_Pnadc_CE, svymean, na.rm = T)
freqSexoInstrRN <- svyby(~VD3001, ~V2007, dados_Pnadc_RN, svymean, na.rm = T)
freqSexoInstrSE <- svyby(~VD3001, ~V2007, dados_Pnadc_SE, svymean, na.rm = T)
freqSexoInstrPB <- svyby(~VD3001, ~V2007, dados_Pnadc_PB, svymean, na.rm = T)
freqSexoInstrPI <- svyby(~VD3001, ~V2007, dados_Pnadc_PI, svymean, na.rm = T)
freqSexoInstrMA <- svyby(~VD3001, ~V2007, dados_Pnadc_MA, svymean, na.rm = T)
freqSexoInstrAL <- svyby(~VD3001, ~V2007, dados_Pnadc_AL, svymean, na.rm = T)


write.table(freqSexoInstr, "c:/R/freqSexoInstr.csv", sep="\t")
write.table(freqSexoInstrPE, "c:/R/freqSexoInstrPE.csv", sep="\t")
write.table(freqSexoInstrBA, "c:/R/freqSexoInstrBA.csv", sep="\t")
write.table(freqSexoInstrCE, "c:/R/freqSexoInstrCE.csv", sep="\t")
write.table(freqSexoInstrRN, "c:/R/freqSexoInstrRN.csv", sep="\t")
write.table(freqSexoInstrSE, "c:/R/freqSexoInstrSE.csv", sep="\t")
write.table(freqSexoInstrPB, "c:/R/freqSexoInstrPB.csv", sep="\t")
write.table(freqSexoInstrPI, "c:/R/freqSexoInstrPI.csv", sep="\t")
write.table(freqSexoInstrMA, "c:/R/freqSexoInstrMA.csv", sep="\t")
write.table(freqSexoInstrAL, "c:/R/freqSexoInstrAL.csv", sep="\t")





