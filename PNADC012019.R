install.packages("PNADcIBGE")
library("PNADcIBGE")
dadosPNADc <- get_pnadc(year = 2019, quarter = 1, vars=c("VD4001","VD4002"))
dadosPNADc

variaveis_selecionadas <- c("UF", "V2007", "V2009",
                            "V2010", "V3007", "VD3004", "VD4001",
                            "VD4002","VD4019", "VD4020", "VD4035")

dadosPNADc <- get_pnadc(year = 2019, quarter = 1, 
                        vars = variaveis_selecionadas)
library(survey)

################ Calculando renda total e média do Brasil ####################

totalrenda <- svytotal(~VD4019, dadosPNADc, na.rm = T)
totalrenda

mediarenda <- svymean(~VD4019, dadosPNADc, na.rm = T)

mediarenda

################ Calculando a taxa de desemprego do Brasil ###################

txdesocup <- svyratio(~VD4002 == "Pessoas desocupadas",
                      ~VD4001 == "Pessoas na força de trabalho", dadosPNADc, na.rm = T)
txdesocup

################ Calculando os totais por sexo ################################

totalsexo <- svytotal(~V2007, dadosPNADc, na.rm = T)

totalsexo

################# Totais por sexo e raça #######################################

totalsexoraca <- svytotal(~V2007 + V2010,
                          dadosPNADc, na.rm = T)
format(totalsexoraca, scientific=F) ###### expressar sem notação científica.

totalsexoraca


################### Cruzando sexo e raça ######################################

totalsexoEraca <- svytotal(~ interaction(V2007,V2010), dadosPNADc, na.rm = T)

totalsexoEraca

write.table(totalsexoEraca,
            "/home/dinilson/totalsexoEraca.csv", sep="\t")

##################### Calculando o índice de Gini do Brasil ##################
install.packages("convey")
library (convey)
dadosPNADc2 <- convey_prep(dadosPNADc)

giniHab <- svygini(~VD4019, dadosPNADc2, na.rm  =  TRUE)
giniHab <- svygini(~VD4020, dadosPNADc2, na.rm  =  TRUE)

giniHab

giniUF <- svyby(~VD4020, by = ~UF,dadosPNADc2, svygini, na.rm  =  TRUE)

###################### Calculando o índice de Gini por UF #####################

giniUF <- svyby(~VD4020, by = ~UF,
                dadosPNADc2, svygini, na.rm  =  TRUE)

write.table(giniUF,
            "/home/dinilson/giniUF.csv", sep="\t")


###################### Renda média por UF #####################################

mediarendaUF <- svyby(~VD4020, by = ~UF, dadosPNADc2, svymean, na.rm = T)
mediarendaUF
write.table(mediarendaUF,
            "/home/dinilson/mediarendaUF.csv", sep="\t")

###################### Renda média por raça ###################################

mediarendaraça <- svyby(~VD4020, by = ~V2010, dadosPNADc, svymean, na.rm = T)
mediarendaraça

###################### Desemprego por raça e sexo ###############################

txdesocupSexoRaca <- svyby(~VD4002 == "Pessoas desocupadas",
                           ~interaction(V2007,V2010), 
                           dadosPNADc, svyratio, 
                           denominator = ~VD4001 == "Pessoas na força de trabalho", 
                           na.rm = T, vartype = "cv")

txdesocupSexoRaca
write.table(txdesocupSexoRaca,
            "/home/dinilson/txdesocupSexoRaca.csv", sep="\t")

####################### Sexo por grau de instrução (%) ##########################

freqSexoInstr <- svyby(~V2007, ~VD3004, dadosPNADc, svymean, na.rm = T)
freqSexoInstr
write.table(freqSexoInstr,
            "/home/dinilson/freqSexoInstr.csv", sep="\t")

####################### Grau de instrução por sexo (%) ##########################

freqInstrSexo <- svyby(~VD3004, ~V2007, dadosPNADc, svymean, na.rm = T)
write.table(freqInstrSexo,
            "/home/dinilson/freqInstrSexo.csv", sep="\t")

####################### Renda média por sexo x raça (R$) ########################

mediarendasexoEraça <- svyby(~VD4020, ~ interaction(V2007,V2010), dadosPNADc, svymean, 
                        na.rm = T)
mediarendasexoEraça
write.table(mediarendasexoEraça,
            "/home/dinilson/mediarendasexoEraça.csv", sep="\t")

######### Nível de instrução de mulheres com 30 ou mais de idade ##########

nivelinstrMPr30 <- svymean(~VD3004, subset(dadosPNADc2, 
                                          
                                          V2007 == "Mulher" & 
                                            V2010 == "Preta" & V2009 > 30), na.rm = T)
nivelinstrMPr30
write.table(nivelinstrMPr30,
            "/home/dinilson/nivelinstrMPr30.csv", sep="\t")

nivelinstrMB30 <- svymean(~VD3004, subset(dadosPNADc2,
                                           
                                           V2007 == "Mulher" & 
                                             V2010 == "Branca" & V2009 > 30), na.rm = T)
write.table(nivelinstrMB30,
            "/home/dinilson/nivelinstrMB30.csv", sep="\t")


######################## Quantis de renda ############################################

quantisrenda <- svyquantile(~VD4020, dadosPNADc, 
                            quantiles = c(.1,.25,.5,.75,.9), na.rm = T)
quantisrenda
