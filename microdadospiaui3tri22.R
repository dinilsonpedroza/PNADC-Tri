###############################################################
########## Perfil do idoso no mercado de trabalho no Piauí ####
###############################################################
########## Dados do 3º trimestre de 2022 ######################
###############################################################

### Carregando pacotes

library(PNADcIBGE)
library(convey)
library(survey)
library (readr)
library(dplyr)
### Selecionando as variáveis
#### UF (ex. Piauí = 22)
### V2007 = Sexo (1 = Homem, 2 = Mulher)
### V2009 = Idade do morador.
### V2010 = Cor ou raça.
### VD3004 = Nível de instrução.
### VD4001 = Condição em relação à força de trabalho (1 = na força, 2 = fora).
### VD4002 = Condição de ocupação (1 = ocupada, 2 = desocupada)
### VD4010 = Grupamentos de atividade (ex.: 11 = Serviços domésticos).
### VD4012 = Contribuição previdência (1 = contribuinte; 2 = não).
### VD4020 = Rendimento mensal efetivo.
#### VD4001(condição na força de trabalho)
#### SE = erro padrão

variaveis_selecionadas <- c("UF", "VD3004", "VD4001",  "VD4002", "VD4010",
                            "VD4012", "VD4020", "V2007", "V2009", "V2010")

### Coletando os dados por raspagem, com a função get_pnadc do pacote PNADcIBGE.

PNADcBR <- get_pnadc(year = 2022, quarter = 3, vars = variaveis_selecionadas)

########################### PNADc do Piauí #####################################
PNADcPI <- subset(PNADcBR, UF=="Piauí") ### Montando a PNADc do PI.

### Taxa de desemprego no Piauí
txdpi <- svyratio(~VD4002 == "Pessoas desocupadas",
                  ~VD4001 == "Pessoas na força de trabalho", PNADcPI, na.rm = T)
txdpi
# taxa de desocupação de 9,24% [bate com o que foi divulgado]

### Rendimento mensal efetivo no PI (VD 4020)
rmefetivopi <- svymean(~VD4020, PNADcPI, na.rm = T)
rmefetivopi

### Nº de pessoas ocupadas #####################################################
ocupi <- svytotal(~VD4002 == "Pessoas ocupadas", PNADcPI, na.rm = T)
ocupi
cv(ocupi) # Erro padrão
confint(ocupi) # Intervalo de confiança da estimativa de 95% .
confint(ocupi, level= .99)
# número de pessoas ocupadas no Piauí = 1.305.985 [bate com o que foi divulgado]

### Nº de pessoas ocupadas com 60 ou mais anos de idade ########################
ocupi_60 <- svytotal(~VD4002 == "Pessoas ocupadas", subset(PNADcPI, V2009>=60), na.rm = T)
ocupi_60
cv(ocupi_60)
confint(ocupi_60)
confint(ocupi_60, level= .99)
# nº de pessoas com 60 anos ou mais ocupadas no PI = 87.442 [bate com o que foi divulgado]

### Nº de mulheres pretas com 60 ou + ocupadas no PI ###########################
ocupi_60MP <- svytotal(~VD4002 == "Pessoas ocupadas",  
                             subset(PNADcPI, V2007 == "Mulher" & V2010 == "Preta" 
                                    & V2009 >= 60 ), na.rm = T)
ocupi_60MP
cv(ocupi_60MP)
confint(ocupi_60MP)
confint(ocupi_60MP, level= .99)

### Nº de mulheres brancas com 60 ou + ocupadas no PI ##########################
ocupi_60MB <- svytotal(~VD4002 == "Pessoas ocupadas",  
                        subset(PNADcPI, V2007 == "Mulher" & V2010 == "Branca" 
                               & V2009 >= 60 ), na.rm = T)
ocupi_60MB
cv(ocupi_60MB)
confint(ocupi_60MB)
confint(ocupi_60MB, level= .99)

### Nº de homens pretos com 60 ou + ocupados no PI #############################
ocupi_60HP <- svytotal(~VD4002 == "Pessoas ocupadas",  
                        subset(PNADcPI, V2007 == "Homem" & V2010 == "Preta" 
                               & V2009 >= 60 ), na.rm = T)
ocupi_60HP
cv(ocupi_60HP)
confint(ocupi_60HP)
confint(ocupi_60HP, level= .99)

### Nº de homens brancos com 60 ou + ocupados no PI ############################
ocupi_60HB <- svytotal(~VD4002 == "Pessoas ocupadas",  
                        subset(PNADcPI, V2007 == "Homem" & V2010 == "Branca" 
                               & V2009 >= 60 ), na.rm = T)
ocupi_60HB
cv(ocupi_60HB)
confint(ocupi_60HB)
confint(ocupi_60HB, level= .99)

### Renda média para os grupos acima ###########################################
### Método alternativo para a elaboração dos dados.

################################################################################
### "PNAD de ocupados no PI"####################################################
################################################################################
PNADcPI_ocu <- subset(PNADcPI, VD4002 == "Pessoas ocupadas")
ocupi_60b <- svytotal(~V2009>= 60, PNADcPI_ocu, na.rm = T)
ocupi_60b # o nº bate com o divulgado e com o método anterior.
cv(ocupi_60b)
confint(ocupi_60b)
confint(ocupi_60b, level= .99)

### Renda média das pessoas ocupadas no PI.
rmocupi <- svymean(~VD4020, PNADcPI_ocu, na.rm = T)
rmocupi
cv(rmocupi)
confint(rmocupi)
confint(rmocupi, level= .99)

### Renda média de homens ocupados no PI
rmocupi_H <- svymean(~VD4020, subset(PNADcPI_ocu, V2007 == "Homem"), na.rm = T)
rmocupi_H
cv(rmocupi_H)
confint(rmocupi_H)
confint(rmocupi_H, level= .99)

### Renda média de mulheres ocupadas no PI
rmocupi_M <- svymean(~VD4020, subset(PNADcPI_ocu, V2007 == "Mulher"), na.rm = T)
rmocupi_M
cv(rmocupi_M)
confint(rmocupi_M)
confint(rmocupi_M, level= .99)

### Renda média de pessoas ocupadas com 60+ no PI
rmocupi_60 <- svymean(~VD4020, subset(PNADcPI_ocu, V2009 >= 60), na.rm = T)
rmocupi_60
cv(rmocupi_60)
confint(rmocupi_60)
confint(rmocupi_60, level= .99)

### Renda média de homens ocupados com 60+ no PI
rmocupi_60H <- svymean(~VD4020, subset(PNADcPI_ocu, V2007 == "Homem" & V2009 >= 60), na.rm = T)
rmocupi_60H
cv(rmocupi_60H)
confint(rmocupi_60H)
confint(rmocupi_60H, level= .99)

### Renda média de mulheres ocupadas com 60+ no PI
rmocupi_60M <- svymean(~VD4020, subset(PNADcPI_ocu, V2007 == "Mulher" & V2009 >= 60), na.rm = T)
rmocupi_60M
cv(rmocupi_60M)
confint(rmocupi_60M)
confint(rmocupi_60M, level= .99)

### Renda média de homens pretos ocupados com 60+ no PI
rmocupi_60HP <- svymean(~VD4020, subset(PNADcPI_ocu, V2007 == "Homem" & V2009 >= 60
                                        & V2010 == "Preta"), na.rm = T)
rmocupi_60HP
cv(rmocupi_60HP)
confint(rmocupi_60HP)
confint(rmocupi_60HP, level= .99)

### Renda média de homens brancos ocupados com 60+ no PI
rmocupi_60HB <- svymean(~VD4020, subset(PNADcPI_ocu, V2007 == "Homem" & V2009 >= 60
                                        & V2010 == "Branca"), na.rm = T)
rmocupi_60HB
cv(rmocupi_60HB)
confint(rmocupi_60HB)
confint(rmocupi_60HB, level= .99)

### Renda média de mulheres pretas ocupadas com 60+ no PI
rmocupi_60MP <- svymean(~VD4020, subset(PNADcPI_ocu, V2007 == "Mulher" & V2009 >= 60
                                        & V2010 == "Preta"), na.rm = T)
rmocupi_60MP
cv(rmocupi_60MP)
confint(rmocupi_60MP)
confint(rmocupi_60MP, level= .99)

### Renda média de mulheres brancas ocupadas com 60+ no PI
rmocupi_60MB <- svymean(~VD4020, subset(PNADcPI_ocu, V2007 == "Mulher" & V2009 >= 60
                                        & V2010 == "Branca"), na.rm = T)
rmocupi_60MB
cv(rmocupi_60MP)
confint(rmocupi_60MP)
confint(rmocupi_60MP, level= .99)

### Sexo e Escolaridade ###############################################################

### Criando uma "PNADc das pessoas ocupadas com mais de 60 anos no PI" #########
PNADcPI_ocu60<- subset(PNADcPI, VD4002 == "Pessoas ocupadas" & V2009 >= 60)

### População ocupados PI com 60+ (sexo e raça)
totalsr_ocu60 <- svytotal(~V2007 + V2010, PNADcPI_ocu60, na.rm = T)
totalsr_ocu60 # H + M bate com o que foi divulgado.

### Os mesmos dados acima (sexo e raça), só que cruzados
totalsr_ocu60crz <- svytotal(~ interaction(V2007, V2010), PNADcPI_ocu60, na.rm = T)
totalsr_ocu60crz # Totais batem com o que foi divulgado.


###

### População ocupados PI com 60+ (sexo e escolaridade)
totalse_ocu60crz <- svytotal(~ interaction(V2007, VD3004), PNADcPI_ocu60, na.rm = T)
totalse_ocu60crz
df<-as.data.frame(totalse_ocu60crz)
df %>% 
  mutate_if(is.numeric, round, digits=2)
write.table(df, file = "P60ocuedusexPI3tri22.csv", sep = "\t", na = "", quote = TRUE)

###


### Os mesmos dados acima (sexo e escolaridade), só que cruzados
totalse_ocu60crz <- svytotal(~ interaction(V2007, VD3004), PNADcPI_ocu60, na.rm = T)
totalse_ocu60crz

### Renda média (Atenção para a mudança na nomeclatura das variáveis criadas)
mediarenda <- svymean(~VD4020, PNADcPI_ocu60, na.rm = T)
mediarenda #

### Proporção de sexos
propsexo <- svymean(~V2007, PNADcPI_ocu60, na.rm = T)
propsexo

### Proporção sexo e raça
propsexoraca <- svymean(~V2007 + V2010, PNADcPI_ocu60, na.rm = T)
propsexoraca

### Cruzando os dados acima (sexo e raça)
propsexoEraca <- svymean(~ interaction(V2007, V2010), PNADcPI_ocu60, na.rm = T)
propsexoEraca

### Por nível de instrução por sexo
freqSexoInstr <- svyby(~V2007, ~VD3004, PNADcPI_ocu60, svymean, na.rm = T)
freqSexoInstr

### Renda média por raça
mediaRendar <- svyby(~VD4020, ~V2010, PNADcPI_ocu60, svymean, na.rm = T)
mediaRendar
cv(mediaRendar)
### Renda média por sexo x raça
mediaRendars <- svyby(~VD4020, ~interaction(V2007, V2010), PNADcPI_ocu60, svymean, na.rm = T)
mediaRendars # Bate com os resultados que eu havia calculado antes.
cv(mediaRendars)

### Renda média por sexo e instrução
mediaRendase<- svyby(~VD4020, ~interaction(V2007, VD3004), PNADcPI_ocu60, svymean, na.rm = T)
mediaRendase
df1<-as.data.frame(mediaRendase)


df1 %>% 
  mutate_if(is.numeric, round, digits=2)

write.table(df1, file = "edusexPI3tri22.csv", sep = "\t", na = "", quote = TRUE)
