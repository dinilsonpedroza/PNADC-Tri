###############################################################
########## perfil do idoso no mercado de trabalho no Piauí ####
###############################################################
### Carregando pacotes

library(PNADcIBGE)
library(convey)
library(survey)

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

### Nº de pessoas ocupadas #####################################################
ocupi <- svytotal(~VD4002 == "Pessoas ocupadas", PNADcPI, na.rm = T)
ocupi
# número de pessoas ocupadas no Piauí = 1.305.985 [bate com o que foi divulgado]

### Nº de pessoas ocupadas com 60 ou mais anos de idade ########################
ocupi_60 <- svytotal(~VD4002 == "Pessoas ocupadas", subset(PNADcPI, V2009>=60), na.rm = T)
ocupi_60
# nº de pessoas com 60 anos ou mais ocupadas no PI = 97.441 [bate com o que foi divulgado]

### Nº de mulheres pretas com 60 ou + ocupadas no PI ###########################
ocu_pi_60MP <- svytotal(~VD4002 == "Pessoas ocupadas",  
                             subset(PNADcPI, V2007 == "Mulher" & V2010 == "Preta" 
                                    & V2009 >= 60 ), na.rm = T)

ocu_pi_60MP

### Nº de mulheres brancas com 60 ou + ocupadas no PI ##########################

ocu_pi_60MB <- svytotal(~VD4002 == "Pessoas ocupadas",  
                        subset(PNADcPI, V2007 == "Mulher" & V2010 == "Branca" 
                               & V2009 >= 60 ), na.rm = T)

ocu_pi_60MB

### Nº de homens pretos com 60 ou + ocupados no PI #############################

ocu_pi_60HP <- svytotal(~VD4002 == "Pessoas ocupadas",  
                        subset(PNADcPI, V2007 == "Homem" & V2010 == "Preta" 
                               & V2009 >= 60 ), na.rm = T)

ocu_pi_60HP

### Nº de homens brancos com 60 ou + ocupados no PI ############################

ocu_pi_60HB <- svytotal(~VD4002 == "Pessoas ocupadas",  
                        subset(PNADcPI, V2007 == "Homem" & V2010 == "Branca" 
                               & V2009 >= 60 ), na.rm = T)
ocu_pi_60HB

### Renda média para os grupos acima ###########################################
### Método alternativo para a elaboração dos dados.
### PNAD de ocupados no PI.
PNADcPI_ocu <- subset(PNADcPI, VD4002 == "Pessoas ocupadas")
ocupi_60b <- svytotal(~V2009>= 60, PNADcPI_ocu, na.rm = T)
ocupi_60b # o nº bate com o divulgado e com o método anterior.

### Renda média das pessoas ocupadas no PI.
rmocupi <- svymean(~VD4020, PNADcPI_ocu, na.rm = T)
rmocupi
### Renda média de homens ocupados no PI
rmocupi_H <- svymean(~VD4020, subset(PNADcPI_ocu, V2007 == "Homem"), na.rm = T)
rmocupi_H

### Renda média de mulheres ocupadas no PI
rmocupi_M <- svymean(~VD4020, subset(PNADcPI_ocu, V2007 == "Mulher"), na.rm = T)
rmocupi_M
### Renda média de pessoas ocupadas com 60+ no PI
rmocupi_60 <- svymean(~VD4020, subset(PNADcPI_ocu, V2009 >= 60), na.rm = T)
rmocupi_60
### Renda média de homens ocupados com 60+ no PI
rmocupi_60H <- svymean(~VD4020, subset(PNADcPI_ocu, V2007 == "Homem" & V2009 >= 60), na.rm = T)
rmocupi_60H

### Renda média de mulheres ocupadas com 60+ no PI
rmocupi_60M <- svymean(~VD4020, subset(PNADcPI_ocu, V2007 == "Mulher" & V2009 >= 60), na.rm = T)
rmocupi_60M

### Renda média de homens pretos ocupados com 60+ no PI
rmocupi_60HP <- svymean(~VD4020, subset(PNADcPI_ocu, V2007 == "Homem" & V2009 >= 60
                                        & V2010 == "Preta"), na.rm = T)
rmocupi_60HP


### Renda média de homens brancos ocupados com 60+ no PI
rmocupi_60HB <- svymean(~VD4020, subset(PNADcPI_ocu, V2007 == "Homem" & V2009 >= 60
                                        & V2010 == "Branca"), na.rm = T)
rmocupi_60HB

### Renda média de mulheres pretas ocupadas com 60+ no PI
rmocupi_60MP <- svymean(~VD4020, subset(PNADcPI_ocu, V2007 == "Mulher" & V2009 >= 60
                                        & V2010 == "Preta"), na.rm = T)
rmocupi_60MP

### Renda média de mulheres brancas ocupadas com 60+ no PI
rmocupi_60MB <- svymean(~VD4020, subset(PNADcPI_ocu, V2007 == "Mulher" & V2009 >= 60
                                        & V2010 == "Branca"), na.rm = T)
rmocupi_60MB


#################### Resultados ################################################
#################### Até agora #################################################
# Taxa de desocupação no PI = 0.09245134 [OK]
# Número de pessoas ocupadas no PI = 1305985 [OK]
# Número de pessoas ocupadas com 60 ou + no PI = 97441 [OK]
# Nº de mulheres pretas com 60+ ocupadas no PI = 7980
# Nº de mulheres brancas com 60+ ocupadas no PI = 7287
# Nº de homens pretos com 60+ ocupados no PI = 9086
# Nº de homens brancos com 60+ ocupados no PI = 14815
# Renda média das pessoas ocupadas no PI = R$ 2055.9
# Renda média das mulheres ocupadas no PI = R$ 2026.9
# Renda média dos homens ocupados no PI = R$ 2075.9
# Renda média das mulheres ocupadas com 60+ no PI = R$ 3359.4
# Renda média dos homens ocupados com 60+ no PI = R$ 3367.1
# Renda média de mulheres pretas ocupadas com 60+ no PI = R$ 6606.1
# Renda média de mulheres brancas ocupadas com 60+ no PI = R$ 4706.2
# Renda média dos homens pretos ocupados com 60+ no PI = R$ 3754.2
# Renda média dos homens brancos ocupados com 60+ no PI = R$ 3777.9




### Escolaridade ###############################################################

### Criando uma "PNADc das pessoas ocupadas com mais de 60 anos no PI" #########
PNADcPI_ocu60<- subset(PNADcPI, VD4002 == "Pessoas ocupadas" & V2009 >= 60)
totalsr_ocu60 <- svytotal(~V2007 + V2010, PNADcPI_ocu60, na.rm = T)
totalsr_ocu60 # H + M bate com o que foi divulgado.

### Os mesmos dados acima, só que cruzados
totalsr_ocu60crz <- svytotal(~ interaction(V2007, V2010), PNADcPI_ocu60, na.rm = T)
totalsr_ocu60crz # Totais batem com o que foi divulgado.

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

### Renda média por sexo x raça
mediaRendars <- svyby(~VD4020, ~interaction(V2007, V2010), PNADcPI_ocu60, svymean, na.rm = T)
mediaRendars # Bate com os resultados que eu havia calculado antes.


