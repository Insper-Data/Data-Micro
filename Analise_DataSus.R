#===========================================================================================
## Projeto Insper Data - Duque
# Dados DataSus
#===========================================================================================

rm(list = ls())

#===========================================================================================
# Selecione o dirtorio a ser utilizado
#===========================================================================================

setwd('C:\\Users\\arthu\\Desktop\\InsperData\\COVID\\DiffDiff\\Data-Micro')
setwd('C:/Users/Pedro Saboia/Desktop/Insper Data')

#===========================================================================================
# Pacotes utilizados
#===========================================================================================

library(dplyr)
library(tidyverse)
library(microdatasus)
library(skimr)
library(readxl)
library(lubridate)
library(broom)
library(openxlsx)

#===========================================================================================
## Dados sobre mortalidade hospitalar
#===========================================================================================
## Caso não tenha os dados no sistema 
#===========================================================================================

SIH_18 <- fetch_datasus(year_start = 2018, month_start = 1, year_end = 2018, month_end = 8, information_system = "SIH-RD")
SIH_18 <- process_sih(SIH_18)

SIH_19 <- fetch_datasus(year_start = 2019, month_start = 1, year_end = 2019, month_end = 8, information_system = "SIH-RD")
SIH_19 <- process_sih(SIH_19)

SIH_20 <- fetch_datasus(year_start = 2020, month_start = 1, year_end = 2020, month_end = 8, information_system = "SIH-RD")
SIH_20 <- process_sih(SIH_20)

SIH_18 <- SIH_18 %>%   
  filter(MORTE=="Sim") %>% 
  select(MUNIC_RES, MES_CMPT, MORTE, RACA_COR, SEXO, IDADE) %>% 
  mutate(MUNIC_RES = as.integer(MUNIC_RES)) %>% 
  group_by(MES_CMPT, MUNIC_RES) %>% 
  summarise(mortes_18 = n())

SIH_19 <- SIH_19 %>%   
  filter(MORTE=="Sim") %>% 
  select(MUNIC_RES, MES_CMPT, MORTE, RACA_COR, SEXO, IDADE) %>% 
  mutate(MUNIC_RES = as.integer(MUNIC_RES)) %>% 
  group_by(MES_CMPT, MUNIC_RES) %>% 
  summarise(mortes_19 = n())

SIH_20 <- SIH_20 %>%   
  filter(MORTE=="Sim") %>% 
  select(MUNIC_RES, MES_CMPT, MORTE, RACA_COR, SEXO, IDADE) %>% 
  mutate(MUNIC_RES = as.integer(MUNIC_RES)) %>% 
  group_by(MES_CMPT, MUNIC_RES) %>% 
  summarise(mortes_20 = n())

#===========================================================================================
# Dados referentes ao PIB Muncipal 
#===========================================================================================
# Base de dados com o PIB Municipal
#===========================================================================================

base_PIB <- read_excel("PIB_2010_2017.xlsx") %>% 
  filter(Ano == 2017) %>%
  rename(codigo_regiao = "C�digo da Grande Regi�o", 
         regiao = "Nome da Grande Regi�o",
         codigo_UF = "C�digo da Unidade da Federa��o",
         UF = "Sigla da Unidade da Federa��o",
         nome_UF = "Nome da Unidade da Federa��o",
         codigo_municipio = "C�digo do Munic�pio",
         nome_municipio = "Nome do Munic�pio",
         regiao_metropolitana = "Regi�o Metropolitana",
         codigo_mesorregiao = "C�digo da Mesorregi�o",
         nome_mesorregiao = "Nome da Mesorregi�o",
         codigo_microrregiao = "C�digo da Microrregi�o",
         nome_microrregiao =  "Nome da Microrregi�o",
         PIB = "Produto Interno Bruto, \r\na pre�os correntes\r\n(R$ 1.000)") %>%
  select(regiao, UF, nome_UF, 
         nome_municipio, 
         nome_mesorregiao,
         nome_microrregiao, 
         codigo_microrregiao,
         PIB)

#===========================================================================================
## Dados referentes a contaminacao por COVID-19
#===========================================================================================
# Base de dados bruta -> ultima atualizacao: 31/ago/2020                                 
#===========================================================================================

covid_bruto <- read_excel("HIST_PAINEL_COVIDBR_31ago2020_1.xlsx",
                          col_types = c('text', 'text', 'text','numeric','numeric','numeric',
                                        'text', 'date','numeric','numeric','numeric','numeric',
                                        'numeric','numeric','numeric','numeric','logical'))

#===========================================================================================
# Pegar apenas os dados referentes ao final de cada mes
#===========================================================================================

covid_mensal <- covid_bruto %>%
  filter(codmun != 0, 
         codRegiaoSaude != 0,
         data == as.Date("2020-01-31")|
           data == as.Date("2020-02-29")|
           data == as.Date("2020-03-31")|
           data == as.Date("2020-04-30")|
           data == as.Date("2020-05-31")|
           data == as.Date("2020-06-30")|
           data == as.Date("2020-07-31")|
           data == as.Date("2020-08-31")|
           data == as.Date("2020-09-30")) %>% 
  select(codmun, municipio, data, populacaoTCU2019,
         casosAcumulado, `interior/metropolitana`) %>% 
  rename(populacao = populacaoTCU2019,
         metropolitana = `interior/metropolitana`) %>% 
  separate(data, into = c('ano', 'mes', 'dia'), sep = "-") %>% 
  separate(mes, into = c('zero', 'mes'), sep = 1) %>%
  select(-c(ano, dia, zero)) %>% 
  mutate(pop_interior = ifelse(metropolitana == FALSE, populacao, 0),
         pop_urbana = ifelse(metropolitana == TRUE, populacao, 0))

#===========================================================================================
# Com essa base temos a populacao, o municipio, e indicador de zona urbana.   
#===========================================================================================
# Populacao acima de 65 anos
#===========================================================================================

age <- read_csv("POPBR12.csv")

poptotal <- age %>% 
  group_by(MUNIC_RES) %>% 
  summarise(TOTAL = sum(POPULACAO))

mais65 <- age %>%
  separate(FXETARIA, into = c('min', 'max'), sep = -2) %>% 
  filter(min >= 65,
         min != 7,
         min != 8,
         min != 9) %>% 
  unite(faixa, min, max, sep = "-") %>% 
  group_by(MUNIC_RES) %>% 
  summarise(POPULACAO = sum(POPULACAO)) %>% 
  left_join(poptotal, by = c('MUNIC_RES' = 'MUNIC_RES'))%>% 
  left_join(covid_mensal, by = c('MUNIC_RES' = 'codmun')) %>% 
  select(c(POPULACAO:municipio)) %>% 
  left_join(base_PIB, by = c('municipio' = 'nome_municipio')) %>% 
  unique() %>% 
  group_by(nome_microrregiao, codigo_microrregiao) %>% 
  summarise(POPULACAO = sum(POPULACAO),
            TOTAL = sum(TOTAL)) %>% 
  mutate(mais65 = POPULACAO / TOTAL) %>% 
  select(-c(POPULACAO:TOTAL))

#===========================================================================================
# Caso tenha os dados de mortalidade hospitalar no sistema 
#===========================================================================================

load("SIH_18.Rdata")
load("SIH_19.Rdata")
load("SIH_20.Rdata")

#===========================================================================================
# Agrupando e preparando
#===========================================================================================
# Dados hospitalares
#===========================================================================================

SIH_19 <- SIH_19_f 

obitos <- SIH_18 %>% 
  left_join(SIH_19, by = c('MES_CMPT' = 'MES_CMPT', 'MUNIC_RES' = 'MUNIC_RES')) %>% 
  left_join(SIH_20, by = c('MES_CMPT' = 'MES_CMPT', 'MUNIC_RES' = 'MUNIC_RES')) %>% 
  filter(MES_CMPT != 8) #ainda não temos dados de agosto

write.xlsx(obitos, "obitos.xlsx")

mortes <- obitos %>%
  mutate(MES_CMPT = as.character(MES_CMPT)) %>% 
  left_join(covid_mensal, by = c('MUNIC_RES' = 'codmun')) %>% 
  left_join(base_PIB, by = c('municipio' = 'nome_municipio')) %>% 
  filter(mes == 3) %>% 
  group_by(MES_CMPT, nome_microrregiao, codigo_microrregiao) %>%
  filter(!is.na(mortes_18),
         !is.na(mortes_19),
         !is.na(mortes_20)) %>% 
  summarise(mortes_18 = sum(mortes_18),
            mortes_19 = sum(mortes_19),
            mortes_20 = sum(mortes_20)) %>% 
  mutate(excesso = (mortes_20) / ( (mortes_18 + mortes_19) / 2 ) ) %>% 
  select(-c(mortes_18:mortes_20))

#===========================================================================================
# Juntando todos os dados em uma base
#===========================================================================================
# Juntando as bases em uma, para fazer as regress�es
#===========================================================================================

dados_dataSUS_excesso <- covid_mensal %>% 
  left_join(base_PIB, by = c('municipio' = 'nome_municipio')) %>%  
  group_by(nome_microrregiao, codigo_microrregiao, mes) %>% 
  summarise(populacao = sum(populacao),
            casosAcumulado = sum(casosAcumulado),
            PIB = sum(PIB),
            interior = sum(pop_interior),
            urbana = sum(pop_urbana),
            regiao = regiao,
            metropolitana = mean(metropolitana),
            UF = UF,
            mesorregiao = nome_mesorregiao) %>% 
  unique() %>% 
  mutate(casos_relativos = casosAcumulado / populacao,
         PIB_per_capita = PIB / populacao,
         ua = ifelse(metropolitana > 0.5, TRUE, FALSE)) %>% 
  group_by(nome_microrregiao, codigo_microrregiao) %>% 
  summarise(volatilidade = sd(casos_relativos),
            PIB_per_capita = PIB_per_capita,
            regiao = regiao,
            ua = ua,
            UF = UF,
            mesorregiao = mesorregiao,
            populacao = populacao) %>% 
  unique() %>%
  filter(!is.na(nome_microrregiao),
         !is.na(PIB_per_capita)) %>%
  arrange(desc(PIB_per_capita)) %>%
  mutate(wealth = ifelse(PIB_per_capita >= 27.364441, 'RICO',
                         ifelse(PIB_per_capita >= 14.771092, 'MEDIO', 'POBRE'))) %>% 
  right_join(mortes, by = c('codigo_microrregiao' = 'codigo_microrregiao')) %>% 
  left_join(mais65, by = c('codigo_microrregiao' = 'codigo_microrregiao')) %>%  
  mutate(infect = ifelse(volatilidade >= 0.006040925, "ALTA", "BAIXA"),
         corona = ifelse(MES_CMPT > 3, TRUE, FALSE)) %>% 
  mutate(alta = ifelse(wealth == "RICO", TRUE, FALSE),
         baixa = ifelse(wealth == "POBRE", TRUE, FALSE),
         media = ifelse(wealth == "MEDIO", TRUE, FALSE),
         MES_CMPT = as.double(MES_CMPT)) %>%
  mutate(excesso_2 = excesso -1) %>% 
  select(UF, regiao, codigo_microrregiao, nome_microrregiao, MES_CMPT, excesso, excesso_2, corona, wealth, ua, everything()) %>% 
  select(-c(volatilidade, infect, mesorregiao)) %>% 
  rename("mes" = "MES_CMPT")

dados_dataSUS_excesso <- dados_dataSUS_excesso %>% 
  mutate(p_interior = pop_interior/populacao, p_urbana = pop_urbana/populacao)

write.xlsx(dados_dataSUS_excesso, "dados_dataSUS.xlsx")

#===========================================================================================
# Regressao hospitalar
#===========================================================================================

summary(lm(excesso ~ wealth*corona + wealth + corona + ua + mais65 + regiao + populacao, data = dados_dataSUS_excesso)) #com Efeitos Fixos de Regiao e sem Efeitos Fixos de Tempo

summary(lm(excesso ~ wealth*corona + wealth + corona + ua + mais65 + UF + populacao, data = dados_dataSUS_excesso)) #com Efeitos Fixos de Unidade Federativa e sem Efeitos Fixos de Tempo

summary(lm(excesso ~ wealth*corona + wealth + corona + ua + mais65 + populacao + mes, data = dados_dataSUS_excesso)) #sem Efeitos Fixos de Regiao/UF e com Efeitos Fixos de Tempo

summary(lm(excesso ~ wealth*corona + wealth + corona + ua + mais65 + UF + populacao + mes, data = dados_dataSUS_excesso)) #com Efeitos Fixos de Unidade Federativa e com Efeitos Fixos de Tempo

summary(lm(excesso ~ wealth*corona + wealth + corona + ua + mais65 + regiao + populacao + mes, data = dados_dataSUS_excesso)) #com Efeitos Fixos de Regiao e com Efeitos Fixos de Tempo

summary(lm(excesso_2 ~ wealth*corona + wealth + corona + ua + mais65 + regiao + populacao + mes, data = dados_dataSUS_excesso)) #com Efeitos Fixos de Regiao e com Efeitos Fixos de Tempo

#===========================================================================================
# Regressao com efeitos fixos de regiao
#===========================================================================================

reg_regiao <- summary(lm(excesso ~ wealth*corona + wealth + corona + ua + mais65 + regiao + populacao + mes, data = dados_dataSUS_excesso))

tidy_reg_regiao <- tidy(reg_regiao)
tidy_reg_regiao

write.csv(tidy_reg_regiao, "reg_regiao.csv")

#===========================================================================================
# Regressao com efeitos fixos de estado
#===========================================================================================

reg_UF <- (lm(excesso ~ wealth*corona + wealth + corona + ua + mais65 + UF + populacao + mes, data = dados_dataSUS_excesso))
reg_UF

tidy_reg_UF <- tidy(reg_UF)
tidy_reg_UF

write.csv(tidy_reg_UF, "reg_UF.csv")

#===========================================================================================
# Extra
#===========================================================================================

#Acum <- covid_mensal %>% 
#left_join(base_PIB,by = c('municipio' = 'nome_municipio')) %>% 
#  select(nome_microrregiao, codmun, mes, casosAcumulado) %>% 
#  filter(mes == 7) %>% 
#  group_by(nome_microrregiao) %>% 
#  summarise(casosAcumulado = sum(casosAcumulado))

#COVID_test <- COVID %>% 
#  filter(!is.na(nome_microrregiao)) %>% 
#  left_join(Acum, by = c("nome_microrregiao" = "nome_microrregiao")) %>% 
#  mutate(casos_per_capita = (casosAcumulado / populacao),
#         infec1 = ifelse(volatilidade >= median(COVID$volatilidade, na.rm = TRUE), "ALTA", "BAIXA"),
#         infec2 = ifelse(casos_per_capita >= 0.009373, "ALTA", "BAIXA"))

#median(COVID_test$casos_per_capita, na.rm = TRUE)
#median(COVID$volatilidade, na.rm = TRUE)

