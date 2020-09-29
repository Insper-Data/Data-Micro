#Projeto Insper Data - Duque

#Pacotes utilizados

rm(list=ls())

library(dplyr)
library(tidyverse)
library(microdatasus)
library(skimr)
library(readxl)
library(lubridate)

#Aprendendo a usar a base dataSUS

#Ha tres bases de dados disponiveis:
#Entradas no SUS (SIH RD), obitos (SIM) 
#e nascimentos (SINASC)


#teste para entradas no SUS
#teste com o Rio de Janeiro
install.packages("devtools")
devtools::install_github("rfsaldanha/microdatasus")

####

## Analise da COVID-19 

# Selecione diretorio contendo as bases 

setwd("C:/Users/Pedro Saboia/Desktop/Insper Data")

# Buscando os dados referentes a COVID-19

covid_bruto <- read_excel("~/Desktop/Insper Data/HIST_PAINEL_COVIDBR_31ago2020_1.xlsx",col_types = c("text", 
                                        "text", "text","numeric","numeric","numeric",
                                        "text", "date","numeric","numeric","numeric","numeric",
                                        "numeric","numeric","numeric","numeric","logical"))


covid_mensal <- covid_bruto %>%
  filter(codmun != 0, 
         codRegiaoSaude != 0,
         data == as.Date("2020-03-31")|
           data == as.Date("2020-04-30")|
           data == as.Date("2020-05-31")|
           data == as.Date("2020-06-30")|
           data == as.Date("2020-07-31")|
           data == as.Date("2020-08-31")) %>% 
  select(-c(emAcompanhamentoNovos, Recuperadosnovos, semanaEpi))

covid_m_rs <- covid_mensal %>% 
  group_by(codRegiaoSaude, data) %>% 
  summarise(Casos_Acumulados = sum(casosAcumulado))

municipios <- covid_mensal %>% 
  group_by(codmun) %>% 
  summarise(municipio = municipio) %>% 
  unique()

dic <- covid_mensal %>% 
  group_by(codmun) %>% 
  summarise(codSaude = mean(codRegiaoSaude),
            nome_regiao = nomeRegiaoSaude,
            estado = estado) %>% 
  unique()

## Populacao 


pop <- read_excel('ESTIMA_PO.2019-2019.xls', col_names = c('cod', 'Mun', 'populacao'),
                  col_types = c('text', 'text', 'numeric')) %>% 
  select(-cod) %>% 
  filter(Mun != 'Munic?pios')

popul <- municipios %>% 
  left_join(pop, by = c('municipio' = 'Mun')) %>%
  filter(!is.na(populacao)) %>%
  left_join(dic, by = c('codmun' = 'codmun')) %>%
  group_by(codSaude, nome_regiao, estado) %>%
  summarise(populacao_total = sum(populacao))

## Criacao do indice de contaminacao  

covid_data <- covid_m_rs %>% 
  left_join(popul, by = c('codRegiaoSaude' = 'codSaude')) %>% 
  mutate(casos_relativos = (Casos_Acumulados / populacao_total) * 10000)

covid_data %>% 
  group_by(codRegiaoSaude) %>% 
  summarise(sd= sd(casos_relativos)) %>% 
  View()

covid_m_rs <- covid_m_rs %>% 
  group_by(data) %>%
  mutate(index = ((Casos_Acumulados - min(Casos_Acumulados))/(max(Casos_Acumulados) - min(Casos_Acumulados))) * 100)


## Baixando dados de Maio, Junho e Julho para construcao da base final


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

obitos <- SIH_18 %>% 
  left_join(SIH_19, by = c('MES_CMPT' = 'MES_CMPT', 'MUNIC_RES' = 'MUNIC_RES')) %>% 
  left_join(SIH_20, by = c('MES_CMPT' = 'MES_CMPT', 'MUNIC_RES' = 'MUNIC_RES'))

#======================================================================================================================

SIH_18 <- SIH_18 %>%   
  filter(MORTE=="Sim") %>% 
  select(MUNIC_RES, MES_CMPT, MORTE, DIAG_PRINC, RACA_COR, SEXO, IDADE) %>% 
  mutate(MUNIC_RES = as.integer(MUNIC_RES))

SIH_19 <- SIH_19 %>%   
  filter(MORTE=="Sim") %>% 
  select(MUNIC_RES, MES_CMPT, MORTE, RACA_COR, SEXO, IDADE) %>% 
  mutate(MUNIC_RES = as.integer(MUNIC_RES)) 

SIH_20 <- SIH_20 %>%   
  filter(MORTE=="Sim") %>% 
  select(MUNIC_RES, MES_CMPT, MORTE, DIAG_PRINC, RACA_COR, SEXO, IDADE) %>% 
  mutate(MUNIC_RES = as.integer(MUNIC_RES))

dic_doencas <- read_excel("~/Downloads/CID-10-SUBCATEGORIAS.xls")
dic_doencas <- dic_doencas %>% 
  mutate(codigo_doenca = SUBCAT,
         descricao = DESCRICAO) %>% 
  select(codigo_doenca, descricao)

SIH_20 %>% 
  group_by(DIAG_PRINC) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  head(15) %>% 
  ggplot(aes(x = DIAG_PRINC, y = total)) +
  geom_col(aes(fill = DIAG_PRINC))

#Adicionando o codigo da regiao de saude

mort_reg_18 <- SIH_18 %>% 
  left_join(dic, by = c("MUNIC_RES" = "codmun")) %>% 
  group_by(MES_CMPT, codSaude) %>% 
  summarise(mortes_18 = n()) %>% 
  mutate(ANO = 2018)

mort_reg_19 <- SIH_19 %>% 
  left_join(dic, by = c("MUNIC_RES" = "codmun")) %>% 
  group_by(MES_CMPT, codSaude) %>% 
  summarise(mortes_19 = n()) %>% 
  mutate(ANO = 2019)

mort_reg_20 <- SIH_20 %>% 
  left_join(dic, by = c("MUNIC_RES" = "codmun")) %>% 
  group_by(MES_CMPT, codSaude) %>% 
  summarise(mortes_20 = n()) %>% 
  mutate(ANO = 2020)

excesso_f <- mort_reg_18 %>% 
  left_join(mort_reg_19, by = c("codSaude" = "codSaude",
                                "MES_CMPT" = "MES_CMPT")) %>% 
  left_join(mort_reg_20, by = c("codSaude" = "codSaude",
                                "MES_CMPT" = "MES_CMPT")) %>% 
  mutate(excesso_mortes = mortes_20 / 
           ((mortes_18 + mortes_19)/2))

rm(SIH_18,SIH_19, SIH_20)

save(mort_reg_18, file = "STH_18.Rdata")
save(mort_reg_19, file = "STH_19.Rdata")
save(mort_reg_20, file = "STH_20.Rdata")


#Organizando dados do PIB

getwd()
setwd("C:\\Users\\arthu\\Desktop\\6o Semestre\\InsperData\\COVID\\DiffDiff")

base_PIB <- read_excel("PIB_2010_2017.xlsx")

base_casos_obitos <- read_excel("casos_obitos.xlsx", col_types = c("text", "text", "text", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "numeric"))

###

colnames(base_PIB)

base_PIB <- base_PIB %>% 
  rename(codigo_microrregiao = "C?digo da Microrregi?o")

base_PIB <- base_PIB %>% 
  rename(codigo_estado = "C?digo da Grande Regi?o") %>% 
  rename(estado = "Nome da Grande Regi?o") %>% 
  rename(codigo_UF = "C?digo da Unidade da Federa??o") %>% 
  rename(UF = "Sigla da Unidade da Federa??o") %>% 
  rename(nome_UF = "Nome da Unidade da Federa??o") %>% 
  rename(codigo_municipio = "C?digo do Munic?pio") %>% 
  rename(nome_municipio = "Nome do Munic?pio") %>% 
  rename(regiao_metropolitana = "Regi?o Metropolitana") %>% 
  rename(codigo_mesorregiao = "C?digo da Mesorregi?o") %>% 
  rename(nome_mesorregiao = "Nome da Mesorregi?o") %>% 
  rename(nome_microrregiao =  "Nome da Microrregi?o") %>% 
  rename(PIB_per_capita = "Produto Interno Bruto per capita, \r\na pre?os correntes\r\n(R$ 1,00)")

PIB <- base_PIB %>% 
  group_by(Ano, codigo_microrregiao) %>%
  summarise(PIB_p_capita = mean(PIB_per_capita)) %>% 
  filter(Ano == 2017) %>% 
  arrange(desc(PIB_p_capita)) %>% 
  mutate(grupo = ntile(PIB_p_capita, 3),
         Riqueza = ifelse(grupo == 3, 'RICA', ifelse(grupo == 2, 'MEDIA', 'POBRE')))

base_casos_obitos <- base_casos_obitos %>% 
  rename(codigo_microrregiao = codRegiaoSaude) %>% 
  rename(nome_municipio = municipio)

casos_obitos <- base_casos_obitos %>% 
  mutate(data = as_date(data)) %>% 
  separate(data, into = c("ano", "mes", "dia", sep = "-"))

casos_obitos <- casos_obitos %>% 
  group_by(casos_obitos$codigo_microrregiao, casos_obitos$mes) %>% 
  summarise(casos_acumulados = sum(casosAcumulado))

### Primeiro teste envolvendo regress?o

data <- excesso_f %>% 
  select(MES_CMPT, codSaude, excesso_mortes) %>%
  left_join(PIB, by = c('codSaude' = 'codigo_microrregiao')) %>% 
  filter(!is.na(PIB_p_capita)) %>% 
  select(-c(Ano, grupo)) %>% 
  mutate(MES_CMPT = as.character(MES_CMPT),
         codSaude = as.character(codSaude)) %>% 
  mutate(treat_pobre = 0) %>%
  mutate(treat_media = 0) %>%
  mutate(treat_rica = 0) %>% 
  mutate(pre_2018 = 0) %>% 
  mutate(pre_2019 = 0) %>% 
  mutate(post_2020 = 0)

data$treat_pobre = ifelse(data$Riqueza == "POBRE", 1, 0 )
data$treat_media = ifelse(data$Riqueza == "MEDIA", 1, 0 )
data$treat_rica = ifelse(data$Riqueza == "RICA", 1, 0 )

data <- data %>% 
  arrange(desc(treat_pobre))


lm(excesso_mortes ~ Riqueza + codSaude, data = data)


## Dados demogr?ficos populacionais

age <- read.csv('POPBR12.CSV')

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
  group_by(nome_microrregiao) %>% 
  summarise(POPULACAO = sum(POPULACAO),
            TOTAL = sum(TOTAL)) %>% 
  mutate(mais65 = POPULACAO / TOTAL) %>% 
  select(-c(POPULACAO:TOTAL))

# Base de dados final 

COVID <- excesso_f %>% 
  select(MES_CMPT, codSaude, excesso_mortes) %>%
  left_join(PIB, by = c('codSaude' = 'codigo_microrregiao')) %>% 
  filter(!is.na(PIB_p_capita)) %>% 
  select(-c(Ano, grupo)) %>% 
  left_join(popul, by = c('codSaude' = 'codSaude')) %>% 
  left_join(mais65, by = c('codSaude' = 'codSaude')) %>% 
  mutate(MES_CMPT = as.character(MES_CMPT),
         codSaude = as.character(codSaude))

lm(excesso_mortes ~ Riqueza + populacao_total + mais65 + codSaude, data = COVID)


#
#
#
#
#
#
#
#### Projeto Insper Data - Duque

rm(list = ls())

# Selecione o dirtorio a ser utilizado

setwd("C:/Users/Pedro Saboia/Desktop/Insper Data")

# Pacotes utilizados

library(dplyr)
library(tidyverse)
library(microdatasus)
library(skimr)
library(readxl)
library(lubridate)
library(fabricatr)

#===========================================================================================
## Dados referentes ao PIB Muncipal 
#===========================================================================================

# Base de dados com o PIB Municipal

base_PIB <- read_excel("~/Downloads/PIB dos Munic°pios - base de dados 2010-2017.xls") %>% 
  rename(codigo_regiao = "Código da Grande Região", 
         regiao = "Nome da Grande Região",
         codigo_UF = "Código da Unidade da Federação",
         UF = "Sigla da Unidade da Federação",
         nome_UF = "Nome da Unidade da Federação",
         codigo_municipio = "Código do Município",
         nome_municipio = "Nome do Município",
         regiao_metropolitana = "Região Metropolitana",
         codigo_mesorregiao = "Código da Mesorregião",
         nome_mesorregiao = "Nome da Mesorregião",
         codigo_microrregiao = "Código da Microrregião",
         nome_microrregiao =  "Nome da Microrregião",
         PIB = "Produto Interno Bruto, a preços correntes (R$ 1.000)") %>%
  filter(Ano == 2017) %>% 
  select(regiao, UF, 
         nome_municipio, 
         nome_mesorregiao,
         nome_microrregiao, 
         PIB)


#===========================================================================================
## Dados referentes a contaminacao por COVID-19
#===========================================================================================

# Base de dados bruta -> ?ltima atualiza??o: 31/ago/2020

covid_bruto <- read_excel("~/Desktop/Insper Data/HIST_PAINEL_COVIDBR_31ago2020_1.xlsx",
                          col_types = c('text', 'text', 'text','numeric','numeric','numeric',
                                        'text', 'date','numeric','numeric','numeric','numeric',
                                        'numeric','numeric','numeric','numeric','logical'))

# Pegar apenas os dados referentes ao final de cada mes

covid_mensal <- covid_bruto %>%
  filter(codmun != 0, 
         codRegiaoSaude != 0,
         data == as.Date("2020-03-31")|
           data == as.Date("2020-04-30")|
           data == as.Date("2020-05-31")|
           data == as.Date("2020-06-30")|
           data == as.Date("2020-07-31")|
           data == as.Date("2020-08-31")) %>% 
  select(codmun, municipio, data, populacaoTCU2019,
         casosAcumulado, `interior/metropolitana`) %>% 
  rename(populacao = populacaoTCU2019,
         metropolitana = `interior/metropolitana`) %>% 
  separate(data, into = c('ano', 'mes', 'dia'), sep = "-") %>% 
  separate(mes, into = c('zero', 'mes'), sep = 1) %>%
  select(-c(ano, dia, zero)) 


# Com essa base temos a populacao, o municipio, e indicador de zona urbana.   

#===========================================================================================
## Populacao mais de 65 anos
#===========================================================================================

age <- read_csv("~/Downloads/POPBR12.csv")

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
  group_by(nome_microrregiao) %>% 
  summarise(POPULACAO = sum(POPULACAO),
            TOTAL = sum(TOTAL)) %>% 
  mutate(mais65 = POPULACAO / TOTAL) %>% 
  select(-c(POPULACAO:TOTAL))


#===========================================================================================
## Dados sobre mortalidade
#===========================================================================================


mortes <- obitos %>%
  mutate(MES_CMPT = as.character(MES_CMPT)) %>% 
  left_join(covid_mensal, by = c('MUNIC_RES' = 'codmun', 'MES_CMPT' = 'mes')) %>% 
  left_join(base_PIB, by = c('municipio' = 'nome_municipio')) %>% 
  group_by(MES_CMPT, nome_microrregiao) %>%
  filter(!is.na(mortes_18),
         !is.na(mortes_19),
         !is.na(mortes_20)) %>% 
  summarise(mortes_18 = sum(mortes_18),
            mortes_19 = sum(mortes_19),
            mortes_20 = sum(mortes_20)) %>% 
  mutate(excesso = (mortes_20) / ( (mortes_18 + mortes_19) / 2 ) ) %>% 
  select(-c(mortes_18:mortes_20))


#===========================================================================================
## Juntando todos os dados em uma base
#===========================================================================================

# Juntando as bases em uma, para fazer as regress?es

COVID <- covid_mensal %>% 
  left_join(base_PIB, by = c('municipio' = 'nome_municipio')) %>%  
  group_by(nome_microrregiao, mes) %>% 
  summarise(populacao = sum(populacao),
            casosAcumulado = sum(casosAcumulado),
            PIB = sum(PIB),
            regiao = regiao,
            metropolitana = mean(metropolitana),
            UF = UF,
            mesorregiao = nome_mesorregiao) %>% 
  unique() %>% 
  mutate(casos_relativos = casosAcumulado / populacao,
         PIB_per_capita = PIB / populacao,
         ua = ifelse(metropolitana > 0.5, TRUE, FALSE)) %>% 
  group_by(nome_microrregiao) %>% 
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
  right_join(mortes, by = c('nome_microrregiao' = 'nome_microrregiao')) %>% 
  left_join(mais65, by = c('nome_microrregiao' = 'nome_microrregiao')) %>% 
  filter(MES_CMPT == 7)


Acum <- covid_mensal %>% left_join(base_PIB,by = c('municipio' = 'nome_municipio')) %>% 
  select(nome_microrregiao, codmun, mes, casosAcumulado) %>% 
  filter(mes == 7) %>% 
  group_by(nome_microrregiao) %>% 
  summarise(casosAcumulado = sum(casosAcumulado))

median(COVID_test$casos_per_capita, na.rm = TRUE)
median(COVID$volatilidade, na.rm = TRUE)

COVID_test <- COVID %>% 
  filter(!is.na(nome_microrregiao)) %>% 
  left_join(Acum, by = c("nome_microrregiao" = "nome_microrregiao")) %>% 
  mutate(casos_per_capita = (casosAcumulado / populacao),
         infec1 = ifelse(volatilidade >= 0.006036, "ALTA", "BAIXA"),
         infec2 = ifelse(casos_per_capita >= 0.009373, "ALTA", "BAIXA"))


#===========================================================================================
## Regressao
#===========================================================================================

lm(excesso ~ wealth + ua + mais65 + regiao + populacao, data = COVID)

lm(excesso ~ wealth + infec1 + ua + mais65 + regiao + populacao, data = COVID_test)

lm(excesso ~ wealth + infec2 + ua + mais65 + regiao + populacao, data = COVID_test)

lm(excesso ~ wealth + infec1 + ua + regiao + populacao, data = COVID_test)

lm <- lm(excesso ~ wealth + infec1 + ua + regiao + populacao, data = COVID_test)
lm2 <- lm(excesso ~ wealth + infec2 + ua + regiao + populacao, data = COVID_test)
lm3 <- lm(excesso ~ wealth + infec2 + ua + mais65 + regiao + populacao, data = COVID_test)
lm4 <- lm(excesso ~ wealth + infec1 + infec1*wealth + ua + mais65 + regiao + populacao, data = COVID_test)

summary(lm(excesso ~ wealth + infec1 + ua + mais65 + regiao + populacao, data = COVID_test))

