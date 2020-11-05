#### Projeto Insper Data - Duque

rm(list = ls())

# Selecione o dirtorio a ser utilizado

setwd('C:/Users/Pedro Saboia/Desktop/Insper Data')

# Pacotes utilizados

library(dplyr)
library(tidyverse)
library(microdatasus)
library(skimr)
library(readxl)
library(lubridate)
library(broom)

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
## Dados sobre mortalidade do CONASS
#===========================================================================================

conas_18 <- read_excel("CONASS_2018.xlsx") %>% 
  mutate(Ano = 2018) %>% 
  rename(obitos = "mortes") %>% 
  select(mes, UF, municipio, obitos) %>% 
  rename(obitos_18 = "obitos")

conas_19 <- read_excel("CONASS_2019.xlsx") %>% 
  rename(mes = "Mês",
         municipio = "Município",
         obitos = "Óbitos",
         estado = "UF") %>%
  mutate(UF = ifelse(estado == "Acre", "AC",
              ifelse(estado == "Alagoas", "AL",
              ifelse(estado == "Amapá", "AP", 
              ifelse(estado == "Amazonas", "AM",
              ifelse(estado == "Bahia", "BA",
              ifelse(estado == "Ceara", "CE",
              ifelse(estado == "Espírito Santo", "ES",
              ifelse(estado == "Goiás", "GO",
              ifelse(estado == "Maranhão", "MA",
              ifelse(estado == "Mato Grosso", "MT",
              ifelse(estado == "Mato Grosso do Sul", "MS",
              ifelse(estado == "Minas Gerais", "MG", 
              ifelse(estado == "Pará", "PA",
              ifelse(estado == "Paraíba", "PB", 
              ifelse(estado == "Paraná", "PR",
              ifelse(estado == "Pernambuco", "PE",
              ifelse(estado == "Piauí", "PI", 
              ifelse(estado == "Rio de Janeiro", "RJ",
              ifelse(estado == "Rio Grande do Norte", "RN",
              ifelse(estado == "Rio Grande do Sul", "RS",
              ifelse(estado == "Rondônia", "RO",
              ifelse(estado == "Roraima", "RR",
              ifelse(estado == "Santa Catarina", "SC",
              ifelse(estado == "São Paulo", "SP",
              ifelse(estado == "Sergipe", "SE",
              ifelse(estado == "Tocantins", "TO", 
              ifelse(estado == "Distrito Federal", "DF", NA)
              ))))))))))))))))))))))))))) %>% 
  select(mes, UF, municipio, obitos) %>% 
  rename(obitos_19 = "obitos")

conas_20 <- read_excel("CONASS_2020.xlsx") %>% 
  select(Mes_num, Sigla, Cidade, Registros) %>% 
  rename(mes = "Mes_num",
         UF = "Sigla", 
         municipio = "Cidade", 
         obitos_20 = "Registros")

#===========================================================================================
## Dados referentes ao PIB Muncipal 
#===========================================================================================

# Base de dados com o PIB Municipal

base_PIB <- read_excel("PIB_2010_2017.xlsx") %>% 
  filter(Ano == 2017) %>%
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
         PIB = "Produto Interno Bruto, \r\na preços correntes\r\n(R$ 1.000)") %>%
  select(regiao, UF, nome_UF, 
         nome_municipio, 
         nome_mesorregiao,
         nome_microrregiao, 
         PIB)

#===========================================================================================
## Dados referentes a contaminacao por COVID-19
#===========================================================================================

# Base de dados bruta -> ultima atualizacao: 31/ago/2020

covid_bruto <- read_excel("HIST_PAINEL_COVIDBR_03nov2020.xlsx",
                          col_types = c('text', 'text', 'text','numeric','numeric','numeric',
                                        'text', 'date','numeric','numeric','numeric','numeric',
                                        'numeric','numeric','numeric','numeric','logical'))

# Pegar apenas os dados referentes ao final de cada mes

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
  select(-c(ano, dia, zero)) 


# Com essa base temos a populacao, o municipio, e indicador de zona urbana.   

#===========================================================================================
## Populacao acima de 65 anos
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
  group_by(nome_microrregiao) %>% 
  summarise(POPULACAO = sum(POPULACAO),
            TOTAL = sum(TOTAL)) %>% 
  mutate(mais65 = POPULACAO / TOTAL) %>% 
  select(-c(POPULACAO:TOTAL))

## Caso tenha os dados de mortalidade hospitalar no sistema 
#===========================================================================================

load("SIH_18.Rdata")
load("SIH_19.Rdata")
load("SIH_20.Rdata")

## Agrupando e preparando
#===========================================================================================

# Dados hospitalares

obitos <- SIH_18 %>% 
  left_join(SIH_19, by = c('MES_CMPT' = 'MES_CMPT', 'MUNIC_RES' = 'MUNIC_RES')) %>% 
  left_join(SIH_20, by = c('MES_CMPT' = 'MES_CMPT', 'MUNIC_RES' = 'MUNIC_RES')) %>% 
  filter(MES_CMPT != 8) #ainda não temos dados completos de agosto

mortes <- obitos %>%
  mutate(MES_CMPT = as.character(MES_CMPT)) %>% 
  left_join(covid_mensal, by = c('MUNIC_RES' = 'codmun')) %>% 
  left_join(base_PIB, by = c('municipio' = 'nome_municipio')) %>% 
  filter(mes == 3)%>% 
  group_by(MES_CMPT, nome_microrregiao) %>%
  filter(!is.na(mortes_18),
         !is.na(mortes_19),
         !is.na(mortes_20)) %>% 
  summarise(mortes_18 = sum(mortes_18),
            mortes_19 = sum(mortes_19),
            mortes_20 = sum(mortes_20)) %>% 
  mutate(excesso = (mortes_20) / ( (mortes_18 + mortes_19) / 2 ) ) %>% 
  select(-c(mortes_18:mortes_20))

# Dados do Conass

conass <- conas_18 %>% 
  full_join(conas_19, by = c('municipio' = 'municipio', 'mes' = 'mes', 'UF' = 'UF')) %>% 
  full_join(conas_20, by = c('municipio' = 'municipio', 'mes' = 'mes', 'UF' = 'UF')) %>% 
  replace_na(list(obitos_18 = 0, obitos_19 = 0, obitos_20 = 0))

conass_mortes <- conass %>%  
  left_join(base_PIB, by = c('municipio' = 'nome_municipio', 'UF' = 'UF')) %>%
  group_by(nome_microrregiao, mes, UF) %>% 
  summarise(obitos_18 = sum(obitos_18),
            obitos_19 = sum(obitos_19),
            obitos_20 = sum(obitos_20)) %>% 
  mutate(relative_mortality = obitos_20 / ((obitos_18 + obitos_19) / 2))

#===========================================================================================
## Dados CNES
#===========================================================================================

cnes <- read_excel('CNES_Final.xlsx' )%>%
  mutate(across(names(cnes)[3:19], as.double)) %>%
  mutate_all(funs(replace_na(., 0))) %>% 
  separate(Municipio, into = c("mun_cod", "mun_nome"), sep = 6) %>% 
  separate(mun_nome, into = c("mun_nome", "cod"), sep = "-") %>% 
  filter(is.na(cod)) %>% 
  select(-cod)

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
  mutate(infect = ifelse(volatilidade >= 0.006040925, "ALTA", "BAIXA"),
         corona = ifelse(MES_CMPT > 3, TRUE, FALSE)) %>% 
  mutate(alta = ifelse(wealth == "RICO", TRUE, FALSE),
         baixa = ifelse(wealth == "POBRE", TRUE, FALSE),
         media = ifelse(wealth == "MEDIO", TRUE, FALSE),
         MES_CMPT = as.double(MES_CMPT)) %>% 
  left_join(conass_mortes, by = c('nome_microrregiao' = 'nome_microrregiao', 'MES_CMPT' = 'mes', 'UF' = 'UF')) %>% 
  filter(obitos_18 != 0, obitos_19 != 0) %>% 
  select(-c(obitos_18:obitos_20))


Acum <- covid_mensal %>% 
  left_join(base_PIB,by = c('municipio' = 'nome_municipio')) %>% 
  select(nome_microrregiao, codmun, mes, casosAcumulado) %>% 
  filter(mes == 7) %>% 
  group_by(nome_microrregiao) %>% 
  summarise(casosAcumulado = sum(casosAcumulado))

COVID_test <- COVID %>% 
  filter(!is.na(nome_microrregiao)) %>% 
  left_join(Acum, by = c("nome_microrregiao" = "nome_microrregiao")) %>% 
  mutate(casos_per_capita = (casosAcumulado / populacao),
         infec1 = ifelse(volatilidade >= median(COVID$volatilidade, na.rm = TRUE), "ALTA", "BAIXA"),
         infec2 = ifelse(casos_per_capita >= 0.009373, "ALTA", "BAIXA"))

median(COVID_test$casos_per_capita, na.rm = TRUE)
median(COVID$volatilidade, na.rm = TRUE)


#===========================================================================================
## Regressao hospitalar
#===========================================================================================

summary(lm(excesso ~ wealth*corona + wealth + corona + ua + mais65 + regiao + populacao, data = COVID)) #com Efeitos Fixos de Regiao e sem Efeitos Fixos de Tempo

summary(lm(excesso ~ wealth*corona + wealth + corona + ua + mais65 + UF + populacao, data = COVID)) #com Efeitos Fixos de Unidade Federativa e sem Efeitos Fixos de Tempo

summary(lm(excesso ~ wealth*corona + wealth + corona + ua + mais65 + populacao + MES_CMPT, data = COVID)) #sem Efeitos Fixos de Regiao/UF e com Efeitos Fixos de Tempo

summary(lm(excesso ~ wealth*corona + wealth + corona + ua + mais65 + UF + populacao + MES_CMPT, data = COVID)) #com Efeitos Fixos de Unidade Federativa e com Efeitos Fixos de Tempo

summary(lm(excesso ~ wealth*corona + wealth + corona + ua + mais65 + regiao + populacao + MES_CMPT, data = COVID)) #com Efeitos Fixos de Regiao e com Efeitos Fixos de Tempo

# Regressao com efeitos fixos de regiao
reg_regiao <- (lm(excesso ~ wealth*corona + wealth + corona + ua + mais65 + regiao + populacao, data = COVID))

tidy_reg_regiao <- tidy(reg_regiao)
tidy_reg_regiao

write.csv(tidy_reg_regiao, "reg_regiao.csv")

# Regressao com efeitos fixos de estado
reg_UF <- (lm(excesso ~ wealth*corona + wealth + corona + ua + mais65 + UF + populacao + MES_CMPT, data = COVID))
reg_UF

tidy_reg_UF <- tidy(reg_UF)
tidy_reg_UF

write.csv(tidy_reg_UF, "reg_UF.csv")

#===========================================================================================
## Regressao CONASS
#===========================================================================================

summary(lm(relative_mortality ~ wealth*corona + wealth + corona + ua + mais65 + UF + populacao, data = COVID))

# Grafico de Excesso de Mortalidade
COVID %>%
  filter(!is.na(wealth)) %>%
  group_by(wealth, MES_CMPT) %>% 
  summarise(excesso = mean(excesso),
            sd = sd(excesso)) %>%
  unique() %>%
  ggplot() + 
  geom_line(aes(MES_CMPT, excesso, group = wealth, color = wealth), size = 2) +
  theme_classic() +
  ylab("Excesso de mortalidade relativo") + 
  xlab("Mês") +
  scale_colour_discrete(name = "Nível de PIB per capita", labels = c("Alto","Médio", "Baixo")) +
  labs(color = "Nível de Renda",
       title = "Evolução do excesso de mortalidade em 2020",
       subtitle = "por microrregião",
       caption = "Fonte: MicroDataSUS e IBGE")

# Gráfico de Excesso de Mortalidade com eixos diferentes
COVID %>%
  filter(!is.na(wealth)) %>%
  group_by(wealth, MES_CMPT) %>% 
  summarise(excesso = mean(excesso)) %>% 
  unique() %>%
  ggplot() + 
  geom_line(aes(MES_CMPT, excesso, group = wealth, color = wealth), size = 2) +
  theme_classic() +
  ylab("Excesso de mortalidade relativo") + 
  xlab("Mês") + 
  coord_cartesian(xlim = c(1,7), ylim = c(0,1.5))+
  scale_colour_discrete(name = "Nível de PIB per capita", labels = c("Alto","Médio", "Baixo"))+
  labs(title = "Evolução do excesso de mortalidade em 2020",
       subtitle = "por microrregião",
       caption = "Fonte: MicroDataSUS e IBGE")

#===========================================================================================
# EXTRA 
#===========================================================================================

Acum <- covid_mensal %>% 
  left_join(base_PIB,by = c('municipio' = 'nome_municipio')) %>% 
  select(nome_microrregiao, codmun, mes, casosAcumulado) %>% 
  filter(mes == 7) %>% 
  group_by(nome_microrregiao) %>% 
  summarise(casosAcumulado = sum(casosAcumulado))1

median(COVID_test$casos_per_capita, na.rm = TRUE)
median(COVID$volatilidade, na.rm = TRUE)

COVID_test <- COVID %>% 
  filter(!is.na(nome_microrregiao)) %>% 
  left_join(Acum, by = c("nome_microrregiao" = "nome_microrregiao")) %>% 
  mutate(casos_per_capita = (casosAcumulado / populacao),
         infec1 = ifelse(volatilidade >= median(COVID_test$volatilidade, na.rm = TRUE), "ALTA", "BAIXA"),
         infec2 = ifelse(casos_per_capita >= 0.009373, "ALTA", "BAIXA"))

##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
