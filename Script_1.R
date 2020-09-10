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

dados <- fetch_datasus(year_start = 2018, month_start = 3, year_end = 2018, month_end = 4, uf = "RJ", information_system = "SIH-RD")
dados <- process_sih(dados)

skim(dados)

#MUNIC_RES indica o cÃ³digo do municÃ­pio de residÃªncia do entrante
#A base com PIBs per capita contÃ©m os mesmo cÃ³digos, porÃ©m com um dÃ­gito a mais


#2020 - Sao Paulo

dados_teste2 <- fetch_datasus(year_start = 2020, month_start = 3, year_end = 2020, month_end = 4,uf="SP", information_system = "SIH-RD")
dados_teste2 <- process_sih(dados_teste2)

#Variaveis: Municipio de Residencia, Municipio do Hospital, Mes, Morte, Sexo
#Dias de Permanencia, Raca/Cor, Idade, Instrucao, Ocupacaoo, Numero de diarias

teste_2 <- dados_teste2 %>% 
  select(MUNIC_RES, MUNIC_MOV, MES_CMPT, MORTE, SEXO, DIAS_PERM, 
         RACA_COR, IDADE, INSTRU, CBOR, QT_DIARIAS) %>% 
  filter(MORTE == "Sim")


#Necessario ainda baixar a base para todos os estados
#Possivelmente adicionar o nome dos municipios, alem do 
#codigo do IBGE

####

#Calculando o excesso de mortalidade para o mes de Maio

dados_t_2018 <- fetch_datasus(year_start = 2018, month_start = 5, year_end = 2018, month_end = 5, information_system = "SIH-RD")
dados_m_2018 <- process_sih(dados_t_2018)

dados_m_2019 <- fetch_datasus(year_start = 2019, month_start = 5, year_end = 2019, month_end = 5, information_system = "SIH-RD")
dados_m_2019 <- process_sih(dados_m_2019)

dados_m_2020 <- fetch_datasus(year_start = 2020, month_start = 5, year_end = 2020, month_end = 5, information_system = "SIH-RD")
dados_m_2020 <- process_sih(dados_m_2020)

d_18 <- dados_m_2018 %>%
  filter(MORTE=="Sim") %>% 
  select(MUNIC_RES, MORTE) %>% 
  group_by(MUNIC_RES) %>% 
  summarise(total_5_18 = n())

d_19 <- dados_m_2019 %>%
  filter(MORTE=="Sim") %>% 
  select(MUNIC_RES, MORTE) %>% 
  group_by(MUNIC_RES) %>% 
  summarise(total_5_19 = n())

d_20 <- dados_m_2020 %>%
  filter(MORTE=="Sim" ) %>%
  mutate(MUNIC_RES = as.integer(MUNIC_RES)) %>% 
  select(MUNIC_RES, MORTE) %>% 
  group_by(MUNIC_RES) %>% 
  summarise(total_5_20 = n())

d_18_19 <- d_18 %>% 
  full_join(d_19,
            by = c("MUNIC_RES" = "MUNIC_RES"))

excesso <- d_18_19 %>% 
  full_join(d_20,
            by = c("MUNIC_RES" = "MUNIC_RES")) %>% 
  mutate(excesso_mortes = total_5_20 / ((total_5_18 + total_5_19)/2))

View(excesso %>% 
  arrange(desc(total_5_20)))

View(excesso)

## Analise da COVID-19 

# Selecione diretorio contendo as bases 

setwd("C:/Users/Pedro Saboia/Desktop/Insper Data")

# Buscando os dados referentes a COVID-19

covid_bruto <- read_excel('HIST_PAINEL_COVIDBR_31ago2020_1.xlsx',
                          col_types = c('text', 'text', 'text','numeric','numeric','numeric',
                                        'text', 'date','numeric','numeric','numeric','numeric',
                                        'numeric','numeric','numeric','numeric','logical'))


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

## Agrupar mortes por regiao de saude 

d_18_rs <- d_18 %>% 
  left_join(dic, by = c('MUNIC_RES' = 'codmun')) %>% 
  group_by(codSaude) %>% 
  summarise(mortes_18 = sum(total_5_18))

d_19_rs <- d_19 %>% 
  left_join(dic, by = c('MUNIC_RES' = 'codmun')) %>% 
  group_by(codSaude) %>% 
  summarise(mortes_19 = sum(total_5_19))

d_20_rs <- d_20 %>% 
  left_join(dic, by = c('MUNIC_RES' = 'codmun')) %>% 
  group_by(codSaude) %>% 
  summarise(mortes_20 = sum(total_5_20))

excesso_rs <- d_18_rs %>% 
  left_join(d_19_rs, by = c('codSaude' = 'codSaude')) %>% 
  left_join(d_20_rs, by = c('codSaude' = 'codSaude')) %>% 
  mutate(excesso_mortes = mortes_20 / ((mortes_18 + mortes_19)/2))

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


## Baixando dados de Maio, Junho e Julho para construÃ§Ã£o da base final


SIH_18 <- fetch_datasus(year_start = 2018, month_start = 5, year_end = 2018, month_end = 7, information_system = "SIH-RD")
SIH_18 <- process_sih(SIH_18)

SIH_19 <- fetch_datasus(year_start = 2019, month_start = 5, year_end = 2019, month_end = 7, information_system = "SIH-RD")
SIH_19 <- process_sih(SIH_19)

SIH_20 <- fetch_datasus(year_start = 2020, month_start = 5, year_end = 2020, month_end = 7, information_system = "SIH-RD")
SIH_20 <- process_sih(SIH_20)


SIH_18 <- SIH_18 %>%   
  filter(MORTE=="Sim") %>% 
  select(MUNIC_RES, MES_CMPT, MORTE, RACA_COR, SEXO, IDADE) %>% 
  mutate(MUNIC_RES = as.integer(MUNIC_RES)) 

SIH_19 <- SIH_19 %>%   
  filter(MORTE=="Sim") %>% 
  select(MUNIC_RES, MES_CMPT, MORTE, RACA_COR, SEXO, IDADE) %>% 
  mutate(MUNIC_RES = as.integer(MUNIC_RES)) 

SIH_20 <- SIH_20 %>%   
  filter(MORTE=="Sim") %>% 
  select(MUNIC_RES, MES_CMPT, MORTE, RACA_COR, SEXO, IDADE) %>% 
  mutate(MUNIC_RES = as.integer(MUNIC_RES)) 

#Adicionando o codigo da regiao de saude

mort_reg_18 <- SIH_18 %>% 
  left_join(dic, by = c("MUNIC_RES" = "codmun")) %>% 
  group_by(MES_CMPT, codSaude) %>% 
  summarise(mortes_18 = n())

mort_reg_19 <- SIH_19 %>% 
  left_join(dic, by = c("MUNIC_RES" = "codmun")) %>% 
  group_by(MES_CMPT, codSaude) %>% 
  summarise(mortes_19 = n())

mort_reg_20 <- SIH_20 %>% 
  left_join(dic, by = c("MUNIC_RES" = "codmun")) %>% 
  group_by(MES_CMPT, codSaude) %>% 
  summarise(mortes_20 = n())

excesso_f <- mort_reg_18 %>% 
  left_join(mort_reg_19, by = c("codSaude" = "codSaude",
                                "MES_CMPT" = "MES_CMPT")) %>% 
  left_join(mort_reg_20, by = c("codSaude" = "codSaude",
                                "MES_CMPT" = "MES_CMPT")) %>% 
  mutate(excesso_mortes = mortes_20 / 
           ((mortes_18 + mortes_19)/2))


#Organizando dados do PIB

getwd()
setwd("C:\\Users\\arthu\\Desktop\\6o Semestre\\InsperData\\COVID\\DiffDiff")

base_PIB <- read_excel("PIB_2010_2017.xlsx")

base_casos_obitos <- read_excel("casos_obitos.xlsx", col_types = c("text", "text", "text", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "numeric"))

###

colnames(base_PIB)

base_PIB <- base_PIB %>% 
  rename(codigo_microrregiao = "Código da Microrregião")

base_PIB <- base_PIB %>% 
  rename(codigo_estado = "Código da Grande Região") %>% 
  rename(estado = "Nome da Grande Região") %>% 
  rename(codigo_UF = "Código da Unidade da Federação") %>% 
  rename(UF = "Sigla da Unidade da Federação") %>% 
  rename(nome_UF = "Nome da Unidade da Federação") %>% 
  rename(codigo_municipio = "Código do Município") %>% 
  rename(nome_municipio = "Nome do Município") %>% 
  rename(regiao_metropolitana = "Região Metropolitana") %>% 
  rename(codigo_mesorregiao = "Código da Mesorregião") %>% 
  rename(nome_mesorregiao = "Nome da Mesorregião") %>% 
  rename(nome_microrregiao =  "Nome da Microrregião") %>% 
  rename(PIB_per_capita = "Produto Interno Bruto per capita, \r\na preços correntes\r\n(R$ 1,00)")

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

data <- excesso_f %>% 
  select(MES_CMPT, codSaude, excesso_mortes) %>%
  left_join(PIB, by = c('codSaude' = 'codigo_microrregiao')) %>% 
  filter(!is.na(PIB_p_capita)) %>% 
  select(-c(Ano, grupo)) %>% 
  mutate(MES_CMPT = as.character(MES_CMPT))


lm(excesso_mortes ~ Riqueza + MES_CMPT, data = data)




