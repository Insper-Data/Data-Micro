#Projeto Insper Data - Duque

#Pacotes utilizados

library(dplyr)
library(tidyverse)
library(microdatasus)
library(skimr)
library(readxl)


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

#agrupando as mortes no RJ por municupio e por mes

dados %>% 
  filter(MORTE=="Sim") %>% 
  group_by(munResNome,MES_CMPT) %>% 
  summarise(total=n()) %>% 
  arrange(desc(total))

#pegando dados do Brasil inteiro para os meses de marco e abril de 2018

dados_teste <- fetch_datasus(year_start = 2018, month_start = 3, year_end = 2018, month_end = 4, information_system = "SIH-RD")
dados_teste <- process_sih(dados_teste)

skim(dados_teste)

#MUNIC_RES indica o cÃ³digo do municÃ­pio de residÃªncia do entrante
#A base com PIBs per capita contÃ©m os mesmo cÃ³digos, porÃ©m com um dÃ­gito a mais

dados_teste1 <- dados_teste %>% 
  filter(MORTE==1) %>% 
  select(ANO_CMPT,MES_CMPT,RACA_COR,SEXO,UF_ZI,MUNIC_RES) %>% 
  group_by(MUNIC_RES,MES_CMPT) %>% 
  summarise(total=n()) %>% 
  arrange(MES_CMPT,MUNIC_RES)

#2020 - Sao Paulo

dados_teste2 <- fetch_datasus(year_start = 2020, month_start = 3, year_end = 2020, month_end = 4,uf="SP", information_system = "SIH-RD")
dados_teste2 <- process_sih(dados_teste2)

#Variaveis: Municipio de Residencia, Municipio do Hospital, Mes, Morte, Sexo
#Dias de Permanencia, Raca/Cor, Idade, Instrucao, Ocupacaoo, Numero de diarias

teste_2 <- dados_teste2 %>% 
  select(MUNIC_RES, MUNIC_MOV, MES_CMPT, MORTE, SEXO, DIAS_PERM, 
         RACA_COR, IDADE, INSTRU, CBOR, QT_DIARIAS) %>% 
  filter(MORTE == "Sim")

teste_2 %>% 
  group_by(MUNIC_RES, MES_CMPT) %>% 
  summarise(total = n()) %>% 
  arrange(MES_CMPT, desc(total))

teste_2 %>% 
  group_by(MUNIC_RES) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total))

#Necessario ainda baixar a base para todos os estados
#Possivelmente adicionar o nome dos municipios, alem do 
#codigo do IBGE

####

#Base de dados de obitos
install.packages("devtools")
devtools::install_github("rfsaldanha/microdatasus")

dados_2018 <- fetch_datasus(year_start = 2018, year_end = 2018, uf = "RJ", information_system = "SIM-DO")
dados_2018 <- process_sim(dados_2018)


dados_2018 %>% 
  select(munResNome,CAUSABAS,DTOBITO)

#Seria preciso separar as datas por dia, mes e ano

dados_2018 %>% 
  group_by(munResNome) %>% 
  summarise(total=n()) %>% 
  arrange(desc(total))

#Sao Paulo - 2018
#Parece que no e possivel acessar os dados para 2019 e 2020

dados_teste3 <- fetch_datasus(year_start = 2018, year_end = 2018, uf = "SP", information_system = "SIM-DO")
dados_teste3 <- process_sim(dados_teste3)

teste_3 <- dados_teste3 %>% 
  filter(TIPOBITO != 2) %>% 
  select(DTOBITO, munResNome, IDADEanos, SEXO,
         RACACOR, ESC, ESCMAE, OCUP, CAUSABAS)

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

dic <- covid_mensal %>% 
  group_by(codmun) %>% 
  summarise(codSaude = mean(codRegiaoSaude),
            nome_regiao = nomeRegiaoSaude,
            estado = estado) %>% 
  unique()

## Agrupar mortes por região de saúde 

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
  
## Criacao do indice de contaminaçao   
  
covid_m_rs <- covid_m_rs %>% 
  group_by(data) %>%
  mutate(index = ((Casos_Acumulados - min(Casos_Acumulados))/(max(Casos_Acumulados) - min(Casos_Acumulados))) * 100)
  
## Populacao 


pop <- read_excel('ESTIMA_PO.2019-2019.xls', col_names = c('cod', 'Mun', 'populacao'),
                  col_types = c('text', 'text', 'numeric')) %>% 
  select(-cod) %>% 
  filter(Mun != 'Municípios')

ibge <- read_excel('RELATORIO_DTB_BRASIL_MUNICIPIO.XLS', col_types = 'text') %>%  
  select(c('Código Município Completo', 'Nome_Município')) %>% 
  separate('Código Município Completo', into = c('idmun', 'extra'), sep = -1) %>% 
  select(-extra)

popul <- ibge %>% 
  left_join(pop, by = c('Nome_Município' = 'Mun')) %>%
  filter(!is.na(populacao)) %>%
  mutate(idmun = as.double(idmun)) %>% 
  left_join(dic, by = c('idmun' = 'codmun')) %>%
  group_by(codSaude, nome_regiao, estado) %>%
  summarise(populacao_total = sum(populacao))

