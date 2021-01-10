#===========================================================================================
## Projeto Insper Data - Duque
# Dados CONASS - Municipio
#===========================================================================================

rm(list = ls())

#===========================================================================================
# Selecione o diretorio a ser utilizado
#===========================================================================================

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
library(foreign)
library(fuzzyjoin)
library(geobr)
library(sf)
library(rio)
library(ggthemes)
library(ggalt)
library(gganimate)

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
               ifelse(estado == "Amapa", "AP", 
               ifelse(estado == "Amazonas", "AM",
               ifelse(estado == "Bahia", "BA",
               ifelse(estado == "Ceara", "CE",
               ifelse(estado == "Espirito Santo", "ES",
               ifelse(estado == "Goias", "GO",
               ifelse(estado == "Maranhao", "MA",
               ifelse(estado == "Mato Grosso", "MT",
               ifelse(estado == "Mato Grosso do Sul", "MS",
               ifelse(estado == "Minas Gerais", "MG", 
               ifelse(estado == "Para", "PA",
               ifelse(estado == "Paraiba", "PB", 
               ifelse(estado == "Parana", "PR",
               ifelse(estado == "Pernambuco", "PE",
               ifelse(estado == "Piaui", "PI", 
               ifelse(estado == "Rio de Janeiro", "RJ",
               ifelse(estado == "Rio Grande do Norte", "RN",
               ifelse(estado == "Rio Grande do Sul", "RS",
               ifelse(estado == "Rondonia", "RO",
               ifelse(estado == "Roraima", "RR",
               ifelse(estado == "Santa Catarina", "SC",
               ifelse(estado == "Sao Paulo", "SP",
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
#===========================================================================================


base_PIB <- read_excel("Bases/PIB_2010_2018.xlsx") %>% 
  filter(Ano == 2018) %>%
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
         PIB, codigo_municipio,
         codigo_microrregiao)

base_PIB_1 <- base_PIB %>% 
  separate(codigo_municipio, into = c('codigo_municipio', 'extra'), sep = -1) %>% 
  select(-extra) %>% 
  mutate(codigo_municipio = as.double(codigo_municipio))


#===========================================================================================
## Dados referentes a contaminacao por COVID-19
#===========================================================================================
## Base de dados bruta -> ultima atualizacao: 31/ago/2020
#===========================================================================================



covid_bruto <- read_excel("HIST_PAINEL_COVIDBR_31ago2020_1.xlsx",
                          col_types = c('text', 'text', 'text','numeric','numeric','numeric',
                                        'text', 'date','numeric','numeric','numeric','numeric',
                                        'numeric','numeric','numeric','numeric','logical'))



#===========================================================================================
## Pegar apenas os dados referentes ao final de cada mes
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
## Identificacao de pos e pre evento de interesse por municipio
#===========================================================================================



corona <- covid_bruto %>% 
  mutate(Corona = ifelse(casosAcumulado > 0, 1, 0)) %>% 
  separate(data, into = c('ano', 'mes', 'dia'), sep = "-") %>% 
  separate(mes, into = c('zero', 'mes'), sep = 1) %>% 
  filter(municipio != 0) %>% 
  group_by(mes, estado, codmun) %>% 
  summarise(mean = mean(Corona)) %>% 
  mutate(mes = as.double(mes))



#===========================================================================================
## Populacao acima de 65 anos
#===========================================================================================



age <- read_csv("Bases/POPBR12.csv")

poptotal <- age %>% 
  group_by(MUNIC_RES) %>% 
  summarise(TOTAL = sum(POPULACAO))

mais65_mun <- age %>%
  separate(FXETARIA, into = c('min', 'max'), sep = -2) %>% 
  filter(min >= 65,
         min != 7,
         min != 8,
         min != 9) %>% 
  unite(faixa, min, max, sep = "-") %>% 
  group_by(MUNIC_RES) %>% 
  summarise(POPULACAO = sum(POPULACAO)) %>% 
  left_join(poptotal, by = c('MUNIC_RES' = 'MUNIC_RES')) %>%
  mutate(mais65 = POPULACAO / TOTAL) %>% 
  unique()



#===========================================================================================
## Dados do CNES
#===========================================================================================



cnes <- read_excel("Bases/CNES_Final.xlsx")

cnes <- cnes %>%
  mutate(across(names(cnes)[3:19], as.double)) %>%
  mutate_all(funs(replace_na(., 0))) %>% 
  separate(Municipio, into = c("mun_cod", "mun_nome"), sep = 6) %>% 
  separate(mun_nome, into = c("mun_nome", "cod"), sep = "-") %>% 
  filter(is.na(cod)) %>% 
  select(-cod) %>% 
  rename(mes = Mes,
        leitos_amb_ped = "Leito_Amb_Rep/Obs_Ped",
        leitos_amb_indif = "Leito_Amb_Rep/Obs_Indif",
        leitos_amb_fem = "Leito_Amb_Rep/Obs_Fem",
        leitos_amb_masc = "Leito_Amb_Rep/Obs_Masc",
        leitos_urg_ped = "Leito_Urg_Rep/Obs_Ped",
        leitos_urg_indif = "Leito_Urg_Rep/Obs_Indif",
        leitos_urg_fem = "Leito_Urg_Rep/Obs_Fem",
        leitos_urg_masc = "Leito_Urg_Rep/Obs_Masc",
        Leito_Complementar_Total = 'Leito_Complementar_existente',
        Estab_c_Equip_SUS = 'Estab_c/_Equip_SUS') %>% 
  mutate(Leito_Amb_Total = leitos_amb_ped + leitos_amb_indif + leitos_amb_fem + leitos_amb_masc,
         Leito_Urg_Total = leitos_urg_ped + leitos_urg_indif + leitos_urg_fem + leitos_urg_masc,
         mun_cod = as.double(mun_cod)) %>% 
  select(-c(Leito_Int_SUS:leitos_amb_indif,Leito_Complementar_SUS:leitos_urg_indif))



#===========================================================================================
## Dados sobre mortalidade excessiva
#===========================================================================================
## Juntando as bases de mortalidade por mes
#===========================================================================================


conass_1 <- base_PIB_1 %>% 
  left_join(conas_18 %>% filter(mes == 1), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 1) %>% 
  left_join(conas_19 %>% filter(mes == 1), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  left_join(conas_20 %>% filter(mes == 1), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))

conass_2 <- base_PIB_1 %>% 
  left_join(conas_18 %>% filter(mes == 2), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 2) %>% 
  left_join(conas_19 %>% filter(mes == 2), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  left_join(conas_20 %>% filter(mes == 2), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))

conass_3 <- base_PIB_1 %>% 
  left_join(conas_18 %>% filter(mes == 3), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 3) %>% 
  left_join(conas_19 %>% filter(mes == 3), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  left_join(conas_20 %>% filter(mes == 3), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))

conass_4 <- base_PIB_1 %>% 
  left_join(conas_18 %>% filter(mes == 4), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 4) %>% 
  left_join(conas_19 %>% filter(mes == 4), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  left_join(conas_20 %>% filter(mes == 4), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))

conass_5 <- base_PIB_1 %>% 
  left_join(conas_18 %>% filter(mes == 5), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 5) %>% 
  left_join(conas_19 %>% filter(mes == 5), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  left_join(conas_20 %>% filter(mes == 5), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))

conass_6 <- base_PIB_1 %>% 
  left_join(conas_18 %>% filter(mes == 6), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 6) %>% 
  left_join(conas_19 %>% filter(mes == 6), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  left_join(conas_20 %>% filter(mes == 6), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))

conass_7 <- base_PIB_1 %>% 
  left_join(conas_18 %>% filter(mes == 7), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 7) %>% 
  left_join(conas_19 %>% filter(mes == 7), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  left_join(conas_20 %>% filter(mes == 7), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))

conass_8 <- base_PIB_1 %>% 
  left_join(conas_18 %>% filter(mes == 8), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 8) %>% 
  left_join(conas_19 %>% filter(mes == 8), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  left_join(conas_20 %>% filter(mes == 8), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))

conass_9 <- base_PIB_1 %>% 
  left_join(conas_18 %>% filter(mes == 9), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 9) %>% 
  left_join(conas_19 %>% filter(mes == 9), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  left_join(conas_20 %>% filter(mes == 9), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))

conas_final <- rbind(conass_1, conass_2, conass_3, conass_4, conass_5, conass_6, conass_7, conass_8,
      conass_9) %>% 
  mutate(obitos_18 = replace_na(obitos_18, 0),
         obitos_19 = replace_na(obitos_19, 0),
         obitos_20 = replace_na(obitos_20, 0)) 

conas_final <- conas_final %>% 
  filter(nome_municipio != "Alegre" | obitos_20 != 31) 

conas_final <- conas_final %>% 
  arrange(UF, mes, nome_municipio) %>% 
  select(UF, mes, nome_municipio, obitos_18, obitos_19, obitos_20, regiao, nome_UF,
         nome_mesorregiao, nome_microrregiao, PIB, codigo_municipio, codigo_microrregiao)



# Essa base tem um problema. Estamos fazendo a junção da base pelo nome dos municípios.
# O ideal seria fazer o join pelo código do muniípio, porém esse código não consta nos dados
# do CONASS. Então, fizemos o join pelo nome do município e pela UF. No entanto, alguns
# municípios tem o nome escrito de forma diferente na base do CONASS e na base do IBGE. 
# O R gera, portanto, uma série de valores ausentes que não deveriam ser gerados. Para arrumar
# isso, concertamos manualmente aqueles municípios que estavam com um valor ausente de maneira
# indevida.


# Base para conferir os óbitos de cada município que estava dando problema:


conass_1_teste <- base_PIB_1 %>% 
  full_join(conas_18 %>% filter(mes == 1), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 1) %>% 
  full_join(conas_19 %>% filter(mes == 1), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  full_join(conas_20 %>% filter(mes == 1), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))

conass_2_teste <- base_PIB_1 %>% 
  full_join(conas_18 %>% filter(mes == 2), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 2) %>% 
  full_join(conas_19 %>% filter(mes == 2), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  full_join(conas_20 %>% filter(mes == 2), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))

conass_3_teste <- base_PIB_1 %>% 
  full_join(conas_18 %>% filter(mes == 3), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 3) %>% 
  full_join(conas_19 %>% filter(mes == 3), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  full_join(conas_20 %>% filter(mes == 3), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))

conass_4_teste <- base_PIB_1 %>% 
  full_join(conas_18 %>% filter(mes == 4), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 4) %>% 
  full_join(conas_19 %>% filter(mes == 4), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  full_join(conas_20 %>% filter(mes == 4), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))

conass_5_teste <- base_PIB_1 %>% 
  full_join(conas_18 %>% filter(mes == 5), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 5) %>% 
  full_join(conas_19 %>% filter(mes == 5), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  full_join(conas_20 %>% filter(mes == 5), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))

conass_6_teste <- base_PIB_1 %>% 
  full_join(conas_18 %>% filter(mes == 6), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 6) %>% 
  full_join(conas_19 %>% filter(mes == 6), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  full_join(conas_20 %>% filter(mes == 6), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))

conass_7_teste <- base_PIB_1 %>% 
  full_join(conas_18 %>% filter(mes == 7), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 7) %>% 
  full_join(conas_19 %>% filter(mes == 7), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  full_join(conas_20 %>% filter(mes == 7), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))

conass_8_teste <- base_PIB_1 %>% 
  full_join(conas_18 %>% filter(mes == 8), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 8) %>% 
  full_join(conas_19 %>% filter(mes == 8), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  full_join(conas_20 %>% filter(mes == 8), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))

conass_9_teste <- base_PIB_1 %>% 
  full_join(conas_18 %>% filter(mes == 9), by = c("nome_municipio" = "municipio", "UF" = "UF")) %>% 
  mutate(mes = 9) %>% 
  full_join(conas_19 %>% filter(mes == 9), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes")) %>% 
  full_join(conas_20 %>% filter(mes == 9), by = c("nome_municipio" = "municipio", "UF" = "UF", "mes" = "mes"))



conas_teste_NA <- rbind(conass_1_teste, conass_2_teste, conass_3_teste, conass_4_teste, 
                        conass_5_teste, conass_6_teste, conass_7_teste, conass_8_teste,
                        conass_9_teste) %>% 
  filter(nome_municipio != "Alegre" | obitos_20 != 31) %>% 
  filter(is.na(regiao)) %>% 
  arrange(UF, mes, nome_municipio) %>% 
  select(UF, mes, nome_municipio, obitos_18, obitos_19, obitos_20)

# Essa base contém apenas os municípios o R não conseguiu fazer o join. Passamos essa 
# base para o excel junto com a base que tem todos os municípios. Lá, comparamos as duas
# bases e preenchemos os dados de mortalidade manualmente.

write.xlsx(conas_teste_NA, "CONASS_Ausentes.xlsx")
write.xlsx(conas_final, "CONASS_Final.xlsx")

# Chamando de volta a base concertada:

CONASS_final <- read_excel("CONASS_Final.xlsx")

#===========================================================================================
## Fazendo a base com a medida de excesso de mortalidade adaptada
#===========================================================================================


# Criando a medida de mortalidade excessiva:

CONASS_municipio <- CONASS_final %>%
  mutate(excesso_conass = (1 + obitos_20) / ( 1 + (obitos_18 + obitos_19) / 2),
         mes = as.character(mes)) 

# Não temos nenhum NA no excesso de mortalidade:

CONASS_municipio <- CONASS_municipio %>%
  mutate(codigo_municipio = as.double(codigo_municipio)) %>% 
  left_join(covid_mensal, by = c('codigo_municipio' = 'codmun', 'nome_municipio' = 'municipio')) %>%
  select(-c(mes.y, casosAcumulado)) %>% 
  unique() 

# Filtrando municípios que não tem informação de população:

CONASS_municipio <- CONASS_municipio %>% 
  filter(!is.na(populacao))

# Nesse ponto, temos 5552 municípios com informações de todas as variáveis

CONASS_municipio <- CONASS_municipio %>% 
  mutate(PIB_per_capita = PIB / populacao) %>% 
  filter(!is.na(PIB_per_capita),
         excesso_conass != Inf) %>%
  filter(!is.na(PIB_per_capita)) %>%
  rename(mes = mes.x) 

# Juntando com outras variáveis relevantes

CONASS_municipio <- CONASS_municipio %>% 
  mutate(mes = as.double(mes)) %>% 
  left_join(mais65_mun, by = c('codigo_municipio' = 'MUNIC_RES')) %>%
  left_join(corona, by = c('codigo_municipio' = 'codmun', 
                           'UF' = 'estado',
                           'mes' = 'mes')) %>%
  mutate(corona = ifelse(mean > 0.5, 1, 0),
         corona = replace_na(corona, 0)) %>%
  select(mes, codigo_municipio, nome_municipio, nome_microrregiao, 
         UF, excesso_conass, populacao, metropolitana, 
         PIB_per_capita, mais65, corona, regiao)

# Filtramos valores de mais65 ausentes

CONASS_municipio <- CONASS_municipio %>% 
  filter(!is.na(mais65))
  
# Temos agora uma amostra de 5547 municípios. O próximo passo é juntar os dados provenientes do CNES.

CONASS_municipio_cnes <- CONASS_municipio  %>% 
  left_join(cnes, by = c('codigo_municipio' = 'mun_cod', 'mes' = 'mes')) %>% 
  select(-mun_nome) %>% 
  mutate(Leito_Int_Relat = Leito_Int_Total / populacao,
         Leito_Total = Leito_Int_Total + Leito_Complementar_Total + Leito_Amb_Total + Leito_Urg_Total,
         Leito_Relat = (Leito_Total / populacao) * 1000) %>% 
  filter(!is.na(Leito_Relat))

# Perdemos 26 municípios que não tinham dados de leitos. 
# Por fim, com a amostra final de 5519 municípios, calculamos a medida de renda relativa pelos tercis
# do PIB per capita.

CONASS_municipio_cnes <- CONASS_municipio_cnes %>% 
  mutate(wealth = ifelse(PIB_per_capita >= quantile(PIB_per_capita , p = c(2/3)), 'RICO',
                       ifelse(PIB_per_capita >= quantile(PIB_per_capita, p = c(1/3)), 'MEDIO', 'POBRE')))


# Filtramos alguns outliers que estavam com um excesso muito acima da média
CONASS_municipio_cnes_1 <- CONASS_municipio_cnes %>% 
  filter(codigo_municipio != 211220) %>% 
  filter(codigo_municipio != 230410) %>% 
  filter(codigo_municipio != 230960)


write.dta(CONASS_municipio_cnes, "CONASS_municipio_cnes.dta")
write.xlsx(CONASS_municipio_cnes, "CONASS_municipio_cnes.xlsx")


#===========================================================================================
## Gráficos
#===========================================================================================

# Gráfico de quando a pandemia 'começou'
covid_bruto %>% 
  mutate(Corona = ifelse(casosAcumulado > 0, 1, 0)) %>% 
  filter(municipio != 0) %>%
  group_by(data) %>% 
  summarise(corona = sum(Corona)) %>% 
  ggplot() + 
  geom_area(aes(x = data, y = corona), fill = 'red', alpha = 0.2) +
  geom_line(aes(x = data, y = corona), color = 'red', size = 2) + 
  theme_classic() +
  xlab('Data') +
  ylab('Municípios com casos') #+
labs(title = 'Número de municípios com casos ao longo do ano de 2020')

# Mortes x PIB per capita
CONASS_municipio_cnes %>%
  group_by(codigo_municipio, regiao) %>% 
  summarise(mean = mean(excesso_conass),
            PIB = PIB_per_capita) %>%
  unique() %>%
  #filter(mean < 30) %>% 
  ggplot() + 
  geom_point(aes(x = log(PIB), y = mean, color = regiao)) + 
  xlab('Log do PIB per capita') +
  ylab('Mortalidade excessiva média') + 
  labs(title = 'Mortalidade excessiva média com base na renda per capita',
       color = 'Região') +
  theme_classic() #+  
theme(legend.position = 'null')


# Gráfico do Excesso de Mortalidade ao longo do ano de 2020

CONASS_municipio_cnes %>%
  group_by(wealth, mes) %>% 
  summarise(excesso = mean(excesso_conass),
            sd = sd(excesso_conass)) %>%
  mutate(wealth = ifelse(wealth == 'RICO', 'aa.Rico',
                         ifelse(wealth == 'POBRE', 'cc.Pobre',
                                ifelse(wealth == 'MEDIO', 'bb.Médio', NA)))) %>%
  ggplot() + 
  geom_line(aes(as.factor(mes), excesso, group = wealth, color = wealth), size = 2) +
  theme_classic() +
  ylab("Excesso de mortalidade relativo") + 
  xlab("Mês") +
  scale_colour_manual(name = "Nível de Renda", 
                      label = c('Rico', 'Médio', 'Pobre'),
                      values = c('#0059A1','#39A100', '#FFEA54')) +
  theme(legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.position = 'right')

# Pré e pós 
CONASS_municipio_cnes %>% 
  group_by(wealth, corona) %>% 
  summarise(death = mean(excesso_conass)) %>%
  mutate(corona = as.factor(corona)) %>%
  pivot_wider(names_from = corona, values_from = death) %>%
  mutate(wealth = ifelse(wealth == 'RICO', 'cc.Rico',
                         ifelse(wealth == 'POBRE', 'aa.Pobre',
                                ifelse(wealth == 'MEDIO', 'bb.Médio', NA)))) %>% 
  ggplot() +
  geom_dumbbell(aes(y = wealth, x = `0`, xend = `1`),
                size = 1.5, size_x = 3, size_xend = 3,
                color = 'gray', colour_x = 'blue', colour_xend = 'red') +
  ylab('') +
  xlab('Mortes Relativas') +
  scale_color_manual(values = c('blue', 'red')) +
  scale_y_discrete(label = c('Pobre', 'Médio', 'Rico')) +
  labs(title = 'Mortes relativas antes e depois do inicio da pandemia com base no nível de renda',
       subtitle = 'para municípios') +
  theme_classic()

#Pré e pós 2
CONASS_municipio_cnes %>% 
  group_by(wealth, corona) %>% 
  summarise(death = mean(excesso_conass)) %>%
  mutate(corona = as.factor(corona)) %>%
  mutate(wealth = ifelse(wealth == 'RICO', 'aa.Rico',
                         ifelse(wealth == 'POBRE', 'cc.Pobre',
                                ifelse(wealth == 'MEDIO', 'bb.Médio', NA)))) %>%
  ggplot(aes(x = corona, y = death, group = wealth, colour = wealth)) +
  geom_line() +
  geom_point(size = 3) +
  ylab('Mortes excessivas') +
  xlab('Pré x pós o 1º caso') +
  labs(title = 'Impacto da pandemia do coronavírus no excesso de mortes',
       subtitle = 'em nível municipal', 
       colour = 'Riqueza',
       caption = 'Fonte: CONASS e Ministério da Saúde') +
  scale_colour_discrete(name = "Riqueza", label = c('Rico',
                                                    'Médio',
                                                    'Pobre')) +
  theme_classic()

#===========================================================================================
## Mapa
#===========================================================================================

# Base com a função de dados geograficos dos municipios
mun <- read_municipality(code_muni = "all", year = 2019)

# Mapa do nível de riqueza dos municipios

mun <- mun %>%
  separate(code_muni, into = c('code_muni', 'extra'), sep = -1) %>%
  mutate(code_muni = as.double(code_muni)) %>% 
  left_join(CONASS_municipio_cnes %>% filter(mes == 1), by = c('code_muni'='codigo_municipio')) %>% 
  filter(!is.na(wealth))

mun %>% mutate(wealth = ifelse(wealth == 'RICO', 'aa.Rico',
                               ifelse(wealth == 'POBRE', 'cc.Pobre',
                                      ifelse(wealth == 'MEDIO', 'bb.Médio', NA)))) %>% 
  ggplot() +
  geom_sf(aes(fill = wealth), color = NA, size = .20) +
  scale_fill_manual(name = 'Riqueza',label = c('Rico',
                                               'Médio',
                                               'Pobre'),
                    values = c('#0059A1','#39A100', '#FFEA54')) +
  theme_map() +
  theme(legend.key.size = unit(1, 'cm'),
        legend.spacing.y = unit(0.5, 'cm'))

# Mapa se o municipio é urbano ou rural 
mun %>% 
  ggplot() +
  geom_sf(aes(fill = metropolitana), color = NA, size = .50) +
  labs(title = 'Municipios brasileiros considerados parte de regiões metropolitanas') +
  scale_fill_discrete(name = 'É região metropolitana:',
                      label = c('Não', 'Sim')) +
  theme_minimal()



