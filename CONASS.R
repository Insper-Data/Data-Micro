#===========================================================================================
## Projeto Insper Data - Duque
# Dados CONASS
#===========================================================================================

rm(list = ls())

#===========================================================================================
# Selecione o dirtorio a ser utilizado
#===========================================================================================

setwd('C:\\Users\\arthu\\Desktop\\InsperData\\COVID\\DiffDiff\\Data-Micro')
#setwd('C:/Users/Pedro Saboia/Desktop/Insper Data')

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
#===========================================================================================

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
         PIB, codigo_municipio,
         codigo_microrregiao)

#===========================================================================================
## Dados referentes a contaminacao por COVID-19
#===========================================================================================
## Base de dados bruta -> ultima atualizacao: 31/ago/2020
#===========================================================================================

cnes <- read_excel("CNES_Final.xlsx") 

covid_bruto <- read_excel("HIST_PAINEL_COVIDBR_31ago2020_1.xlsx",
                          col_types = c('text', 'text', 'text','numeric','numeric','numeric',
                                        'text', 'date','numeric','numeric','numeric','numeric',
                                        'numeric','numeric','numeric','numeric','logical'))
cnes <- cnes %>%
  mutate(across(names(cnes)[3:19], as.double)) %>%
  mutate_all(funs(replace_na(., 0))) %>% 
  separate(Municipio, into = c("mun_cod", "mun_nome"), sep = 6) %>% 
  separate(mun_nome, into = c("mun_nome", "cod"), sep = "-") %>% 
  filter(is.na(cod)) %>% 
  select(-cod)

CNES <- base_PIB %>% 
  separate(codigo_municipio, into = c("codigo_municipio", "extra"), sep = -1) %>% 
  left_join(cnes, by = c("codigo_municipio" = "mun_cod")) %>% 
  select(-c(extra, mun_nome)) %>% 
  rename(mes = Mes,
         leitos_amb_ped = "Leito_Amb_Rep/Obs_Ped",
         leitos_amb_indif = "Leito_Amb_Rep/Obs_Indif",
         leitos_amb_fem = "Leito_Amb_Rep/Obs_Fem",
         leitos_amb_masc = "Leito_Amb_Rep/Obs_Masc",
         leitos_urg_ped = "Leito_Urg_Rep/Obs_Ped",
         leitos_urg_indif = "Leito_Urg_Rep/Obs_Indif",
         leitos_urg_fem = "Leito_Urg_Rep/Obs_Fem",
         leitos_urg_masc = "Leito_Urg_Rep/Obs_Masc") %>% 
  mutate(leitos_amb_total = leitos_amb_ped + leitos_amb_indif + leitos_amb_fem + leitos_amb_masc,
         leitos_urg_total = leitos_urg_ped + leitos_urg_indif + leitos_urg_fem + leitos_urg_masc) %>% 
  filter(!is.na(mes)) %>% 
  group_by(codigo_microrregiao, nome_microrregiao, mes) %>% 
  summarise(leito_int_total = sum(Leito_Int_Total),
            leito_amb_total = sum(leitos_amb_total),
            leito_urg_total = sum(leitos_urg_total),
            equipamentos_existentes = sum(Equipamentos_Existentes),
            equipamentos_uso = sum(Equipamentos_em_Uso),
            leito_comp_SUS = sum(Leito_Complementar_SUS),
            leito_comp_nao_SUS = sum(Leito_Complementar_Nao_SUS))

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

#===========================================================================================
## Dados sobre mortalidade excessiva
#===========================================================================================
## Limpando as bases
#===========================================================================================

conass <- conas_18 %>% 
  full_join(conas_19, by = c('municipio' = 'municipio', 'mes' = 'mes', 'UF' = 'UF')) %>% 
  full_join(conas_20, by = c('municipio' = 'municipio', 'mes' = 'mes', 'UF' = 'UF')) %>% 
  replace_na(list(obitos_18 = 0, obitos_19 = 0, obitos_20 = 0))

mortes <- conass %>%  
  left_join(base_PIB, by = c('municipio' = 'nome_municipio', 'UF' = 'UF')) %>%
  group_by(nome_microrregiao, mes, UF) %>% 
  summarise(obitos_18 = sum(obitos_18),
            obitos_19 = sum(obitos_19),
            obitos_20 = sum(obitos_20)) %>% 
  mutate(excesso_conass = obitos_20 / ((obitos_18 + obitos_19) / 2)) %>% 
  mutate(excesso_conass_2 = excesso_conass -1) %>% 
  select(-c(obitos_18:obitos_20)) %>% 
  filter(!is.na(nome_microrregiao))

#===========================================================================================
## Arrumando a base
#===========================================================================================

controles <- covid_mensal %>% 
  left_join(base_PIB, by = c('municipio' = 'nome_municipio')) %>% 
  group_by(nome_microrregiao, UF, regiao) %>%
  summarise(populacao = sum(populacao),
            PIB = sum(PIB),
            interior = sum(pop_interior),
            urbana = sum(pop_urbana)) %>%
  mutate(PIB_per_capita = PIB / populacao,
         porcentagem_rural = interior/populacao,
         porcentagem_urbana = urbana/populacao) %>%
  filter(!is.na(nome_microrregiao),
         !is.na(PIB_per_capita)) %>%
  arrange(desc(PIB_per_capita)) %>%
  mutate(wealth = ifelse(PIB_per_capita >= 27.364441, 'RICO',
                         ifelse(PIB_per_capita >= 14.771092, 'MEDIO', 'POBRE'))) %>%
  left_join(mais65, by = c('nome_microrregiao' = 'nome_microrregiao')) %>% 
  select(nome_microrregiao, UF, regiao, populacao, wealth, mais65, porcentagem_rural, porcentagem_urbana) %>% 
  rename(p_rural = porcentagem_rural, p_urbana = porcentagem_urbana)

#===========================================================================================
## Base Final
#===========================================================================================

CONASS <- mortes %>% 
  left_join(controles,  by = c('nome_microrregiao' = 'nome_microrregiao', "UF" = "UF")) %>% 
  filter(!is.na(regiao),
         excesso_conass != "Inf") %>% 
  mutate(corona = ifelse(mes > 3, 1, 0)) %>% 
  left_join(CNES, by = c("nome_microrregiao" = "nome_microrregiao", "mes" = "mes"))


#===========================================================================================
## Graficos
#===========================================================================================

CONASS %>%
  group_by(wealth, mes) %>% 
  summarise(excesso = mean(excesso_conass),
            sd = sd(excesso_conass)) %>%
  ggplot() + 
  geom_line(aes(mes, excesso, group = wealth, color = wealth), size = 2) +
  theme_classic() +
  ylab("Excesso de mortalidade relativo") + 
  xlab("Mês") +
  scale_colour_discrete(name = "Nível de PIB per capita", labels = c("Alto","Médio", "Baixo")) +
  labs(color = "Nível de Renda",
       title = "Evolução do excesso de mortalidade em 2020",
       subtitle = "por microrregião",
       caption = "Fonte: MicroDataSUS e IBGE")

#===========================================================================================
## Regressao CONASS
#===========================================================================================

summary(lm(excesso_conass_2 ~ wealth*corona + wealth + corona + mais65 + populacao + p_urbana + p_rural + UF + mes , data = CONASS))

summary(lm(excesso_conass_2 ~ wealth*corona + wealth + leito_int_total + leito_amb_total + leito_urg_total + equipamentos_uso + corona + mais65 + populacao + p_urbana + p_rural + regiao + mes, data = CONASS))

#===========================================================================================
## Salvando os dados
#===========================================================================================

write.xlsx(CONASS, "CONASS.xlsx")



