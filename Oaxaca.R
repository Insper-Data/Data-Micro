#### Projeto Insper Data - Duque - Oaxaca Blinder                      ###
##########################################################################

rm(list = ls())

##########################################################################
### Selecione o dirtorio a ser utilizado                               ###
##########################################################################

setwd('C:\\Users\\arthu\\Desktop\\InsperData\\COVID\\Oaxaca')

##########################################################################
### Pacotes utilizados                                                 ###
##########################################################################

library(dplyr)
library(tidyverse)
library(microdatasus)
library(skimr)
library(readxl)
library(lubridate)
library(broom)
library(openxlsx)
library(oaxaca)

##########################################################################
### Coletando dados da PNAD COVID                                      ###
##########################################################################

pnad05 <- read.csv("PNAD_COVID_052020.csv")

pnad06 <- read.csv("PNAD_COVID_062020.csv")

pnad07 <- read.csv("PNAD_COVID_072020.csv")

pnad08 <- read.csv("PNAD_COVID_082020.csv")

pnad09 <- read.csv("PNAD_COVID_092020.csv")

##########################################################################
### Coletando dados do CNES                                            ###
##########################################################################

cnes <- read_excel("CNES_Final.xlsx") 

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
  select(regiao, UF, 
         nome_municipio,
         nome_microrregiao, 
         codigo_microrregiao,
         codigo_municipio)

##########################################################################
### Filtrando as bases                                                 ###
##########################################################################

cnes <- cnes %>%
  mutate(across(names(cnes)[3:19], as.double)) %>%
  mutate_all(funs(replace_na(., 0))) %>% 
  separate(Municipio, into = c("mun_cod", "mun_nome"), sep = 6) %>% 
  separate(mun_nome, into = c("mun_nome", "cod"), sep = "-") %>% 
  filter(is.na(cod)) %>% 
  select(-cod)

pnad5 <- pnad05 %>%
  select(Ano, UF, V1013, Estrato, UPA, V1022, V1023, A002, A003, A004, A005, B0011, B0012, 
         B0013, B0014, B0015, B0016, B0017, B0018, B0019, B00110, B00111, B00112, 
         C007B, C007C, C007D, C008, C010, C01011, C01012, C013, C014, D0011, D0013,
         D0031, D0033, D0051, D0053) %>%
  rename(ano = Ano, mes = V1013, ua = V1022, RM = V1023, idade = A002, sexo = A003, raca = A004, 
         escolaridade = A005, febre = B0011, tosse = B0012, dorgarganta = B0013,
         dificu_respiratoria = B0014, dorcabeca = B0015, dorpeito = B0016, nausea = B0017,
         narizentupido = B0018, fadiga = B0019, dorolhos = B00110,
         perdeucheiro = B00111, dormuscular = B00112, carteira = C007B, tipo_trabalho = C007C,
         tipo_setor = C007D, horas_trabalho = C008, recebia = C010, quanto_recebia = C01011,
         valor_recebia = C01012, home_office = C013, INSS = C014, aposentado = D0011,
         quanto_aposentado = D0013, bolsa_familia = D0031, quanto_bf = D0033, 
         auxilio_emergencial = D0051, quanto_ae = D0053)

pnad6 <- pnad06 %>%
  select(Ano, UF, V1013, Estrato, UPA, V1022, V1023, A002, A003, A004, A005, B0011, B0012, 
         B0013, B0014, B0015, B0016, B0017, B0018, B0019, B00110, B00111, B00112, 
         C007B, C007C, C007D, C008, C010, C01011, C01012, C013, C014, D0011, D0013,
         D0031, D0033, D0051, D0053) %>%
  rename(ano = Ano, mes = V1013, ua = V1022, RM = V1023, idade = A002, sexo = A003, raca = A004, 
         escolaridade = A005, febre = B0011, tosse = B0012, dorgarganta = B0013,
         dificu_respiratoria = B0014, dorcabeca = B0015, dorpeito = B0016, nausea = B0017,
         narizentupido = B0018, fadiga = B0019, dorolhos = B00110,
         perdeucheiro = B00111, dormuscular = B00112, carteira = C007B, tipo_trabalho = C007C,
         tipo_setor = C007D, horas_trabalho = C008, recebia = C010, quanto_recebia = C01011,
         valor_recebia = C01012, home_office = C013, INSS = C014, aposentado = D0011,
         quanto_aposentado = D0013, bolsa_familia = D0031, quanto_bf = D0033, 
         auxilio_emergencial = D0051, quanto_ae = D0053)

pnad7 <- pnad07 %>% 
  select(Ano, UF, V1013, Estrato, UPA, V1022, V1023, A002, A003, A004, A005, B0011, B0012, 
         B0013, B0014, B0015, B0016, B0017, B0018, B0019, B00110, B00111, B00112, 
         C007B, C007C, C007D, C008, C010, C01011, C01012, C013, C014, D0011, D0013,
         D0031, D0033, D0051, D0053) %>%
  rename(ano = Ano, mes = V1013, ua = V1022, RM = V1023, idade = A002, sexo = A003, raca = A004, 
         escolaridade = A005, febre = B0011, tosse = B0012, dorgarganta = B0013,
         dificu_respiratoria = B0014, dorcabeca = B0015, dorpeito = B0016, nausea = B0017,
         narizentupido = B0018, fadiga = B0019, dorolhos = B00110,
         perdeucheiro = B00111, dormuscular = B00112, carteira = C007B, tipo_trabalho = C007C,
         tipo_setor = C007D, horas_trabalho = C008, recebia = C010, quanto_recebia = C01011,
         valor_recebia = C01012, home_office = C013, INSS = C014, aposentado = D0011,
         quanto_aposentado = D0013, bolsa_familia = D0031, quanto_bf = D0033, 
         auxilio_emergencial = D0051, quanto_ae = D0053)

pnad8 <- pnad08 %>%
  select(Ano, UF, V1013, Estrato, UPA, V1022, V1023, A002, A003, A004, A005, B0011, B0012, 
         B0013, B0014, B0015, B0016, B0017, B0018, B0019, B00110, B00111, B00112, 
         C007B, C007C, C007D, C008, C010, C01011, C01012, C013, C014, D0011, D0013,
         D0031, D0033, D0051, D0053) %>%
  rename(ano = Ano, mes = V1013, ua = V1022, RM = V1023, idade = A002, sexo = A003, raca = A004, 
         escolaridade = A005, febre = B0011, tosse = B0012, dorgarganta = B0013,
         dificu_respiratoria = B0014, dorcabeca = B0015, dorpeito = B0016, nausea = B0017,
         narizentupido = B0018, fadiga = B0019, dorolhos = B00110,
         perdeucheiro = B00111, dormuscular = B00112, carteira = C007B, tipo_trabalho = C007C,
         tipo_setor = C007D, horas_trabalho = C008, recebia = C010, quanto_recebia = C01011,
         valor_recebia = C01012, home_office = C013, INSS = C014, aposentado = D0011,
         quanto_aposentado = D0013, bolsa_familia = D0031, quanto_bf = D0033, 
         auxilio_emergencial = D0051, quanto_ae = D0053)

pnad9 <- pnad09 %>% 
  select(Ano, UF, V1013, Estrato, UPA, V1022, V1023, A002, A003, A004, A005, B0011, B0012, 
         B0013, B0014, B0015, B0016, B0017, B0018, B0019, B00110, B00111, B00112, 
         C007B, C007C, C007D, C008, C010, C01011, C01012, C013, C014, D0011, D0013,
         D0031, D0033, D0051, D0053) %>%
  rename(ano = Ano, mes = V1013, ua = V1022, RM = V1023, idade = A002, sexo = A003, raca = A004, 
         escolaridade = A005, febre = B0011, tosse = B0012, dorgarganta = B0013,
         dificu_respiratoria = B0014, dorcabeca = B0015, dorpeito = B0016, nausea = B0017,
         narizentupido = B0018, fadiga = B0019, dorolhos = B00110,
         perdeucheiro = B00111, dormuscular = B00112, carteira = C007B, tipo_trabalho = C007C,
         tipo_setor = C007D, horas_trabalho = C008, recebia = C010, quanto_recebia = C01011,
         valor_recebia = C01012, home_office = C013, INSS = C014, aposentado = D0011,
         quanto_aposentado = D0013, bolsa_familia = D0031, quanto_bf = D0033, 
         auxilio_emergencial = D0051, quanto_ae = D0053)

pnad <- pnad5 %>% 
  rbind(pnad6) %>% 
  rbind(pnad7) %>% 
  rbind(pnad8) %>% 
  rbind(pnad9)

##########################################################################
### Organizando a base principal                                       ###
##########################################################################

oaxaca <- pnad %>% 
  mutate(febre = ifelse(febre == 1, 1, ifelse(febre == 2, 0, NA)),
         tosse = ifelse(tosse == 1, 1, ifelse(tosse == 2, 0, NA)),
         dorgarganta = ifelse(dorgarganta == 1, 1, ifelse(dorgarganta == 2, 0, NA)),
         dificu_respiratoria = ifelse(dificu_respiratoria == 1, 1, ifelse(dificu_respiratoria == 2, 0, NA)),
         dorcabeca = ifelse(dorcabeca == 1, 1, ifelse(dorcabeca == 2, 0, NA)),
         dorpeito = ifelse(dorpeito == 1, 1, ifelse(dorpeito == 2, 0, NA)),
         nausea = ifelse(nausea == 1, 1, ifelse(nausea == 2, 0, NA)),
         narizentupido = ifelse(narizentupido == 1, 1, ifelse(narizentupido == 2, 0, NA)),
         fadiga = ifelse(fadiga == 1, 1, ifelse(fadiga == 2, 0, NA)),
         dorolhos = ifelse(dorolhos == 1, 1, ifelse(dorolhos == 2, 0, NA)),
         perdeucheiro = ifelse(perdeucheiro == 1, 1, ifelse(perdeucheiro == 2, 0, NA)),
         dormuscular = ifelse(dormuscular == 1, 1, ifelse(dormuscular == 2, 0, NA))) %>% 
  mutate(soma = febre + tosse + dorgarganta + dificu_respiratoria + dorcabeca + dorpeito + 
           nausea + narizentupido + fadiga + dorolhos + perdeucheiro + dormuscular,
         suspeita = ifelse(soma >= 4, 1, 0),
         formal = ifelse(carteira == c(1,2), TRUE, FALSE),
         RM = as.factor(RM))

oaxaca_stata <- oaxaca %>% 
  filter(!is.na(formal)) %>% 
  filter(!is.na(suspeita))

##########################################################################
### Construindo base CNES                                              ###
##########################################################################

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

##########################################################################
### Salvando as bases                                                  ###
##########################################################################

write.xlsx(oaxaca_stata, "oaxaca.xlsx")

write.xlsx(CNES, "CNES.xlsx")

##########################################################################
### Realizando as estimacoes por Oaxaca Blinder                        ###
##########################################################################

oaxaca::oaxaca(suspeita ~ idade + UF + capital + V1023 + ua + formal| formal , data = oaxaca)
