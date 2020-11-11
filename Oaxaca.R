#### Projeto Insper Data - Duque - Oaxaca Blinder

rm(list = ls())

# Selecione o dirtorio a ser utilizado

setwd("C:\Users\arthu\Desktop\InsperData\COVID\Oaxaca")

# Pacotes utilizados

library(dplyr)
library(tidyverse)
library(microdatasus)
library(skimr)
library(readxl)
library(lubridate)
library(broom)
library(openxlsx)

##########################################################################
### Coletando dados da PNAD COVID
##########################################################################

pnad05 <- read.csv("PNAD_COVID_052020.csv")

pnad06 <- read.csv("PNAD_COVID_062020.csv")

pnad07 <- read.csv("PNAD_COVID_072020.csv")

pnad08 <- read.csv("PNAD_COVID_082020.csv")

pnad09 <- read.csv("PNAD_COVID_092020.csv")

##########################################################################
### Coletando dados do CNES
##########################################################################

cnes <- read_excel("CNES_Final.xlsx") 

##########################################################################
### Filtrando as bases
##########################################################################

cnes <- cnes %>%
  mutate(across(names(cnes)[3:19], as.double)) %>%
  mutate_all(funs(replace_na(., 0))) %>% 
  separate(Municipio, into = c("mun_cod", "mun_nome"), sep = 6) %>% 
  separate(mun_nome, into = c("mun_nome", "cod"), sep = "-") %>% 
  filter(is.na(cod)) %>% 
  select(-cod)

pnad5 <- pnad05 %>%
  select(Ano, UF, CAPITAL, V1013, Estrato, UPA, V1022, V1030, V1031, V1032, A001A, 
         A002, A003, A004, A005, B0011, B0012, B0013, B0014, B0015, B0016, 
         B0017, B0018, B0019, B00110, B00111, B00112, B002, B0031, B0032, B0033,
         B0034, B0035, B0036, B0037, B0041, B0042, B0043, B0044, B0045, B0046, 
         B005, B006, B007, C001, C002, C003, C004, C005, C0051, C0052, C0053, 
         C006, C007, C007A, C007B, C007C, C007D, C007E, C007E1, C007E2, C008, 
         C009, C010, C0101, C01011, C01012, C0102, C01021, C01022, C0103, C0104,
         C011A1, C011A11, C011A12, C011A12, C011A2, C011A21, C011A22, C012, C013,
         C014, C015, C016, C017A, D0011, D0013, D0021, D0023, D0031, D0033, D0041,
         D0043, D0051, D0053, D0061, D0063, D0071, D0073, F001, F0021, F0022, 
         F0061, F006) %>% 
  rename(capital = CAPITAL, mes = V1013, ua = V1022, proj_pop = V1030, condicao_dom = A001A,
         idade = A002, sexo = A003, raca = A004, escolaridade = A005, febre = B0011,
         tosse = B0012, dorgarganta = B0013, dificu_respiratoria = B0014, 
         dorcabeca = B0015, dorpeito = B0016, nausea = B0017,
         narizentupido = B0018, fadiga = B0019, dorolhos = B00110,
         perdeucheiro = B00111, dormuscular = B00112, estab_saude = B002,
         ficou_casa = B0031, ligou_prof = B0032, tomou_remed_soz = B0033, tomou_remed_med = B0034,
         receb_prof_SUS = B0035, receb_prof_priv = B0036, outra_providencia = B0037, 
         postosaude_atend = B0041, PS_SUS_UPA_atend = B0042, hosp_SUS_atend = B0043, 
         ambul_priv_atend = B0044, priv_atend = B0046, internado = B005, inter_sedado = B006, 
         planosaude_priv = B007, trabalhou = C001, afastado = C002, motivo_afast = C003, 
         cont_remunerado = C004, tempo_afastado = C005, tempo_afastado_2 = C0051, 
         tempo_afastado_3 = C0052, tempo_afastado_4 = C0053, mais_trabalho = C006,
         )

pnad6 <- pnad06 %>%
  select(Ano, UF, CAPITAL, V1013, Estrato, UPA, V1022, V1030, V1031, V1032, A001A, 
         A002, A003, A004, A005, B0011, B0012, B0013, B0014, B0015, B0016, 
         B0017, B0018, B0019, B00110, B00111, B00112, B002, B0031, B0032, B0033,
         B0034, B0035, B0036, B0037, B0041, B0042, B0043, B0044, B0045, B0046, 
         B005, B006, B007, C001, C002, C003, C004, C005, C0051, C0052, C0053, 
         C006, C007, C007A, C007B, C007C, C007D, C007E, C007E1, C007E2, C008, 
         C009, C010, C0101, C01011, C01012, C0102, C01021, C01022, C0103, C0104,
         C011A1, C011A11, C011A12, C011A12, C011A2, C011A21, C011A22, C012, C013,
         C014, C015, C016, C017A, D0011, D0013, D0021, D0023, D0031, D0033, D0041,
         D0043, D0051, D0053, D0061, D0063, D0071, D0073, F001, F0021, F0022, 
         F0061, F006)





