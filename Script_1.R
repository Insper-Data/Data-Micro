#Projeto Insper Data - Duque

#Aprendendo a usar a base dataSUS

#Há três bases de dados disponíveis:
#Entradas no SUS (SIH RD), óbitos (SIM) 
#e nascimentos (SINASC)

library(dplyr)
library(tidyverse)

#teste para entradas no SUS
#teste com o Rio de Janeiro
install.packages("devtools")
devtools::install_github("rfsaldanha/microdatasus")
library(microdatasus)

dados <- fetch_datasus(year_start = 2018, month_start = 3, year_end = 2018, month_end = 4, uf = "RJ", information_system = "SIH-RD")
dados <- process_sih(dados)

#agrupando as mortes no RJ por município e por mês

dados %>% 
  filter(MORTE=="Sim") %>% 
  group_by(munResNome,MES_CMPT) %>% 
  summarise(total=n()) %>% 
  arrange(desc(total))

####

#Base de dados de óbitos
install.packages("devtools")
devtools::install_github("rfsaldanha/microdatasus")
library(microdatasus)
dados_2018 <- fetch_datasus(year_start = 2018, year_end = 2018, uf = "RJ", information_system = "SIM-DO")
dados_2018 <- process_sim(dados_2018)

library(dplyr)
library(tidyverse)

dados_2018 %>% 
  select(munResNome,CAUSABAS,DTOBITO)

#Seria preciso separar as datas por dia, mês e ano

dados_2018 %>% 
  group_by(munResNome) %>% 
  summarise(total=n()) %>% 
  arrange(desc(total))

#mudança


