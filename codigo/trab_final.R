###############################################################################
###############################################################################
######## SCRIPT PARA ANALISAR OS DISCURSOS DA CPI DA COVID             ########
########                                                               ########  
######## 1 - Importação e limpeza das bases                            ########
########                                                               ########
########   1.1 - Limpando as strings e tratando base dos discursos     ########
########       e dos candidatos                                        ########        
########   1.2 -                         ########
########   1.3 -   ########
########   1.4 -                  ########
########   1.5 -                               ########
########                                                               ######## 
######## 2- Gráficos                                         ######## 
########   2.1 -    ########
########   2.2 -          ########
########   2.3 -                                                ######## 
########                                                               ######## 
###############################################################################


setwd("C://Users//duque//Desktop//PUC//Mestrado//Verão 1//Estatística//Data Science//Trabalho Final//Input")

pacman::p_load(sf, tidyverse, geobr, ggplot2, terra, spData, stringr)

##### 1. Importando e limpando as bases #####

## Importando as bases

discurso <- read.csv("discursos.csv", encoding="UTF-8") # encoding veio esquisito

candidatos <- read.csv("candidatos.csv")

casos <- read.csv("caso.csv")

##### 1.1 Limpando as strings e tratando base dos discursos e dos candidatos #####

## Criando funcao para padronizar strings (texto maisculo e sem acento)

arrumar <- function(vec){
  vec <- toupper(vec)
  vec <- gsub("Á","A",vec)
  vec <- gsub("Ã","A",vec)
  vec <- gsub("À","A",vec)
  vec <- gsub("Â","A",vec)
  vec <- gsub("É","E",vec)
  vec <- gsub("È","E",vec)
  vec <- gsub("Ê","E",vec)
  vec <- gsub("Í","I",vec)
  vec <- gsub("Ì","I",vec)
  vec <- gsub("Ó","O",vec)
  vec <- gsub("Õ","O",vec)
  vec <- gsub("Ò","O",vec)
  vec <- gsub("Ô","O",vec)
  vec <- gsub("Ú","U",vec)
  vec <- gsub("Ù","U",vec)
  vec <- gsub("Ç","C",vec)
  return(vec)
}

# usando a funcao nas colunas relevantes 

discurso <- discurso %>%
  mutate(texto_discurso = arrumar(texto_discurso),
         nome_discursante = arrumar(nome_discursante),
         categoria_discursante = ifelse(categoria_discursante == "Senador/a","senador",categoria_discursante))

candidatos <- candidatos %>%
  mutate(nome = arrumar(nome),
         nome_urna = arrumar(nome_urna))

## Filtrando a base de candidatos pelos deputados que participaram da cpi

# desconsiderando depoentes 

deputados <- discurso %>%
  filter(categoria_discursante == "senador")

# pegando o vetor de nomes 

deputados <- unique(deputados$nome_discursante)

# pegando as informacoes dos discursantes para os mandatos validos hoje

candidato_filt <- candidatos %>%
  filter(nome_urna %in% deputados,
         cargo %in% c("senador","1Âº suplente senador","2Âº suplente senador"),
         ano >= 2014) %>% 
  select(nome_urna,ano,sigla_uf,sigla_partido,numero_partido,ocupacao,idade,instrucao,estado_civil,raca)

# selecionando variaveis e dando join

discurso_test <- discurso %>%
  select(sequencial_sessao:sinalizacao_fora_microfone) %>%
  left_join(candidato_filt, by = c("nome_discursante"="nome_urna"))

##### 1.2 - Estatísticas Descritivas dos Participantes #####

##### 1.3 - Dados Covid #####